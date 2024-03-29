# import tdt

.tdt.temp = reactiveValues()

shinyDirChoose(input, "data_import_tdt_dir", root=c(directories, `Working Directory`='.', getVolumes()()), filetypes=c("tsq","tev"))

observeEvent(input$data_tdt, {
    .tdt.temp$headers = NULL
    output$data_import_tdt_path = renderText("select a folder that contains both a .tsq & .tev file")
    showModal(
        modalDialog(title = "TDT Fiber Photometry Gizmo",
            fluidRow(
                column(2, shinyDirButton("data_import_tdt_dir", "Directory", "")),
                column(10, verbatimTextOutput("data_import_tdt_path"))
            ),
            fluidRow(
                column(4, pickerInput("data_import_tdt_streams", "Available Streams", choices = c(), multiple = T))
            ),
            size = "l",
            footer = tagList(modalButton("Cancel"), actionButton("data_import_tdt_action", "Import")))
    )
})

observeEvent(input$data_import_tdt_dir, {
    if (is.integer(input$data_import_tdt_dir)) {

    } else {
        di = parseDirPath(roots=c(directories, `Working Directory`='.', getVolumes()()), selection = input$data_import_tdt_dir)
        output$data_import_tdt_path = renderText(file.path(di))
        tsq = parse_tsq(file.path(di, dir(di, "*.tsq")))
        only.ch = tsq[!(code %in% c(1, 2)), .(code_c, nch = length(unique(channel)), freq = unique(frequency)), by = .(code_c)][nch == 1 & freq > 0,]
        streams.pc = tsq[str_starts(code_c, "P"), unique(str_sub(code_c, 1, 3))]

        .tdt.temp$headers = tsq

        updatePickerInput(session, "data_import_tdt_streams",
            choices = c(only.ch$code_c, streams.pc),
            selected = c(only.ch$code_c, streams.pc),
            choicesOpt = list(subtext = c(paste(only.ch$freq, "Hz"), rep("\u238d", length(streams.pc)))))
    }
})

observeEvent(input$data_import_tdt_action, {
    if (is.integer(input$data_import_tdt_dir)) {

    } else {
        req(.tdt.temp$headers)

        di = parseDirPath(roots=c(directories, `Working Directory`='.', getVolumes()()), selection = input$data_import_tdt_dir)
        output$data_import_tdt_path = renderText(file.path(di))
        tev = read.tdt(file.path(di), streams = input$data_import_tdt_streams, .cached.headers = .tdt.temp$headers)
        .str = file.path(di, dir(di, "*.tsq"))

        data$raw[[.str]] = tev

        data$raw[[.str]][, `(time)` := `timestamp` - min(`timestamp`)]

        data$analysis[[.str]] = list()

        # create a new metadata entry named as the imported filename
        data$meta = append(data$meta,
                           setNames(list(list(file = .str,
                                              path = normalizePath(di),
                                              time = "(time)")),
                                    .str))

        removeModal()
        updatePickerInput(session, "data_dataset", choices = names(data$meta))
        updatePickerInput(session, "heatmap_dataset", choices = names(data$meta))
        updatePickerInput(session, "heatmap2_dataset", choices = names(data$meta))
        updatePickerInput(session, "events_dataset", choices = names(data$meta))
        updatePickerInput(session, "power_dataset", choices = names(data$meta))
        updatePickerInput(session, "lag_dataset", choices = names(data$meta))
        updatePickerInput(session, "summary_dataset", choices = names(data$meta))
    }
})

tsq.events = list(
    unknown =   0x00000000,
    str.on =    0x00000101,
    str.off =   0x00000102,
    scalar =    0x00000201,
    stream =    0x00008101,
    snip =      0x00008201,
    mark =      0x00008801,
    has.data =  0x00008000,
    ucf =       0x00000010,
    phantom =   0x00000020,
    mask =      0x0000ff0f,
    invalid =   0xffff0000
)

tsq.blocks = list(
    start =     0x00000001,
    stop =      0x00000002
)

tsq.formats = list(
    float = 0,
    long = 1,
    short = 2,
    byte = 3,
    double = 4,
    qword = 5
)

parse_tsq = function(path) {
    assert_file_exists(path, extension = "tsq")

    tsq = file(path, "rb")
    file_size = file.size(path)

    xxx = readBin(tsq, integer64(), 1) # ?

    seek(tsq, 48)
    code1 = readBin(tsq, integer(), 1)
    printf("TSQ file info:\n  start_code %s\n", ifelse(code1 == tsq.blocks$start, col_green("OK"), col_red("not ok")))

    seek(tsq, file_size - 32)
    code2 = readBin(tsq, integer(), 1)
    printf("  stop_code  %s\n", ifelse(code2 == tsq.blocks$stop, col_green("OK"), col_red("not ok")))

    seek(tsq, 56)
    start_time = as_datetime(readBin(tsq, double(), 1))
    printf("  start_time %s\n", col_yellow(start_time))

    seek(tsq, file_size - 24)
    stop_time = as_datetime(readBin(tsq, double(), 1))
    printf("  stop_time  %s\n", col_yellow(stop_time))

    duration = stop_time - start_time
    printf("  duration  %s %s\n", col_blue(sprintf("%0.6f", as.numeric(duration))), col_blue(units(duration)))

    seek(tsq, 40)

    tsq_ = readBin(tsq, raw(), file_size)

    seek(tsq, 40)

    n. = (file_size - 40)/40

    tsq.size = integer(n.)
    tsq.type = integer(n.)
    tsq.code = integer(n.)
    tsq.code_c = character(n.)
    tsq.channel = integer(n.)
    tsq.sortcode = integer(n.)
    tsq.timestamp = double(n.)
    tsq.offset = integer64(n.)
    tsq.format = integer(n.)
    tsq.frequency = double(n.)

    chunk.parsed = 0
    chunk.size = 100
    total.parsed = 0

    start.time = Sys.time()
    cli_progress_bar( total = n., format = "Reading block headers... {cli::pb_current}-{cli::pb_current+chunk.size-1} of {cli::pb_total} ({cli::pb_percent}) done, eta: {cli::pb_eta}", clear = F)

    while (total.parsed + 1 < n.) {
        buffer = readBin(tsq, raw(), 40 * chunk.size)

        for (i in 1:chunk.size) {
            if (total.parsed + 1 > n.) break
            suppressWarnings({
                .size = readBin(buffer[(i-1)*40+1:4], integer(), 1)
                .type = readBin(buffer[(i-1)*40+5:8], integer(), 1)
                .code = readBin(buffer[(i-1)*40+9:12], integer(), 1)
                .code_c = readBin(buffer[(i-1)*40+9:12], raw(), 4)
                .channel = readBin(buffer[(i-1)*40+13:14], integer(), 1, size = 2)
                .sortcode = readBin(buffer[(i-1)*40+15:16], integer(), 1, size = 2)
                .timestamp = readBin(buffer[(i-1)*40+17:24], double(), 1)
                .offset = as.integer64(readBin(buffer[(i-1)*40+25:32], integer(), 1, size = 8)) # this is probably not right for > 32-bit
                .format = readBin(buffer[(i-1)*40+33:36], integer(), 1)
                .frequency = readBin(buffer[(i-1)*40+37:40], double(), 1, size = 4)

                tsq.size[total.parsed + 1] = .size
                tsq.type[total.parsed + 1] = .type
                tsq.code[total.parsed + 1] = .code
                tsq.code_c[total.parsed + 1] = rawToChar(.code_c)
                tsq.channel[total.parsed + 1] = .channel
                tsq.sortcode[total.parsed + 1] = .sortcode
                tsq.timestamp[total.parsed + 1] = .timestamp
                tsq.offset[total.parsed + 1] = .offset
                tsq.format[total.parsed + 1] = .format
                tsq.frequency[total.parsed + 1] = .frequency

                # test reading data timing
                # if (!(.code %in% c(1, 2)) & .offset != 0) {
                #     em = read_tev_block(tev, .offset, .format, .size)
                # }
            })
            total.parsed = total.parsed + 1
        }
        chunk.parsed = chunk.parsed + 1
        #setTxtProgressBar(pb, chunk.parsed)
        cli_progress_update(set = chunk.parsed * chunk.size)
    }

    #close(pb)
    cli_progress_done()

    #close(tev)
    close(tsq)


    ev.list = data.table(
        size = tsq.size,
        type = tsq.type,
        code = tsq.code,
        code_c = tsq.code_c,
        channel = tsq.channel,
        sortcode = tsq.sortcode,
        timestamp = tsq.timestamp,
        offset = tsq.offset,
        format = tsq.format,
        frequency = tsq.frequency
    )

    #browser()

    ev.list
}

read.tdt = function(path, tsq.file = dir(path, "*.tsq"), tev.file = dir(path, "*.tev"), streams = c("405A", "465A", "560B"), .cached.headers = NULL, wide = T) {
    assert_file_exists(file.path(path, tsq.file))
    assert_file_exists(file.path(path, tev.file))

    if (!is.null(.cached.headers)) {
        tsq.headers = .cached.headers
    } else {
        tsq.headers = parse_tsq(file.path(path, tsq.file))
    }

    tev = file(file.path(path, tev.file), "rb")

    printf("Available data streams:\n")
    .hs = tsq.headers[!(code %in% c(1, 2)), .(nch = length(unique(channel)), freq = unique(frequency), s = sum(size) - 10 * .SD[, .N]), by = .(code_c)]
    for (i in .hs[, .I]) {
        .c = .hs[i, code_c]
        if (.hs[i, freq] == 0) {
            printf("  %s: %s channel%s\n", col_green(.c), col_blue(.hs[i, nch]), ifelse(.hs[i, nch] == 1,"","s"))
        } else {
            printf("  %s: %s channel%s at %s Hz (n=%s)\n", col_green(.c), col_blue(.hs[i, nch]), ifelse(.hs[i, nch] == 1,"","s"), col_red(.hs[i, freq]), col_red(.hs[i, s]))
        }
    }
    cat("\n")

    tev.blocks = tsq.headers[!(code %in% c(1, 2)) & ((code_c %in% streams) | (str_starts(code_c, "P"))),]

    # only worried about single channels for now like 405/465/560 or PC streams
    #assert_true(all(tev.blocks[, .(nch = length(unique(channel)) == 1) | str_starts(code_c, "PC"), by = .(code_c)]$nch))

    # there are actually 10 less observations (= 40 bytes of floats) than specified in a given header
    # TODO: unclear what these are for. padding? a checksum? unrelated data in the same buffer?
    n.obs = tev.blocks[code_c == streams[1], sum(size)]
    n.obs = n.obs - 10 * tev.blocks[code_c == streams[1], .N]

    block.i = data.table()
    block.data = data.table()

    .block.block = numeric(n.obs)
    .block.time = numeric(n.obs)

    .block.ls = list()

    streams.not.pc = streams[!str_starts(streams, "P")]
    streams.pc = tev.blocks[str_starts(code_c, "P"), ][, unique(str_sub(code_c, 1, 3))]

    lapply(streams, \(s) {
        block.i[, (s) := numeric(1)]
        block.i[1, (s) := 0]

        block.data[, (s) := numeric(n.obs)]

        .block.ls[[s]] = list()

        if (s %in% streams.not.pc) {
            for (c in unique(tev.blocks[code_c == s, channel])) {
                .block.ls[[s]][[c]] = numeric(n.obs)
            }
        } else {
            .block.ls[[s]][[1]] = numeric(n.obs)
        }
    })

    t.seen = 0

    cli_progress_bar(total = nrow(tev.blocks), format = "Reading data block {cli::pb_current} of {cli::pb_total}, eta: {cli::pb_eta}", clear = F)

    for (i in 1:nrow(tev.blocks)) {
        if (tev.blocks[i, code_c] %in% streams.not.pc) {
            .s = tev.blocks[i, code_c] #sprintf("%s-%s", tev.blocks[i, code_c], tev.blocks[i, channel])
            .c = tev.blocks[i, channel]
            .ts = tev.blocks[i, timestamp]
            .f = tev.blocks[i, frequency] # all the same
            .l = tev.blocks[i, size] - 10

            .block.ls[[.s]][[.c]][(1 + block.i[1, get(.s)]*.l):(block.i[1, get(.s)]*.l + .l)] =
                read.tev.block(file.path(path, tev.file), tev.blocks[i, offset], tev.blocks[i, format], .l + 10)

            block.i[1, (.s) := get(.s) + 1]

            t.seen = t.seen + .l + 10
        }
        cli_progress_update(set = i)
    }

    close(tev)

    block.data[, `:=`(timestamp = .block.time)]


    lapply(streams, \(s) {
        for (c in unique(tev.blocks[code_c == s, channel])) {
            if (s %in% streams.not.pc) {
                block.data[, (sprintf('%s', s)) := .block.ls[[s]][[c]]]
            }

            if (s %in% streams.pc) {
                block.data[, (sprintf('%s', s)) := .block.ls[[s]][[1]]]
            }
        }
    })

    # timestamps
    block.data[, timestamp := tev.blocks[, min(timestamp)] + (0:(.N-1)) * 1 / .f]


    # build PC events
    for (c in streams.pc) {
        tsi = tev.blocks[str_starts(code_c, c), ]

        el = data.table(start = numeric(), end = numeric())

        if (tsi[1, type] == 258) {
            el = rbindlist(list(el, data.table(start = tev.blocks[, min(timestamp)], end = tsi[1, timestamp])))
            tsi = tsi[2:nrow(tsi),] # interpret an initial falling edge ?
        }

        for (i in 1:nrow(tsi)) {
            if (i %% 2 == 0) {
                el = rbindlist(list(el, data.table(start = tsi[i - 1, timestamp], end = tsi[i, timestamp])))
            }
        }

        # could fit this in above
        for (n in 1:nrow(el)) {
            .st = el[n, start]
            .nd = el[n, end]
            block.data[timestamp >= .st & timestamp < .nd, (c) := 1]
        }
    }

    if (!wide) {
        block.data = melt.data.table(block.data, measure.vars = streams, value.name = "value", variable.name = "stream")
    }

    #browser()

    block.data
}

read.tev.block = function(f, offset, format, n) {
    if (is.character(f)) fp = file(f, "rb")
    else fp = f

    seek(fp, offset)

    if (format == tsq.formats$float) {
        ret = readBin(fp, double(), n - 10, size = 4)
    }

    if (is.character(f)) close(fp)

    ret
}
