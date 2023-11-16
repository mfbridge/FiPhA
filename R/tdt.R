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

#' Open and parse a *.tsq file's data block and event metadata
#'
#' @param path folder containing TDT data files
#'
#' @return data.table containing metadata for the associated .tev file
#' @export
read_tsq = function(path) {
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
    chunk.size = 1000
    total.parsed = 0

    start.time = Sys.time()
    cli_progress_bar( total = n., format = "{cli::style_bold('Reading metadata...')} {cli::pb_current}-{cli::pb_current+chunk.size-1} of {cli::pb_total} ({cli::pb_percent}) done, eta: {cli::pb_eta}", clear = F)

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
                .offset = as.integer64(readBin(buffer[(i-1)*40+25:32], integer(), 1, size = 8)) # is this right for 64-bit?
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
            })
            total.parsed = total.parsed + 1
        }
        chunk.parsed = chunk.parsed + 1
        cli_progress_update(set = chunk.parsed * chunk.size)
    }

    cli_progress_done()

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

    ev.list
}

#' Print information about the available data streams
#'
#' @param path folder containing TDT data files
#' @param tsq.headers metadata from read_tsq
#' @param tsq.file tsq file name
#'
#' @export
#'
tsq_info = function(tsq.headers) {
    printf("Available data streams:\n")
    .hs = as.data.table(tsq.headers)[!(code %in% c(1, 2)), list(nch = length(unique(channel)), freq = unique(frequency), s = sum(size) - 10 * .SD[, .N]), by = list(code_c)]

    for (i in .hs[, .I]) {
        .c = .hs[i, code_c]
        if (.hs[i, freq] == 0) {
            printf("  %s: %s event channel%s\n", col_green(.c), col_blue(.hs[i, nch]), ifelse(.hs[i, nch] == 1,"","s"))
        } else {
            printf("  %s: %s data channel%s at %s Hz (n=%s)\n", col_green(.c), col_blue(.hs[i, nch]), ifelse(.hs[i, nch] == 1,"","s"), col_red(.hs[i, freq]), col_red(.hs[i, s]))
        }
    }
    cat("\n")
}

#' Read raw values contained in tev file using tsq data
#' @description
#' TODO: fix for importing multiple frequencies at once
#'
#' @param path folder containing TDT data files (*.tsq, *.tev, etc.)
#' @param tsq.headers tsq header information from parse_tsq
#' @param tsq.file tsq file name
#' @param tev.file tev file name
#' @param streams which data streams to parse?
#' @param wide return data in a wide format rather than long
#'
#' @return data.table of raw values
#' @export
#'
read_tev = function(path, tsq.headers = NULL, streams = c("405A", "465A", "560B"), wide = T) {
    tev = file(path, "rb")

    tsq.headers = as.data.table(tsq.headers)

    #browser()

    tev.blocks = tsq.headers[!(code %in% c(1, 2)) & ((code_c %in% streams) | (str_starts(code_c, "P"))),]

    # there are 10 less observations (= 40 bytes of floats) than specified in a given header (padding?)
    n.obs = tev.blocks[code_c == streams[[1]], ]
    n.obs = n.obs - 10 * tev.blocks[tev.blocks$code_c == streams[[1]], .N]

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
            for (c in unique(tev.blocks[tev.blocks$code_c == s, tev.blocks$channel])) {
                .block.ls[[s]][[c]] = numeric(n.obs)
            }
        } else {
            .block.ls[[s]][[1]] = numeric(n.obs)
        }
    })

    t.seen = 0

    cli_progress_bar(total = nrow(tev.blocks), format = "{cli::style_bold('Reading data blocks'), {cli::pb_current} of {cli::pb_total}, eta: {cli::pb_eta}", clear = F)

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

    for (i in 1:nrow(tev.blocks)) {
        if (tev.blocks[i, code_c] %in% streams.not.pc) {
            .s = tev.blocks[i, code_c]
            .c = tev.blocks[i, channel]
            .ts = tev.blocks[i, timestamp]
            .f = tev.blocks[i, frequency] # TODO: assuming all the same frequency
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

    block.data
}
