getFixedEventTimes = function() {
    tbl = setNames(as.data.table(excel_to_R(input$events_fixed_list)), c("start", "end"))
    tbl$start = as.numeric(tbl$start)
    tbl$end = as.numeric(tbl$end)
    tbl
}

output$events_fixed_list = renderExcel({
    excelTable(default$fixed_list,
        allowInsertColumn = F,
        allowDeleteColumn = F,
        allowRenameColumn = F,
        rowDrag = T,
        columns = data.frame(
            title = c("Start (sec)", "End (sec)"),
            type = c("number", "number"),
            width = c(200, 200)
        )
    )
})


identify.fixed.events = function(dt, t, event.table = NULL, type = "") {
    mindt = dt[2, get(t)] - dt[1, get(t)]
    event.table[, length := max(end - start, mindt)] # minimum length of one time step at each event
    event.table[, end := start + length] # recalculate `end` as it may have been equal to `start` if the event was a single frame
    event.table[, type := type] # passthrough

    return(event.table) # :)
}
