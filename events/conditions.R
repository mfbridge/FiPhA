testConditionValid = function() {
    t = T
     tryCatch({
        .test = data$raw[[input$events_dataset]][, eval(parse(text=input$events_conditions))]
    }, error = function(e) {
        t = F
    }, warning = function(e) {
        t = F
    })
    t
}

condition.events = function(.dt, .el, .t, c, if.all = F) {
    ret = list()

    # View(list(.dt, .el, .t, c))
    #View(.el)

    #print(1)
    #print(match.call())
    #print(.t)
    #print(names(.dt))


    for (f in names(.dt)) {
        dt = .dt[[f]]
        el = .el #.el[[f]]
        t = .t[[f]]

        keep = c()
        .eps = (2/3)*(dt[2, get(t)] - dt[1, get(t)])
        print(.eps)
        for (e in 1:nrow(el)) {
            condition = dt[get(t) - el[e, start] >= -.eps & get(t) - el[e, end] < .eps, eval(parse(text = c))]

            if (if.all) {
                met = all(condition)
            } else {
                met = any(condition)
            }

            keep[e] = met
        }

        ret[[f]] = el[keep]
    }

    #print(2)

    ret
}

observeEvent(input$events_conditions_test, {
    if (str_length(input$events_conditions) > 0) {
        showModal(
            modalDialog(title = "Condition Test", footer = modalButton("Close"),
                htmlOutput("events_conditions_test_info"))
        )
    }
})

output$events_conditions_test_info = renderUI({
    tryCatch({
        .test = data$raw[[input$events_dataset]][, eval(parse(text=input$events_conditions))]
        tags$pre(HTML(sprintf("Parsing of <i>%s</i> resulted in <b>%d</b> true and <b>%d</b> false values (total is <b>%d</b>).", input$events_conditions, sum(.test == T), sum(.test == F), length(.test))))
    }, error = function(e) {
        tags$pre(toString(e))
    }, warning = function(e) {
        tags$pre(toString(e))
    })
})
