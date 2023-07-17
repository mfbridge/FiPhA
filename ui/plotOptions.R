# plotOptions.R
# shiny module for generating a bunch of the same UI elements to be displayed in a dropdown()


plotOptions = function(id, label = NULL, options = list(),
                       label.css = "text-align: center; font-weight: bold; font-size: 0.8rem; width: 100%",
                       title.css = "text-align: center; margin-top: 0;font-size: 0.8rem;",
                       row.css = "min-height: 0px; line-height: 2rem; height: 2rem; font-size: 0.75rem;",
                       hr.css = "margin-top: 0.4rem; margin-bottom: 0.4rem;") {
    ns = NS(id)

    .tags = div(style="width: 350px;")

    for (e in 1:length(options)) {
        if (options[[e]]$type == "title") {
            .tags = tagAppendChild(.tags, tags$h6(options[[e]]$text, style=title.css))

        }  else if (options[[e]]$type == "hr") {
            .tags = tagAppendChild(.tags, tags$hr(style=hr.css))

        } else if (options[[e]]$type == "theme") {
            .tags = tagAppendChild(.tags,
                fluidRow(style=row.css,
                    column(4, options[[e]]$text),
                    column(8, pickerInput(options[[e]]$id, NULL, options = list(style = c("btn-light", "btn-sm", "bs-placeholder")), choices = c("prism", "minimal", "classic", "dark", "light", "linedraw", "bw", "gray"), selected = "prism"))))

        } else if (options[[e]]$type == "font-size+family") {
            .tags = tagAppendChild(.tags,
                fluidRow(style=row.css,
                    column(4, options[[e]]$text),
                    column(4, numericInput(options[[e]]$size.id, NULL, options[[e]]$size.value, min = 1, max = Inf, step = 0.5)),
                    column(4, pickerInput(options[[e]]$family.id, NULL, options = list(style = c("btn-light", "btn-sm", "bs-placeholder")), choices = default$fonts, selected = options[[e]]$family.value))
                )
            )

        } else if (options[[e]]$type == "line-size") {
            .tags = tagAppendChild(.tags,
                fluidRow(style=row.css,
                    column(8, options[[e]]$text),
                    column(4, numericInput(options[[e]]$id, NULL, options[[e]]$value, min = 0, max = Inf, step = 0.01))))

        } else if (options[[e]]$type == "line-color") {
            .tags = tagAppendChild(.tags,
                fluidRow(style=row.css,
                    column(8, options[[e]]$text),
                    column(4, colorPickr(options[[e]]$id, NULL, selected = options[[e]]$value, opacity = T))))


        } else if (options[[e]]$type == "line-size+color") {
            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css, column(4, options[[e]]$text),
                    column(4, numericInput(options[[e]]$size.id, NULL, options[[e]]$size.value, min = 0, max = Inf, step = 0.01)),
                    column(4, colorPickr(options[[e]]$color.id, NULL, selected = options[[e]]$color.value, opacity = T))))

        } else if (options[[e]]$type == "line-size+alpha") {

            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css, column(4, options[[e]]$text),
                    column(4, numericInput(options[[e]]$size.id, NULL, options[[e]]$size.value, min = 0, max = Inf, step = 0.01)),
                    column(4, numericInput(options[[e]]$alpha.id, NULL, options[[e]]$alpha.value, min = 0, max = 1, step = 0.1))))

        } else if (options[[e]]$type == "checkbox") {
            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css,
                    column(12, prettyCheckbox(options[[e]]$id, options[[e]]$text, value = options[[e]]$checked))
               )
            )
        } else if (options[[e]]$type == "text") {

            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css,
                        column(4, options[[e]]$text),
                        column(8, textInput(options[[e]]$id, NULL, options[[e]]$value))
               )
            )
        } else if (options[[e]]$type == "mean+sd") {

            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css,
                        column(8, prettyCheckbox(options[[e]]$show.id, "\u03bc \u00b1 N SD")),
                        column(4, numericInput(options[[e]]$n.id, NULL, options[[e]]$n.value, min = 0, max = Inf, step = 0.01))
               )
            )

        } else if (options[[e]]$type == "mean+se") {

            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css,
                        column(4, prettyCheckbox(options[[e]]$show.id, "\u03bc \u00b1")),
                        column(4, numericInput(options[[e]]$n.id, NULL, options[[e]]$n.value, min = 0, max = Inf, step = 0.01)),
                        column(4, pickerInput(options[[e]]$n.f, NULL, choices = list("SD", "SE"), selected = "SE"))
               )
            )
        } else if (options[[e]]$type == "checkbox+text") {

            .tags = tagAppendChild(.tags,
               fluidRow(style=row.css,
                        column(6, prettyCheckbox(options[[e]]$checkbox.id, options[[e]]$text, options[[e]]$checked)),
                        column(6, textInput(options[[e]]$text.id, NULL, options[[e]]$value))
               )
            )
        } else if (options[[e]]$type == "palette") {
            .tags = tagAppendChildren(.tags,
                fluidRow(style = row.css,
                    column(4, options[[e]]$text),
                    column(8, pickerInput(options[[e]]$palette.id, NULL, options = list(style = c("btn-light", "btn-sm", "bs-placeholder")),
                        choices = list("default", "gradient2", "viridis package"=list("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo"))))
                ),
                fluidRow(style = row.css,
                    column(4, "Gradient"),
                    column(4, colorPickr(options[[e]]$from.id, NULL, selected = options[[e]]$from.color, opacity = T)),
                    column(4, colorPickr(options[[e]]$to.id, NULL, selected = options[[e]]$to.color, opacity = T))
                )
            )
        }

    }

    .tags

}

getPaletteFill = function(palette.id, g2.low, g2.high) {
    if (palette.id == "gradient2") {
        return(scale_fill_gradient2(low = g2.low, high = g2.high))
    } else if (palette.id %in% c("viridis", "magma", "plasma", "inferno", "cividis", "mako", "rocket", "turbo")) {
        return(scale_fill_viridis_d(option = palette.id, begin = 0.1, end = 0.9))
    } else {
        #default
    }
}

# handle the internal functions
plotOptionsServer = function(id) {
    moduleServer(id, function(input, output, session) {

    })
}


