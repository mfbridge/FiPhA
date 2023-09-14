# intervals-layout.R

intervals.ui = nav("Intervals",
    fluidRow(
       column(6,
              pickerInput("intervals_variables", NULL, multiple = T, width = "100%", choices = list("hello "=c("a b", "c d"), `One.xlsx`=c("X", "Y", "Z"), `Two.csv`=c("AB", "CD"))
              )
       )
    )
)