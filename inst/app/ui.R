shinyUI(
  fluidPage(
    fluidRow(
        column(3, h2("Cross Section 1 (Baseline)"),
          checkboxInput("enable1", "Enable Cross Section 1", FALSE),
          numericInput("mannings1", "Manning's n", 0.025, min = 0.001, step=0.001),
          numericInput("slope1", "Slope (ft/ft)", 0.010, min = 0.001, step=0.001),
          hr(),
          fileInput("file_xs1", "Choose (Station,Elevation) CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          DTOutput("dt_xs1"),
        ),
        column(3, h2("Cross Section 2 (Design)"),
          checkboxInput("enable2", "Enable Cross Section 2", FALSE),
          numericInput("mannings2", "Manning's n", 0.025, min = 0.001, step=0.001),
          numericInput("slope2", "Slope (ft/ft)", 0.010, min = 0.001, step=0.001),
          hr(),
          fileInput("file_xs2", "Choose (Station,Elevation) CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          DTOutput("dt_xs2"),
        ),
        column(4, h2("Plot Result"),
          numericInput("input_q", "Discharge (cfs) to plot", 1000, min = 1),
          plotOutput("plot_xs"),
          plotOutput("plot_rc"),
        )
      ),
    fluidRow(
      column(10, 
             hr(),
             h2("Tabular Result")
      )),
    fluidRow(
      column(3, 
          DTOutput("dt_qs"),
          br(),
      ),
      column(7,
        DTOutput("eval_result1"),
        DTOutput("eval_result2"),
        br(),
      )
    )
    )
  )
