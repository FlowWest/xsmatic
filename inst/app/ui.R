shinyUI(
  fluidPage(
    fluidRow(
        column(2, "Cross Section 1",
          numericInput("mannings1", "Manning's n", 0.025, min = 0, max = 1),
          numericInput("slope1", "Slope (ft/ft)", 0.01, min = 0, max = 1),
          fileInput("file_xs1", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          DTOutput("dt_xs1"),
          #actionButton("go_xs1",label = "Update"),
        ),
        column(2, "Cross Section 2",
          numericInput("mannings2", "Manning's n", 0.025, min = 0, max = 1),
          numericInput("slope2", "Slope (ft/ft)", 0.01, min = 0, max = 1),
          fileInput("file_xs2", "Choose CSV File",
                    multiple = FALSE,
                    accept = c("text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
          DTOutput("dt_xs2"),
          #actionButton("go_xs2",label = "Update"),
        ),
        column(2, "Discharges",
          numericInput("input_q", "Plot discharge (cfs)", 1000, min = 0),
          DTOutput("dt_qs"),
          #actionButton("go_qs",label = "Update")
        ),
        column(4, "Results",
          plotOutput("plot_xs"),
          plotOutput("plot_rc"),
          DTOutput("eval_result")
        )
      )
    )
  )
