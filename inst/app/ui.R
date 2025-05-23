shinyUI(
  tabsetPanel(
  tabPanel("Baseline vs Design",
  shinyjs::useShinyjs(),
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
          numericInput("input_q", "Discharge (cfs) to plot", NA, min = 0),
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
          tags$div(
          actionButton("dt_qs_addrow","+"),
          actionButton("dt_qs_delrow","−")
          ),
          p(HTML("Enter flow frequency result based on one of the following sources: (a) regional regression at site location via <a href=https://streamstats.usgs.gov/ss/ target=_blank>StreamStats</a>; (b) gaged peak flow estimates calculated via <a href=https://doi.org/10.3133/tm4B5 target=_blank>Bulletin 17C</a> methods such as via <a href=https://www.hec.usace.army.mil/software/hec-ssp/ target=_blank>HEC-SSP</a> or as published on a StreamStats Gage Page; or (c) a weighted estimate of the two as documented in <a href=http://pubs.usgs.gov/sir/2012/5113 target=_blank>USGS SIR 2012-5113</a> (<em>Estimation for an Ungaged Site Near a Streamgage</em>).")),
          br(),
      ),
      column(7,
        DTOutput("eval_result1"),
        DTOutput("eval_result2"),
        br(),
      )
    )
    )
  ),
  tabPanel("Single Cross Section",
   fluidPage(
     fluidRow(
       column(3, h2("Input Cross Section"),
              fileInput("file_xs_hwm", "Choose (Station,Elevation) CSV File",
                        multiple = FALSE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              DTOutput("dt_xs_hwm"),
       ),
              column(3, h2("Parameters"),
                     radioButtons("hwm_mode", label = "Calculation Mode", selected = "default",
                                  choices = c("Hydraulic Geometry at WSE" = "default",
                                              "Manning's Q (known WSE, n, S)" = "q",
                                              "Manning's n (known Q, WSE, S)" = "n",
                                              "Manning's S (known Q, WSE, n)" = "s",
                                              "Estimate WSE (known Q, S, n)" = "wse")),
                     numericInput("wse_hwm", "Water Surface Elev. (ft)", NA),
                     numericInput("discharge_hwm", "Q = Streamflow (cfs)", NA, min = 0, step=1),
                     numericInput("mannings_hwm", "n = Roughness", NA, min = 0.001, step=0.001),
                     numericInput("slope_hwm", "S = Slope (ft/ft)", NA, min = 0.001, step=0.001),
              DTOutput("dt_result_hwm"),),
       column(4, 
              plotOutput("plot_xs_hwm"),
              plotOutput("plot_rc_hwm"),
              )
       ),
   )
  ),
  )
  )
