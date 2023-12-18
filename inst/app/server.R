function(input, output, session) {
  
  df_xs1 <- reactive({
    req(input$file_xs1)  
    updateCheckboxInput(session, "enable1", value = TRUE)
    read_csv(input$file_xs1$datapath) %>% select(c(1,2)) %>% rename(station_ft = 1, elevation_ft = 2)
    })
  
  df_xs2 <- reactive({
    req(input$file_xs2)
    updateCheckboxInput(session, "enable2", value = TRUE)
    read_csv(input$file_xs2$datapath) %>% select(c(1,2)) %>% rename(station_ft = 1, elevation_ft = 2)
    })

  df_qs <- reactiveValues(discharges = { 
    data.frame(name = character(0), discharge = numeric(0)) %>% 
      add_row(name = c("Q2","Q5","Q10","Q25","Q50","Q100"), discharge = c(50,100,200,500,1000,2000))
  })

  output$dt_xs1 <- renderDT({
    DT::datatable(df_xs1(), 
                  editable = FALSE, 
                  colnames=c("Station (ft)", "Elevation (ft)"), 
                  options = list(dom = 'tp'), 
                  caption="Cross Section 1 Input")
  })
  
  output$dt_xs2 <- renderDT({
    DT::datatable(df_xs2(), 
                  editable = FALSE, 
                  colnames=c("Station (ft)", "Elevation (ft)"), 
                  options = list(dom = 'tp'), 
                  caption="Cross Section 2 Input")
  })
  
  output$dt_qs <- renderDT({
    DT::datatable(df_qs$discharges, 
                  editable = TRUE, 
                  colnames=c("Profile Name", "Discharge (cfs)"), 
                  options = list(dom = 't'), 
                  caption="Enter Discharges for Tabular Output")
  })
  
  observeEvent(input$dt_qs_cell_edit, {
    info = input$dt_qs_cell_edit
    i = as.numeric(info$row)
    j = as.numeric(info$col)
    k = as.numeric(info$value)
    df_qs$discharges[i,j] <- k
  })
  
  observeEvent(input$dt_qs_addrow, {
    df_qs$discharges <- df_qs$discharges %>% add_row(name = "Q_", discharge = 0)
  })
  
  observeEvent(input$dt_qs_delrow, {
    df_qs$discharges <- df_qs$discharges %>% filter(row_number() <= n()-1)
  })
  
  xs1 <- reactive({df_xs1() %>% xs_prep(data = ., sta = !!as.name(colnames(.)[1]), elev = !!as.name(colnames(.)[2]))})
  xs2 <- reactive({df_xs2() %>% xs_prep(data = ., sta = !!as.name(colnames(.)[1]), elev = !!as.name(colnames(.)[2]))})
  rc1 <- reactive({xs_rating_curve(xs = xs1(), input$slope1, input$mannings1)})
  rc2 <- reactive({xs_rating_curve(xs = xs2(), input$slope2, input$mannings2)})
  wse1 <- reactive({xs_rc_interpolate(rc = rc1(), discharge = input$input_q)})
  wse2 <- reactive({xs_rc_interpolate(rc = rc2(), discharge = input$input_q)})
  res1 <- reactive({xs_eval_all(xs = xs1(), rc = rc1(), 
                                discharges = df_qs$discharges 
                                #sediment_transport = input$toggle_sed, 
                                #slope = input$slope1
                                )})
  res2 <- reactive({xs_eval_all(xs = xs2(), rc = rc2(), 
                                discharges = df_qs$discharges 
                                #sediment_transport = input$toggle_sed, 
                                #slope = input$slope2
                                )})
  
  output$plot_xs <- renderPlot({
    if(input$enable1 & input$enable2) {  
      xs_plot2(xs1 = xs1(), xs2 = xs2(), wse1 = wse1(), wse2 = wse2())
    } else if(input$enable1) {
      xs_plot(xs = xs1(), wse = wse1())
    } else if(input$enable2) {
      xs_plot(xs = xs2(), wse = wse2())
    }
  })
  
  output$plot_rc <- renderPlot({
    if(input$enable1 & input$enable2) {  
      xs_plot_rc2(rc1 = rc1(), rc2 = rc2())
    } else if(input$enable1) {
      xs_plot_rc(rc = rc1())
    } else if(input$enable2) {
      xs_plot_rc(rc = rc2())
    }
  })
  
  # output$plot_sed <- renderPlot({
  #   if (input$toggle_sed & input$slope1){
  #     if(input$enable1 & input$enable2) {  
  #       xs_plot_sediment2(xs1 = xs1(), xs2 = xs2(), rc1 = rc1(), rc2 = rc2(), slope1 = input$slope1, slope2 = input$slope2)
  #     } else if(input$enable1) {
  #       xs_plot_sediment(xs = xs1(), rc = rc1(), slope = input$slope1)
  #     } else if(input$enable2) {
  #       xs_plot_sediment(xs = xs2(), rc = rc2(), slope = input$slope2)
  #     }
  #   }
  # })
  
  column_name_list <- reactive({
    cols <- c("Profile Name",
              "Discharge (cfs)",
              "Thalweg Elev (ft)", 
              "WS Elev (ft)", 
              "Max Depth (ft)",
              "XS Area (ft)", 
              "Wet Perim (ft)", 
              "Velocity (ft/s)"
              )
    # if(input$toggle_sed){
    #   c(cols,
    #     "Hydraulic Radius (ft)",
    #     "Critical Shields Number",
    #     "Grain Size Mobilized (mm)",
    #     "Shear Velocity (ft/s)",
    #     "Grain Size Suspended (mm)"
    #     ) 
    # } else {
    #   cols
    # }
    cols
  })
  
  numeric_columns <- reactive({
    cols <- c("thalweg_elevation", "water_surface_elevation", "max_depth", "cross_sectional_area", "wetted_perimeter", "velocity")
    #if(input$toggle_sed){
    #  c(cols, "hydraulic_radius", "critical_shields_number", "grain_size_mobilized_mm", "shear_velocity", "grain_size_suspended_mm")
    #} else {
    #  cols
    #}
    cols
  })

  output$eval_result1 <- renderDT({
    if(input$enable1) {
      DT::datatable(res1(), editable = FALSE, options = list(dom = 't'), caption="Cross Section 1", colnames = column_name_list()) %>% 
        DT::formatRound(columns=numeric_columns(), digits=2)
    }
  })
  
  output$eval_result2 <- renderDT({
    if(input$enable2) {
      DT::datatable(res2(), editable = FALSE, options = list(dom = 't'), caption="Cross Section 2", colnames = column_name_list()) %>% 
        DT::formatRound(columns=numeric_columns(), digits=2)
    }
  })
  
  ##########################
  # HIGH WATER MARK MODULE #
  ##########################
  
  df_xs_hwm <- reactive({
    req(input$file_xs_hwm)  
    read_csv(input$file_xs_hwm$datapath) %>% select(c(1,2)) %>% rename(station_ft = 1, elevation_ft = 2)
  })
  
  output$dt_xs_hwm <- renderDT({
    DT::datatable(df_xs_hwm(), 
                  editable = FALSE, 
                  colnames=c("Station (ft)", "Elevation (ft)"), 
                  options = list(dom = 'tp'), 
                  caption="Cross Section Input",
                  selection = "none"
                  )
  })
  
  # start by setting hwm params to the inputs
  #hwm_params <- reactiveValues(
  #  wse = {input$wse_hwm},
  #  n = {input$mannings_hwm},
  #  s = {input$slope_hwm},
  #  q = {input$discharge_hwm},
  #)
  
  xs_hwm <- reactive({
    df_xs_hwm() %>% 
      xs_prep(data = ., sta = !!as.name(colnames(.)[1]), elev = !!as.name(colnames(.)[2]))
    })
  
  wse_hwm <- reactive({
    if (input$hwm_mode=="wse") {
      wse_hwm_derived()
    } else {
      input$wse_hwm
    }
  })
  
  result_hwm <- reactive({
    xs_calc_geom(xs_hwm(), wse_hwm())
  })
  
  df_result_hwm <- reactive({
    c(result_hwm(), "hydraulic_radius" = result_hwm()$cross_sectional_area/result_hwm()$wetted_perimeter) %>% 
      tibble::enframe() %>%
      mutate(name = name %>% stringr::str_replace_all("_", " ") %>% stringr::str_to_title())
  })
  
  output$dt_result_hwm <- renderDT({
    if(!is.na(input$wse_hwm)) {
     # if(input$wse_hwm >= min(df_xs_hwm()$elev)){
      DT::datatable(df_result_hwm(), 
                    caption = "Hydraulic Geometry",
                    editable = FALSE, 
                    colnames=NULL,#c("Parameter", "Value"), 
                    options = list(dom = 't', ordering = FALSE,
                                   columnDefs = list(list(className = 'dt-right', targets = 1))),
                    selection = "none",
                    rownames = FALSE,
                    class = list(stripe = FALSE)
                    )  %>% 
        DT::formatRound(columns="value", digits=2)
      #}
    } 
  })
  
  output$plot_xs_hwm <- renderPlot({
      xs_plot(xs = xs_hwm(), wse = wse_hwm())
  })
  
  mannings_hwm <- reactive({
    if (input$hwm_mode=="n") {
      if (!is.na(input$slope_hwm) & !is.na(input$discharge_hwm)) {
        1.486 * 
          result_hwm()$cross_sectional_area * 
          (result_hwm()$cross_sectional_area / result_hwm()$wetted_perimeter)^(2/3) *
          input$slope_hwm^(1/2) *
          input$discharge_hwm^(-1) 
      } else {
        NA
      }
    } else {
      input$mannings_hwm
    } 
    })
  
  slope_hwm <- reactive({
    if (input$hwm_mode=="s") {
      if (!is.na(input$mannings_hwm) & !is.na(input$discharge_hwm)) {
       (input$mannings_hwm^2 * input$discharge_hwm^2) /
          (1.486^2 * result_hwm()$cross_sectional_area^2 *
             (result_hwm()$cross_sectional_area / result_hwm()$wetted_perimeter)^(4/3))
    } else {
        NA
      }
    } else {
      input$slope_hwm
    } 
  })
  
  discharge_hwm <- reactive({
    if (input$hwm_mode=="q") {
      if (!is.na(input$slope_hwm) & !is.na(input$mannings_hwm)) {
        1.486 * 
          result_hwm()$cross_sectional_area * 
          (result_hwm()$cross_sectional_area / result_hwm()$wetted_perimeter)^(2/3) *
          input$slope_hwm^(1/2) *
          input$mannings_hwm^(-1) 
      } else {
        NA
      }
    } else {
      input$discharge_hwm
    } 
  })
  
  observeEvent(input$hwm_mode, {
    shinyjs::toggleState("wse_hwm",       condition = input$hwm_mode %in% c("default", "n", "s", "q"))
    shinyjs::toggleState("mannings_hwm",  condition = input$hwm_mode %in% c("s", "q", "wse"))
    shinyjs::toggleState("slope_hwm",     condition = input$hwm_mode %in% c("n", "q", "wse"))
    shinyjs::toggleState("discharge_hwm", condition = input$hwm_mode %in% c("n", "s", "wse"))
  })

  rc_hwm <- reactive({
    xs_rating_curve(xs = xs_hwm(), slope_hwm(), mannings_hwm())
    })
  
  output$plot_rc_hwm <- renderPlot({
    if (input$hwm_mode!="default"){
      xs_plot_rc(rc = rc_hwm())
    }
  })
  
  wse_hwm_derived <- reactive({
    xs_rc_interpolate(rc = rc_hwm(), discharge = discharge_hwm())
  })
  
  observe({
    if (input$hwm_mode=="n") {
      updateNumericInput(inputId = "mannings_hwm", value = mannings_hwm() %>% signif(2))
    }
  })
  observe({
    if (input$hwm_mode=="s") {
      updateNumericInput(inputId = "slope_hwm", value = slope_hwm() %>% signif(2))
    }
  })
  observe({
    if (input$hwm_mode=="q") {
      updateNumericInput(inputId = "discharge_hwm", value = discharge_hwm() %>% round(2))
    }
  })
  observe({
    if (input$hwm_mode=="wse") {
      updateNumericInput(inputId = "wse_hwm", value = wse_hwm_derived() %>% round(2))
    }
  })
  
}
