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
  
  #observeEvent(input$dt_xs1_cell_edit, {
  #  info = input$dt_xs1_cell_edit
  #  i = as.numeric(info$row)
  #  j = as.numeric(info$col)
  #  k = as.numeric(info$value)
  #  df_xs1()[i,j] <- k
  #})
  #
  #observeEvent(input$dt_xs2_cell_edit, {
  #  info = input$dt_xs2_cell_edit
  #  i = as.numeric(info$row)
  #  j = as.numeric(info$col)
  #  k = as.numeric(info$value)
  #  df_xs2()[i,j] <- k
  #})
  
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
  res1 <- reactive({xs_eval_all(xs = xs1(), rc = rc1(), discharges = df_qs$discharges)})
  res2 <- reactive({xs_eval_all(xs = xs2(), rc = rc2(), discharges = df_qs$discharges)})
  
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
  
  column_name_list <- c("Profile Name",
                        "Discharge (cfs)",
                        "Thalweg Elev (ft)", 
                        "WS Elev (ft)", 
                        "Max Depth (ft)",
                        "XS Area (ft)", 
                        "Wet Perim (ft)", 
                        "Velocity (ft/s)")
  
  output$eval_result1 <- renderDT({
    if(input$enable1) {
      DT::datatable(res1(), editable = FALSE, autoHideNavigation = TRUE, options = list(dom = 't'), caption="Cross Section 1", colnames = column_name_list) %>% 
        DT::formatRound(columns=c("thalweg_elevation", "water_surface_elevation", "max_depth", "cross_sectional_area", "wetted_perimeter", "velocity"), digits=2)
    }
  })
  
  output$eval_result2 <- renderDT({
    if(input$enable2) {
      DT::datatable(res2(), editable = FALSE, autoHideNavigation = TRUE, options = list(dom = 't'), caption="Cross Section 2", colnames = column_name_list) %>% 
        DT::formatRound(columns=c("thalweg_elevation", "water_surface_elevation", "max_depth", "cross_sectional_area", "wetted_perimeter", "velocity"), digits=2)
    }
  })
  

  
}
