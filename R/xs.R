#' @title Prep cross section data
#' @description
#' Input a table of station-elevation cross section coordinates, such as from a rod-and-level survey.
#' Output a data frame with densified coordinates that can be used for post-processing.
#' @param data A tibble or data frame containing station-elevation pairs
#' @param sta The name of the station variable in the source data
#' @param elev The name of the elevation variable in the source data 
#' @param delta_x The x interval width to be used for densification. Narrower intervals will produce better results. Defaults to 0.1 ft.
#' @md
#' @export
xs_prep <- function(data, sta, elev, delta_x=0.1) {
  
  # densify along the horizontal axis
  result <- data %>%
    arrange({{sta}}) %>%
    mutate(sta = {{sta}}) %>%
    complete(sta = seq(from = min({{sta}}), to = max({{sta}}), by = delta_x)) %>%
    mutate(gse = zoo::na.approx({{elev}}, x = sta)) %>%  #, na.rm = FALSE
    select(c(sta, gse))
  
  # densify along the vertical axis
  d <- result %>% 
    arrange(-sta) %>%
    mutate(delta_x = abs(lag(sta, 1) - sta),
           delta_z = abs(lag(gse, 1) - gse),
           multiplier = replace_na(ceiling(delta_z/delta_x),1)) %>% 
    arrange(sta)
  
  idx <- rep(1:nrow(d), d$multiplier)
  
  return(
    d[idx,] %>% 
      mutate(gse = case_when(duplicated(sta) ~ NA, TRUE ~ gse),
             sta = case_when(duplicated(sta) ~ NA, TRUE ~ sta)) %>%
      mutate(gse = zoo::na.approx(gse, na.rm = FALSE),
             sta = zoo::na.approx(sta, na.rm = FALSE)) %>%
      select(sta, gse) %>% drop_na() 
  )
  
}

#' @title Calculate cross section hydraulic geometry properties
#' @description
#' Input a cross section data frame as returned by the `xs_prep` function.
#' Outputs a named vector of hydraulic parameters (such as cross-sectional area and wetted perimeter) at the specified water surface elevation.
#' @param xs A tbl_df containing cross section geometry, as returned by the `xs_prep` function
#' @param water_surface_elev The water surface elevation at which the properties are calculated
#' @md
#' @export
xs_calc_geom <- function(xs, water_surface_elev) {
  xs %>% 
    arrange(sta) %>%
    mutate(wse = case_when(water_surface_elev > gse ~ water_surface_elev),
           delta_x = abs(lag(sta, 1) - sta),
           delta_z = abs(lag(gse, 1) - gse),
           depth = wse - gse, 
           hyp_length = sqrt(delta_x^2 + delta_z^2)
    ) %>%
    filter(!is.na(wse)) %>% 
    summarize(thalweg_elevation = min(gse),
              water_surface_elevation = water_surface_elev,
              max_depth = water_surface_elevation - thalweg_elevation,
              cross_sectional_area = sum(delta_x * depth),
              wetted_perimeter = sum(hyp_length)
    ) %>% 
    as.list() %>% 
    list_flatten()
}

#' @title Create rating curve for cross section with given slope and roughness
#' @description
#' This function runs the `xs_calc_geom` function along a series of water surface elevations to return a rating curve of water surface elevation versus cross-sectional area and wetted perimeter. Then it applies Manning's equation to estimate depth and velocity. Returns a `tbl_df` with one row per water surface elevation.
#' @param xs A `tbl_df` containing cross section geometry, as returned by the `xs_prep` function
#' @param slope The channel profile slope at the cross section, used in Manning's equation (normal depth assumption)
#' @param mannings_n The roughness coefficient for the cross section, used in Manning's equation.
#' @param delta_z The elevation interval to be used for calculating outputs at different water surface elevations. Defaults to 0.1 ft.
#' @md
#' @export
xs_rating_curve <- function(xs, slope, mannings_n, delta_z=0.1) {
  rating_curve <- seq(from=min(xs$gse)+delta_z, to=max(xs$gse), by=delta_z) %>% 
    as_tibble() %>%
    mutate(result = map(value, function(wse){xs_calc_geom(xs, wse)})) %>% 
    unnest_wider(col = result) %>%
    drop_na() %>%
    mutate(discharge = 1.486 * cross_sectional_area * 
             (cross_sectional_area / wetted_perimeter)^(2/3) * slope^(1/2) * mannings_n^(-1),
           velocity = discharge / cross_sectional_area) %>%
    arrange(discharge)
}

#' @title Interpolate WSE from rating curve at given discharge
#' @description
#' This function takes a rating curve calculated by xs_rating_curve and returns the water surface elevation for a given discharge. 
#' @param rc A `tbl_df` containing a depth-discharge rating curve, as returned by the `xs_rating_curve` function
#' @param discharge A discharge (cfs) number at which to determine the water surface elevation
#' @md
#' @export
xs_rc_interpolate <- function(rc, discharge) {
  out <- tryCatch({
  rc %>%
    bind_rows(tribble(~selected_wse, ~discharge, TRUE, discharge)) %>%
    arrange(discharge) %>%
    mutate(output_wse = zoo::na.approx(water_surface_elevation), na.rm = FALSE) %>%
    filter(selected_wse) %>% 
    pull(output_wse)
  }, 
  error=function(cond) {
    return(NA)
  })
}

#' @title Calculate hydraulic geometry properties, WSE, and velocity at multiple discharges
#' @description
#' This function takes a cross section data frame as returned by the `xs_prep` function, and a rating curve calculated by `xs_rating_curve.` It returns hydraulic parameters for one or more given discharges.
#' @param xs A `tbl_df` containing cross section geometry, as returned by the `xs_prep` function
#' @param rc A `tbl_df` containing a depth-discharge rating curve, as returned by the `xs_rating_curve` function
#' @param discharges A discharge (cfs) number at which to determine the water surface elevation, or a vector of multiple discharges. Vector can be named or unnamed. Also accepts a data frame or tibble containing a column called "discharge"
#' @param sediment_transport Specify whether to calculate bed mobilization and grain suspension. Defaults to FALSE
#' @param slope If calculating sediment transport, specify a slope to use for the calculation (normal depth assumption)
#' @md
#' @export
xs_eval_all <- function(xs, rc, discharges, sediment_transport = FALSE, slope = NA) {
  if (!("data.frame" %in% class(discharges))){
    discharges <- enframe(discharges, value = "discharge")
  }
  out <- discharges %>%
    mutate(xs_parameters = map(discharge, function(discharge) {
      rc %>% 
        xs_rc_interpolate(., discharge) %>% 
        xs_calc_geom(xs, .)})) %>% 
    unnest_wider(xs_parameters) %>%
    mutate(velocity = discharge / cross_sectional_area)
  if (sediment_transport & !is.na(slope)) {
    out <- out %>%
      mutate(sediment_result = map(water_surface_elevation, function(x) {xs_sediment_transport(xs, x, slope)})) %>% 
      unnest_wider(sediment_result)
  }
  return(out)
}

#' @title Plot cross section
#' @description
#' This function takes a cross section data frame as returned by the `xs_prep` function, and plots it with 1:1 scale. Optionally also adds a given water surface line.
#' @param xs A tbl_df containing cross section geometry, as returned by the `xs_prep` function
#' @param wse A water surface elevation, as returned by the `xs_rc_interpolate` function
#' @md
#' @export
xs_plot <- function(xs, wse = NA) {
  plt <- ggplot(data = xs) + geom_line(aes(x = sta, y = gse, linetype = "Terrain"))
  if (!is.na(wse)) {
    plt <- plt +  geom_line(aes(x = sta, y = case_when(wse > gse ~ wse), linetype = "Water Surface"))
  }
  return(plt + xlab("Station (ft)") + ylab("Elevation (ft)") + theme_classic() + theme(legend.position="top", legend.title=element_blank()) + coord_fixed(ratio = 1) 
  )
}

#' @title Plot rating curve
#' @description
#' This function plots a rating curve as returned by the `xs_rating_curve` function.
#' @param rc A `tbl_df` containing a depth-discharge rating curve, as returned by the xs_rating_curve function
#' @param y The unquoted name of a rating curve variable to plot on the y axis. Defaults to `water_surface_elevation`.
#' @param x The unquoted name of a rating curve variable to plot on the x axis. Defaults to `discharge`.
#' @param ylab If a custom y axis variable is selected, provide a text label for the y axis.
#' @param xlab If a custom x axis variable is selected, provide a text label for the x axis.
#' @md
#' @export
xs_plot_rc <- function(rc, y = water_surface_elevation, x = discharge, ylab="Water Surface Elevation (ft)", xlab="Discharge (cfs)") {
  plt <- ggplot(data = rc) + geom_line(aes(y = {{y}}, x = {{x}}))
  return(plt + ylab(ylab) + xlab(xlab) + theme_classic())
}

#' @title Plot two cross sections
#' @description
#' This function takes two cross section datas frame as returned by the `xs_prep` function, and plots them with 1:1 scale. Optionally also adds a given water surface line.
#' @param xs1 A tbl_df containing a cross section geometry, as returned by the `xs_prep` function
#' @param xs2 A tbl_df containing a cross section geometry, as returned by the `xs_prep` function
#' @param wse1 A water surface elevation for `xs1`, as returned by the `xs_rc_interpolate` function
#' @param wse2 A water surface elevation for `xs2`, as returned by the `xs_rc_interpolate` function
#' @param label1 The label for the first cross section `xs1`. Defaults to "Baseline"
#' @param label2 The label for the second cross section `xs2`. Defaults to "Design"
#' @md
#' @export
xs_plot2 <- function(xs1, xs2, wse1 = NA, wse2 = NA, label1 = "Baseline", label2 = "Design") {
  plt <- ggplot() + 
    geom_line(data = xs1, aes(x = sta, y = gse, color = label1, linetype = "Terrain")) + 
    geom_line(data = xs2, aes(x = sta, y = gse, color = label2, linetype = "Terrain"))
  if (!is.na(wse1)) {
    plt <- plt +  geom_line(data = xs1, aes(x = sta, y = case_when(wse1 > gse ~ wse1), color = label1, linetype = "Water Surface"))
  }
  if (!is.na(wse2)) {
    plt <- plt +  geom_line(data = xs2, aes(x = sta, y = case_when(wse2 > gse ~ wse2), color = label2, linetype = "Water Surface"))
  }
  return(plt + xlab("Station (ft)") + ylab("Elevation (ft)") + theme_classic() + theme(legend.position="top", legend.title=element_blank()) + coord_fixed(ratio = 1) 
  )
}

#' @title Plot two rating curves
#' @description
#' This function plots two rating curves as returned by the `xs_rating_curve` function. For now, always plots water surface elevation versus discharge.
#' @param rc A `tbl_df` containing a depth-discharge rating curve, as returned by the xs_rating_curve function
#' @param label1 The label for the first cross section `xs1`. Defaults to "Baseline"
#' @param label2 The label for the second cross section `xs2`. Defaults to "Design"
#' @md
#' @export
xs_plot_rc2 <- function(rc1, rc2, label1 = "Baseline", label2 = "Design") {
  plt <- ggplot() + 
    geom_line(data = rc1, aes(y = water_surface_elevation, x = discharge, color = label1)) +
    geom_line(data = rc2, aes(y = water_surface_elevation, x = discharge, color = label2)) + 
    ylab("Water Surface Elevation (ft)") + xlab("Discharge (cfs)") + theme_classic() + theme(legend.position="top", legend.title=element_blank())
  return(plt)
}

#' @title Launch app
#' @description
#' This function launches the interactive cross section application.
#' @export
xs_run_app <- function() {
  appDir <- system.file("app", package = "xsmatic")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `xsmatic`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}

#' @title Back-calculate Manning's n given known slope
#' @description
#' Input a cross section data frame as returned by the `xs_prep` function, along with a WSE and slope as might be gathered in a high water mark survey, and a known discharge having produced the given water surface elevation.
#' Returns a Manning's roughness coefficient by back-calculating from Manning's equation.
#' @param xs A tbl_df containing cross section geometry, as returned by the `xs_prep` function
#' @param water_surface_elev The water surface elevation at which the properties are calculated
#' @param slope The channel profile slope at the cross section, used in Manning's equation (normal depth assumption)
#' @param discharge A known discharge value in cfs
xs_mannings_n <- function(xs, water_surface_elev, slope, discharge) {
  xs_calc_geom(xs, water_surface_elev) %>% 
    enframe() %>% 
    pivot_wider() %>%
    unnest(cols=c(thalweg_elevation, water_surface_elevation, max_depth, cross_sectional_area, wetted_perimeter)) %>%
    mutate(
      hydraulic_radius = cross_sectional_area / wetted_perimeter,
      mannings_n = 1.486 * cross_sectional_area * hydraulic_radius^(2/3) 
      * slope^(1/2) * discharge^(-1)
    ) %>%
    pull(mannings_n)
}

#' @title Calculate cross section hydraulic geometry properties and sediment transport estimates
#' @description
#' Input a cross section data frame as returned by the `xs_prep` function.
#' Outputs a named vector with estimates for median mobilized bed grain size and maximum suspended load grain size
#' @param xs A tbl_df containing cross section geometry, as returned by the `xs_prep` function
#' @param water_surface_elev The water surface elevation at which the properties are calculated
#' @param slope The channel profile slope at the cross section, used in Shield's equation and estimation of shear velocity (normal depth assumption)
#' @md
#' @export
xs_sediment_transport <- function(xs, water_surface_elev, slope) {
  
  # gravitational constant, cm/s2
  g_cgs <- 981
  # grain density and water density, g/cm3
  rho_s_cgs <- 2.65
  rho_cgs <- 1.00
  # kinematic viscosity of water, cm2/s
  nu_cgs <- 0.01
  
  xs_calc_geom(xs, water_surface_elev) %>% 
    enframe() %>% 
    pivot_wider() %>%
    unnest(cols=c(thalweg_elevation, water_surface_elevation, max_depth, cross_sectional_area, wetted_perimeter)) %>%
    mutate(
      hydraulic_radius = cross_sectional_area / wetted_perimeter,
      # metric conversions
      hydraulic_radius_m = hydraulic_radius / 0.3048,
      cross_sectional_area_m2 = cross_sectional_area / 0.3048^2,
      # bed mobilization
      critical_shields_number = 0.15 * slope^(1/4),
      grain_size_mobilized_mm = 10 * rho_cgs * hydraulic_radius_m * slope / 
        (critical_shields_number * (rho_s_cgs - rho_cgs)),
      grain_size_mobilized_phi = -log2(grain_size_mobilized_mm),
      # suspended transport
      shear_velocity_cm_s = sqrt(g_cgs * (hydraulic_radius_m*100) * slope),
      settling_velocity_ndim = rho_cgs * shear_velocity_cm_s^3 / 
        ((rho_s_cgs - rho_cgs) * g_cgs * nu_cgs),
      grain_size_suspended_ndim = sqrt(5832 * settling_velocity_ndim),
      grain_size_suspended_mm = 10 * grain_size_suspended_ndim * rho_cgs * nu_cgs^2 /
        ((rho_s_cgs - rho_cgs) * g_cgs)^(1/3),
      grain_size_suspended_phi = -log2(grain_size_suspended_mm),
      shear_velocity = shear_velocity_cm_s / 30.48 # ft/sec
    ) %>% 
    select(hydraulic_radius, critical_shields_number, grain_size_mobilized_mm, shear_velocity, grain_size_suspended_mm) %>%
    as.list() %>% 
    list_flatten()
}
