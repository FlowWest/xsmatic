library(tidyverse)

#' Input a table of station-elevation cross section coordinates, such as from a rod-and-level survey.
#' Output a data frame with densified coordinates that can be used for post-processing.
#' @param data A tibble or data frame containing station-elevation pairs
#' @param sta The name of the station variable in the source data
#' @param elev The name of the elevation variable in the source data 
#' @param delta_x The x interval width to be used for densification. Narrower intervals will produce better results. Defaults to 0.1 ft.
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

#' Input a cross section data frame as returned by the xs_prep function.
#' Outputs a named vector of hydraulic parameters (such as cross-sectional area and wetted perimeter) at the specified water surface elevation.
#' @param xs A tbl_df containing cross section geometry, as returned by the xs_prep function
#' @param water_elev The water surface elevation at which the 
xs_calc_geom <- function(xs, water_surface_elevation) {
  xs %>% 
    arrange(sta) %>%
    mutate(wse = case_when(water_surface_elevation > gse ~ water_surface_elevation),
           delta_x = abs(lag(sta, 1) - sta),
           delta_z = abs(lag(gse, 1) - gse),
           depth = wse - gse, 
           hyp_length = sqrt(delta_x^2 + delta_z^2)
    ) %>%
    filter(!is.na(wse)) %>% 
    summarize(thalweg_elevation = min(gse),
              water_surface_elevation = water_surface_elevation,
              max_depth = water_surface_elevation - thalweg_elevation,
              cross_sectional_area = sum(delta_x * depth),
              wetted_perimeter = sum(hyp_length)
    ) %>% 
    as.list() %>% 
    list_flatten()
}

#' This function runs the xs_calc_geom function along a series of water surface elevations to return a rating curve of water surface elevation versus cross-sectional area and wetted perimeter. Then it applies Manning's equation to estimate depth and velocity. Returns a tbl_df with one row per water surface elevation.
#' @param xs A tbl_df containing cross section geometry, as returned by the xs_prep function
#' @param slope The channel profile slope at the cross section, used in Manning's equation.
#' @param mannings_n The roughness coefficient for the cross section, used in Manning's equation.
#' @param delta_z The elevation interval to be used for calculating outputs at different water surface elevations. Defaults to 0.1 ft.
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

#' This function takes a rating curve calculated by xs_rating_curve and returns the water surface elevation for a given discharge. 
#' @param rc A tbl_df containing a depth-discharge rating curve, as returned by the xs_rating_curve function
#' @param discharge A discharge (cfs) number at which to determine the water surface elevation
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

#' This function takes a cross section data frame as returned by the xs_prep function, and a rating curve calculated by xs_rating_curve. It returns hydraulic parameters for one or more given discharges.
#' @param xs A tbl_df containing cross section geometry, as returned by the xs_prep function
#' @param rc A tbl_df containing a depth-discharge rating curve, as returned by the xs_rating_curve function
#' @param discharges A discharge (cfs) number at which to determine the water surface elevation, or a vector of multiple discharges. Vector can be named or unnamed. Also accepts a data frame or tibble containing a column called "discharge"
xs_eval_all <- function(xs, rc, discharges) {
  if (!("data.frame" %in% class(discharges))){
    discharges <- enframe(discharges, value = "discharge")
  }
  discharges %>%
    mutate(xs_parameters = map(discharge, function(discharge) {
        rc %>% 
        xs_rc_interpolate(., discharge) %>% 
        xs_calc_geom(xs, .)})) %>% 
    unnest_wider(xs_parameters) %>%
    mutate(velocity = discharge / cross_sectional_area)
}

#' This function takes a cross section data frame as returned by the xs_prep function, and plots it with 1:1 scale. Optionally also adds a given water surface line.
#' @param xs A tbl_df containing cross section geometry, as returned by the xs_prep function
#' @param wse A water surface elevation, as returned by the xs_rc_interpolate function
xs_plot <- function(xs, wse = NA) {
  plt <- ggplot(data = xs) + geom_line(aes(x = sta, y = gse, linetype = "Terrain"))
  if (!is.na(wse)) {
    plt <- plt +  geom_line(aes(x = sta, y = case_when(wse > gse ~ wse), linetype = "Water Surface"))
  }
  return(plt + xlab("Station (ft)") + ylab("Elevation (ft)") + theme_classic() + theme(legend.position="top", legend.title=element_blank()) + coord_fixed(ratio = 1) 
  )
}

#' This function takes a cross section data frame as returned by the xs_prep function, and plots it with 1:1 scale. Optionally also adds a given water surface line.
#' @param rc A tbl_df containing a depth-discharge rating curve, as returned by the xs_rating_curve function
#' @param y The unquoted name of a rating curve variable to plot on the y axis. Defaults to max_depth.
#' @param x The unquoted name of a rating curve variable to plot on the x axis. Defaults to discharge.
#' @param ylab If a custom y axis variable is selected, provide a text label for the y axis.
#' @param ylab If a custom x axis variable is selected, provide a text label for the x axis.
xs_plot_rc <- function(rc, y = water_surface_elevation, x = discharge, ylab="Water Surface Elevation (ft)", xlab="Discharge (cfs)") {
  plt <- ggplot(data = rc) + geom_line(aes(y = {{y}}, x = {{x}}))
  return(plt + ylab(ylab) + xlab(xlab) + theme_classic())
}

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

xs_plot_rc2 <- function(rc1, rc2, label1 = "Baseline", label2 = "Design") {
  plt <- ggplot() + 
    geom_line(data = rc1, aes(y = water_surface_elevation, x = discharge, color = label1)) +
    geom_line(data = rc2, aes(y = water_surface_elevation, x = discharge, color = label2)) + 
    ylab("Water Surface Elevation (ft)") + xlab("Discharge (cfs)") + theme_classic() + theme(legend.position="top", legend.title=element_blank())
  return(plt)
}
