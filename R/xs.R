library(tidyverse)

#' Input a table of station-elevation cross section coordinates, such as from a rod-and-level survey.
#' Output a data frame with densified coordinates that can be used for post-processing.
#' @param data A tibble or data frame containing station-elevation pairs
#' @param sta The name of the station variable in the source data
#' @param elev The name of the elevation variable in the source data 
#' @param delta_x The x interval width to be used for densification. Narrower intervals will produce better results. Defaults to 0.1 ft.
prep_xs <- function(data, sta, elev, delta_x=0.1) {
  
  # densify along the horizontal axis
  result <- data %>%
    arrange({{sta}}) %>%
    mutate(sta = {{sta}}) %>%
    complete(sta = seq(from = min({{sta}}), to = max({{sta}}), by = delta_x)) %>%
    mutate(gse = zoo::na.approx({{elev}}, x = sta)) %>% 
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

#' Input a cross section data frame as returned by the prep_xs function.
#' Outputs a named vector of hydraulic parameters (such as cross-sectional area and wetted perimeter) at the specified water surface elevation.
#' @param xs A tbl_df containing cross section geometry, as returned by the prep_xs function
#' @param water_elev The water surface elevation at which the 
calc_xs <- function(xs, water_elev) {
  xs %>% 
    arrange(sta) %>%
    mutate(wse = case_when(water_elev > gse ~ water_elev),
           delta_x = abs(lag(sta, 1) - sta),
           delta_z = abs(lag(gse, 1) - gse),
           depth = wse - gse, 
           hyp_length = sqrt(delta_x^2 + delta_z^2)
    ) %>%
    filter(!is.na(wse)) %>% 
    summarize(thalweg_elevation = min(gse),
              water_surface_elevation = water_elev,
              max_depth = water_surface_elevation - thalweg_elevation,
              cross_sectional_area = sum(delta_x * depth),
              wetted_perimeter = sum(hyp_length)
    ) %>% 
    as.list() %>% 
    list_flatten()
}

#' This function runs the calc_xs function along a series of water surface elevations to return a rating curve of water surface elevation versus cross-sectional area and wetted perimeter. Then it applies Manning's equation to estimate depth and velocity. Returns a tbl_df with one row per water surface elevation.
#' @param xs A tbl_df containing cross section geometry, as returned by the prep_xs function
#' @param slope The channel profile slope at the cross section, used in Manning's equation.
#' @param mannings_n The roughness coefficient for the cross section, used in Manning's equation.
#' @param delta_z The elevation interval to be used for calculating outputs at different water surface elevations. Defaults to 0.1 ft.
calculate_rating_curve <- function(xs, slope, mannings_n, delta_z=0.1) {
  rating_curve <- seq(from=min(xs$gse)+delta_z, to=max(xs$gse), by=delta_z) %>% 
    as_tibble() %>%
    mutate(result = map(value, function(x){calc_xs(data = xs, water_elev = x)})) %>% 
    unnest_wider(col = result) %>%
    drop_na() %>%
    mutate(discharge_cfs = 1.486 * cross_sectional_area * 
             (cross_sectional_area / wetted_perimeter)^(2/3) * slope^(1/2) * mannings_n^(-1),
           velocity_ft_s = discharge_cfs / cross_sectional_area) %>%
    arrange(discharge_cfs)
}

#' This function takes a rating curve calculated by calculate_rating_curve and returns the water surface elevation for a given discharge. 
#' @param rc A tbl_df containing a depth-discharge rating curve, as returned by the calculate_rating_curve function
#' @param discharge A discharge (cfs) number at which to determine the water surface elevation
interpolate_rating_curve <- function(rc, discharge) {
  rc %>%
    bind_rows(tribble(~selected_water_level, ~discharge_cfs, TRUE, discharge)) %>%
    arrange(discharge_cfs) %>%
    mutate(output_wse = zoo::na.approx(water_surface_elevation)) %>%
    filter(selected_water_level) %>% 
    pull(output_wse)
}

#' This function takes a rating curve calculated by calculate_rating_curve and returns hydraulic parameters for multiple given discharges 
#' @param xs A tbl_df containing cross section geometry, as returned by the prep_xs function
#' @param rc A tbl_df containing a depth-discharge rating curve, as returned by the calculate_rating_curve function
#' @param discharges A discharge (cfs) number at which to determine the water surface elevation, or a vector of multiple discharges. Vector can be named or unnamed. Also accepts a data frame or tibble containing a column called "discharge"
xs_eval_all <- function(xs, rc, discharges) {
  if (!("data.frame" %in% class(discharges))){
    discharges <- enframe(discharges, value = "discharge")
  }
  discharges %>%
    mutate(xs_parameters = map(discharge, function(discharge) {
        rc %>% 
        interpolate_rating_curve(., discharge) %>% 
        calc_xs(xs, .)})) %>% 
    unnest_wider(xs_parameters) %>%
    mutate(velocity = discharge / cross_sectional_area)
}
