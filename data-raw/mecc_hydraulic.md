MECC Hydraulic Analysis
================
FlowWest
12 June 2023

``` r
library(tidyverse)
ggplot2::theme_set(theme_classic())
#source("R/xs.R")
library(xsmatic)
```

## Import cross sections

``` r
xs_2007 <- read_csv("xs_2007.csv") %>%
  xs_prep(sta = station_ft, elev = elevation_ft) 

xs_2007 %>% ggplot(aes(y = gse, x = sta)) + geom_point() + 
  ylab("Elevation (ft)") + xlab("Station (ft)") + ggtitle("2007 Baseline") + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
xs_2023 <- read_csv("xs_2023_simplified.csv") %>% #_simplified.csv") %>%
  xs_prep(sta = station_ft, elev = elevation_ft) 

xs_2023 %>% ggplot(aes(y = gse, x = sta)) + geom_point() + 
  ylab("Elevation (ft)") + xlab("Station (ft)") + ggtitle("2023 Proposed") + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
ggplot() + 
  geom_line(data = xs_2007, aes(x = sta, y = gse, color = "2007 Baseline")) +
  geom_line(data = xs_2023, aes(x = sta, y = gse, color = "2023 Proposed")) +
  ylab("Elevation (ft)") + xlab("Station (ft)") + theme(legend.position="top", legend.title=element_blank())  + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

## Calculate depth-discharge rating curves

``` r
model_slope <- (38.6-38.0) / 28.65

model_roughness <- 0.035 # n

print(c("model_slope" = model_slope, "model_roughness" = model_roughness))
```

    ##     model_slope model_roughness 
    ##      0.02094241      0.03500000

*Future improvement to script should allow different slope and roughness
for each scenario.*

``` r
depth_vs_discharge_2007 <- xs_2007 %>% 
  xs_rating_curve(slope = model_slope,
                         mannings_n = model_roughness) # 0.030) #model_roughness)
depth_vs_discharge_2007 %>% 
  ggplot(aes(y = max_depth, x = discharge)) + 
  geom_line() + ylab("Depth (ft)") + xlab("Discharge (cfs)") + ggtitle("2007 Baseline")
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
depth_vs_discharge_2023 <- xs_2023 %>% 
  xs_rating_curve(slope = model_slope,
                         mannings_n = model_roughness)
depth_vs_discharge_2023 %>% 
  ggplot(aes(y = max_depth, x = discharge)) + 
  geom_line() + ylab("Depth (ft)") + xlab("Discharge (cfs)") + ggtitle("2023 Proposed") 
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
ggplot() +
  geom_line(data = depth_vs_discharge_2007, aes(y = max_depth, x = discharge, color = "2007 Baseline")) +
  geom_line(data = depth_vs_discharge_2023, aes(y = max_depth, x = discharge, color = "2023 Proposed")) + 
  ylab("Depth (ft)") + xlab("Discharge (cfs)") + theme(legend.position="top", legend.title=element_blank())
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
ggplot() +
  geom_line(data = depth_vs_discharge_2007, aes(y = water_surface_elevation, x = discharge, color = "2007 Baseline")) +
  geom_line(data = depth_vs_discharge_2023, aes(y = water_surface_elevation, x = discharge, color = "2023 Proposed")) + 
  ylab("Water Surface Elevation (ft)") + xlab("Discharge (cfs)") + theme(legend.position="top", legend.title=element_blank())
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

*Future improvement to script should include functions for the following
analyses to avoid duplication.*

## Q100

### Interpolate water surface for selected discharge

``` r
model_discharge <- 3980 # cfs
```

``` r
wse_2007 <- depth_vs_discharge_2007 %>% xs_rc_interpolate(model_discharge)
print(c("wse 2007" = wse_2007))
```

    ## wse 2007 
    ##    47.84

``` r
wse_2023 <- depth_vs_discharge_2023 %>% xs_rc_interpolate(model_discharge)
print(c("wse 2023" = wse_2023))
```

    ## wse 2023 
    ##    47.64

``` r
xs_2007 %>% xs_calc_geom(wse_2007)
```

    ## $thalweg_elevation
    ## [1] 36.99
    ## 
    ## $water_surface_elevation
    ## [1] 47.84
    ## 
    ## $max_depth
    ## [1] 10.85
    ## 
    ## $cross_sectional_area
    ## [1] 214.2417
    ## 
    ## $wetted_perimeter
    ## [1] 40.73901

``` r
xs_2023 %>% xs_calc_geom(wse_2023)
```

    ## $thalweg_elevation
    ## [1] 36.99
    ## 
    ## $water_surface_elevation
    ## [1] 47.64
    ## 
    ## $max_depth
    ## [1] 10.65
    ## 
    ## $cross_sectional_area
    ## [1] 214.0507
    ## 
    ## $wetted_perimeter
    ## [1] 40.61888

### Plot outputs

``` r
xs_2007 %>% arrange(sta) %>%
      mutate(wse = case_when(wse_2007 > gse ~ wse_2007)) %>%
      ggplot(aes(x = sta)) + 
      geom_line(aes(y = gse)) + 
      geom_line(aes(y = wse)) + 
      xlab("Station (ft)") + ylab("Elevation (ft)") + ggtitle("2007 Baseline") + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
xs_2023 %>% arrange(sta) %>%
      mutate(wse = case_when(wse_2023 > gse ~ wse_2023)) %>%
      ggplot(aes(x = sta)) + 
      geom_line(aes(y = gse)) + 
      geom_line(aes(y = wse)) + 
      xlab("Station (ft)") + ylab("Elevation (ft)") + ggtitle("2023 Proposed") + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-16-1.png)<!-- -->

``` r
ggplot() + 
  geom_line(data = xs_2007, aes(x = sta, y = gse, color = "2007 Baseline", linetype = "Terrain")) +
  geom_line(data = xs_2023, aes(x = sta, y = gse, color = "2023 Proposed", linetype = "Terrain")) +
  geom_line(data = xs_2007, aes(x = sta, y = case_when(wse_2007 > gse ~ wse_2007), 
                                color = "2007 Baseline", linetype = "Water Surface")) + 
  geom_line(data = xs_2023, aes(x = sta, y = case_when(wse_2023 > gse ~ wse_2023), 
                                color = "2023 Proposed", linetype = "Water Surface")) + 
      xlab("Station (ft)") + ylab("Elevation (ft)") + theme(legend.position="top", legend.title=element_blank()) + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

### Calculate design velocity based on Q100 flow

``` r
velocity_2007 <- model_discharge / xs_calc_geom(xs_2007, wse_2007)$cross_sectional_area
print(velocity_2007)
```

    ## [1] 18.57715

``` r
velocity_2023 <- model_discharge / xs_calc_geom(xs_2023, wse_2023)$cross_sectional_area
print(velocity_2023)
```

    ## [1] 18.59373

## Q2

### Interpolate water surface for selected discharge

``` r
model_discharge <- 513 # cfs
```

``` r
wse_2007_Q2 <- depth_vs_discharge_2007 %>% xs_rc_interpolate(model_discharge)
print(c("wse 2007" = wse_2007_Q2))
```

    ## wse 2007 
    ##    40.54

``` r
wse_2023_Q2 <- depth_vs_discharge_2023 %>% xs_rc_interpolate(model_discharge)
print(c("wse 2023" = wse_2023_Q2))
```

    ## wse 2023 
    ##    41.24

``` r
xs_2007 %>% xs_calc_geom(wse_2007_Q2)
```

    ## $thalweg_elevation
    ## [1] 36.99
    ## 
    ## $water_surface_elevation
    ## [1] 40.54
    ## 
    ## $max_depth
    ## [1] 3.55
    ## 
    ## $cross_sectional_area
    ## [1] 48.38457
    ## 
    ## $wetted_perimeter
    ## [1] 22.13284

``` r
xs_2023 %>% xs_calc_geom(wse_2023_Q2)
```

    ## $thalweg_elevation
    ## [1] 36.99
    ## 
    ## $water_surface_elevation
    ## [1] 41.24
    ## 
    ## $max_depth
    ## [1] 4.25
    ## 
    ## $cross_sectional_area
    ## [1] 47.22972
    ## 
    ## $wetted_perimeter
    ## [1] 20.87879

### Plot outputs

``` r
xs_2007 %>% arrange(sta) %>%
      mutate(wse = case_when(wse_2007_Q2 > gse ~ wse_2007_Q2)) %>%
      ggplot(aes(x = sta)) + 
      geom_line(aes(y = gse)) + 
      geom_line(aes(y = wse)) + 
      xlab("Station (ft)") + ylab("Elevation (ft)") + ggtitle("2007 Baseline") + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-25-1.png)<!-- -->

``` r
xs_2023 %>% arrange(sta) %>%
      mutate(wse = case_when(wse_2023_Q2 > gse ~ wse_2023_Q2)) %>%
      ggplot(aes(x = sta)) + 
      geom_line(aes(y = gse)) + 
      geom_line(aes(y = wse)) + 
      xlab("Station (ft)") + ylab("Elevation (ft)") + ggtitle("2023 Proposed") + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
ggplot() + 
  geom_line(data = xs_2007, aes(x = sta, y = gse, color = "2007 Baseline", linetype = "Terrain")) +
  geom_line(data = xs_2023, aes(x = sta, y = gse, color = "2023 Proposed", linetype = "Terrain")) +
  geom_line(data = xs_2007, aes(x = sta, y = case_when(wse_2007_Q2 > gse ~ wse_2007_Q2), 
                                color = "2007 Baseline", linetype = "Water Surface")) + 
  geom_line(data = xs_2023, aes(x = sta, y = case_when(wse_2023_Q2 > gse ~ wse_2023_Q2), 
                                color = "2023 Proposed", linetype = "Water Surface")) + 
      xlab("Station (ft)") + ylab("Elevation (ft)") + theme(legend.position="top", legend.title=element_blank()) + coord_fixed(ratio = 1)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

### Calculate design velocity based on Q2 flow

``` r
velocity_2007_Q2 <- model_discharge / xs_calc_geom(xs_2007, wse_2007_Q2)$cross_sectional_area
print(velocity_2007_Q2)
```

    ## [1] 10.60255

``` r
velocity_2023_Q2 <- model_discharge / xs_calc_geom(xs_2023, wse_2023_Q2)$cross_sectional_area
print(velocity_2023_Q2)
```

    ## [1] 10.86181

## Output design velocity tables

Calculate design velocities based on all modeled flood events from Q2 to
Q100

``` r
streamstats_result <- tribble(~return_interval, ~discharge,
                              2 ,   513,
                              5,   1320,
                              10,  1970,
                              25,  2780,
                              50,  3360,
                              100, 3980 ) 

velocities <- streamstats_result %>%
  mutate(xs_area_2007 = map_dbl(discharge, function(discharge) {
            depth_vs_discharge_2007 %>% 
            xs_rc_interpolate(., discharge) %>%
            xs_calc_geom(xs_2007, .) %>% 
            .$cross_sectional_area
            }),
         xs_area_2023 = map_dbl(discharge, function(discharge) {
            depth_vs_discharge_2023 %>% 
            xs_rc_interpolate(., discharge) %>% 
            xs_calc_geom(xs_2023, .) %>% 
            .$cross_sectional_area
            }),
         velocity_2007 = discharge / xs_area_2007,
         velocity_2023 = discharge / xs_area_2023
         )
velocities %>% knitr::kable()
```

| return_interval | discharge | xs_area_2007 | xs_area_2023 | velocity_2007 | velocity_2023 |
|----------------:|----------:|-------------:|-------------:|--------------:|--------------:|
|               2 |       513 |     48.38457 |     47.22972 |      10.60255 |      10.86181 |
|               5 |      1320 |     95.81888 |     95.12741 |      13.77599 |      13.87613 |
|              10 |      1970 |    125.46387 |    128.97752 |      15.70173 |      15.27398 |
|              25 |      2780 |    159.92333 |    165.48131 |      17.38333 |      16.79948 |
|              50 |      3360 |    185.94605 |    189.26527 |      18.06976 |      17.75286 |
|             100 |      3980 |    214.24166 |    214.05067 |      18.57715 |      18.59373 |

Stable version using function:

``` r
all_parms_2023 <- xs_eval_all(xs = xs_2023, rc = depth_vs_discharge_2023, discharges = streamstats_result)
all_parms_2023 %>% knitr::kable()
```

| return_interval | discharge | thalweg_elevation | water_surface_elevation | max_depth | cross_sectional_area | wetted_perimeter | velocity |
|----------------:|----------:|------------------:|------------------------:|----------:|---------------------:|-----------------:|---------:|
|               2 |       513 |             36.99 |                   41.24 |      4.25 |             47.22972 |         20.87879 | 10.86181 |
|               5 |      1320 |             36.99 |                   43.44 |      6.45 |             95.12741 |         28.59194 | 13.87613 |
|              10 |      1970 |             36.99 |                   44.74 |      7.75 |            128.97752 |         32.37290 | 15.27398 |
|              25 |      2780 |             36.99 |                   46.04 |      9.05 |            165.48131 |         36.03294 | 16.79948 |
|              50 |      3360 |             36.99 |                   46.84 |      9.85 |            189.26527 |         38.38498 | 17.75286 |
|             100 |      3980 |             36.99 |                   47.64 |     10.65 |            214.05067 |         40.61888 | 18.59373 |

## plotting functions

``` r
xs_2007 %>% xs_plot(wse = wse_2007)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->

``` r
depth_vs_discharge_2007 %>% xs_plot_rc()
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-33-1.png)<!-- -->

``` r
xs_plot2(xs_2007, xs_2023, wse_2007, wse_2023)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-34-1.png)<!-- -->

``` r
xs_plot_rc2(depth_vs_discharge_2007, depth_vs_discharge_2023)
```

![](mecc_hydraulic_files/figure-gfm/unnamed-chunk-35-1.png)<!-- -->
