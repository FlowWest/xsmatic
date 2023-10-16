MECC Hydraulic Analysis
================

**[`mecc_hydraulic.Rmd`](mecc_hydraulic.Rmd)** is the source code for
the specific use in the MECC hydraulic report.

**[`mecc_hydraulic.md`](mecc_hydraulic.md)** contains the outputs as
incorporated into the MECC hydraulic report.

**[`xs.R`](R/xs.R)** contains generic functions for conducting Manning’s
equation hydraulic analyses based on surveyed cross section data:

- Clean up station-elevation table and densify coordinate points

- Calculate hydraulic geometry (cross-sectional area, wetted perimeter)
  for a given water surface elevation.

- Using Manning’s *n* and a provided slope and roughness, calculate a
  rating curve of water surface elevation versus cross-sectional area,
  wetted perimeter, velocity, and discharge.

- Use a rating curve to return a water surface elevation for a given
  discharge value.

These functions can now be accessed via the **`xsmatic`** package.

``` r
# authenticate with a github account within the FlowWest organization
usethis::use_git_config(user.name = "yourGithubUsername", user.email = "username@flowwest.com")
usethis::create_github_token() # copy the personal access token 
credentials::set_github_pat() # enter the personal access token into the prompt

# install private package
remotes::install_github("flowwest/mecc-hydraulic-analysis")
```

Load the package:

``` r
library(tidyverse)
library(xsmatic)
```

Run via the interactive prompt:

``` r
xs_run_app()
```

Import cross sections:

``` r
xs_2007 <- read_csv("data-raw/xs_2007.csv") %>%
  xs_prep(sta = station_ft, elev = elevation_ft) 
xs_2023 <- read_csv("data-raw/xs_2023_simplified.csv") %>%
  xs_prep(sta = station_ft, elev = elevation_ft) 

xs_plot2(xs_2007, xs_2023)
```

![](README_files/figure-gfm/xs-1.png)<!-- -->

Calculate rating curves:

``` r
rc_2007 <- xs_2007 %>% 
  xs_rating_curve(slope = 0.021, mannings_n = 0.035)
rc_2023 <- xs_2023 %>% 
  xs_rating_curve(slope = 0.021, mannings_n = 0.035)

xs_plot_rc2(rc_2007, rc_2023)
```

![](README_files/figure-gfm/rc-1.png)<!-- -->

Calculate water surface elevation at given discharge

``` r
q100 <- 3980 # cfs
wse_2007 <- rc_2007 %>% xs_rc_interpolate(q100)
wse_2023 <- rc_2023 %>% xs_rc_interpolate(q100)

xs_plot2(xs_2007, xs_2023, wse_2007, wse_2023)
```

![](README_files/figure-gfm/wse-1.png)<!-- -->

Calculate hydraulic parameters at multiple discharges

``` r
xs_eval_all(xs = xs_2023, rc = rc_2023, 
            discharges = c("Q2"=513, "Q5"=1320, "Q10"=1970, "Q25"=2780, "Q50"=3360, "Q100"=3980))
```

| name | discharge | thalweg_elevation | water_surface_elevation | max_depth | cross_sectional_area | wetted_perimeter | velocity |
|:-----|----------:|------------------:|------------------------:|----------:|---------------------:|-----------------:|---------:|
| Q2   |       513 |             36.99 |                   41.24 |      4.25 |             47.22972 |         20.87879 | 10.86181 |
| Q5   |      1320 |             36.99 |                   43.44 |      6.45 |             95.12741 |         28.59194 | 13.87613 |
| Q10  |      1970 |             36.99 |                   44.74 |      7.75 |            128.97752 |         32.37290 | 15.27398 |
| Q25  |      2780 |             36.99 |                   46.04 |      9.05 |            165.48131 |         36.03294 | 16.79948 |
| Q50  |      3360 |             36.99 |                   46.84 |      9.85 |            189.26527 |         38.38498 | 17.75286 |
| Q100 |      3980 |             36.99 |                   47.64 |     10.65 |            214.05067 |         40.61888 | 18.59373 |
