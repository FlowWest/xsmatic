# MECC Hydraulic Analysis

**[`xs.R`](R/xs.R)** contains generic functions for conducting Manning's equation hydraulic analyses based on surveyed cross section data:

* Clean up station-elevation table and densify coordinate points

* Calculate hydraulic geometry (cross-sectional area, wetted perimeter) for a given water surface elevation.

* Using Manning's *n* and a provided slope and roughness, calculate a rating curve of water surface elevation versus cross-sectional area, wetted perimeter, velocity, and discharge.

* Use a rating curve to return a water surface elevation for a given discharge value.

**[`mecc_hydraulic.Rmd`](mecc_hydraulic.Rmd)** is the source code for the specific use in the MECC hydraulic report. 

**[`mecc_hydraulic.md`](mecc_hydraulic.md)** contains the outputs as incorporated into the MECC hydraulic report. 
