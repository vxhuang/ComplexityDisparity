
# Complex factors influencing US air pollution equity under a Net-Zero transition

This repository contains the R code and input data used in our study analyzing disparities in air pollution exposure across different racial/ethnic groups under net-zero decarbonization scenarios in the United States.

## Air Quality Modeling Framework

The modeling framework used in this study is based on the Community Multiscale Air Quality (CMAQ) model version 5.5 (https://www.epa.gov/cmaq). Simulations were conducted over the contiguous United States at 12 km horizontal resolution for 2017 and 2050 under Reference and Net-Zero scenarios.

Meteorological inputs were obtained from the EPA EQUATES WRF dataset (https://www.epa.gov/cmaq/equates), and boundary conditions were derived from the 108-km Northern Hemisphere CMAQ domain.

Emissions for the base year 2017 were based on the U.S. Environmental Protection Agency’s National Emissions Inventory (NEI 2017: https://www.epa.gov/air-emissions-inventories/2017-national-emissions-inventory-nei-data). Biogenic emissions were calculated using the in-line Biogenic Emission Inventory System (BEIS). Future emissions were mapped from GCAM-USA sectors to CMAQ emission streams. The CB6r3 gas-phase mechanism and AERO7 aerosol module were used in all simulations.

## Repository Structure

0. `input_data/` — All input files used for the analysis (e.g., emissions, population, and spatial data)
1. `rtr_disparity_consistency_numberplup.R`: code to generate results for main text discussing how disparity changed across scenarios
2. `rtr_disparity_numberplug.R`: code to generate results for main text discussing state level changes in disparities
3. `rtr_disparity_numbers_ssp.R`: code to generate results for state level disparity using SSP3 and SSP5 demographics
4. `rtr_disparity_numbers.R` code to generate results for state level disparity
5. `rtr_fig2.R`: code to generate main text figure 2 (national and state level emissions)
5. `rtr_fig3_maps.R`: code to generate main text figure 3 (maps)
6. `rtr_fig4_boxplots.R`: code to generate main text figure 4 (boxplots)
7. `rtr_boxplots_si.R`: code to generate SI boxplot figure (boxplots)
8. `rtr_fig5_arrow.R`: code to generate main text figure 5 (state level arrow diagram)
9. `rtr_arrow_si_v2.R`: code to generate SI state-level arrow diagram figures
10. `rtr_si_race_distro.R`: code to generate SI figure showing the distribution of population across racial groups
11. `rtr_state_change_table.R`: code to generate supplement data sheet
12. `rtr_state_maps.R`: code to generate state-specific pollutant distribution maps for the supplement
13. `rtr_fig6.R`: code to generate main text figure 6 (consistency across scenarios)

## System Requirements

- **Operating system**: macOS or Linux (tested on Ventura 13.6 and Ubuntu 22.04)
- **R version**: ≥ 4.2.0
- **Memory**: ≥ 8 GB RAM recommended due to use of raster operations
- **Typical installation time**: ~5–10 minutes for package setup on a standard desktop computer with a stable internet connection

To install R, visit: [https://cran.r-project.org/](https://cran.r-project.org/) and download the version appropriate for your operating system.

## R Package Dependencies

Install required packages and their tested versions using:

```r
install.packages(c(
  "tidyverse",        # 2.0.0
  "viridis",          # 0.6.4
  "tidycensus",       # 1.4.3
  "raster",           # 3.6-26
  "sf",               # 1.0-14
  "exactextractr",    # 0.9.1
  "ggridges",         # 0.5.4
  "forcats",          # 1.0.0
  "ggpubr",           # 0.6.0
  "readxl",           # 1.4.3
  "gridExtra",        # 2.3
  "ggtext",           # 0.1.2
  "scales",           # 1.3.0
  "ggnewscale"        # 0.4.9
))
````
These versions were tested in our analysis. Newer versions may also work but are not guaranteed.

## Running the Code

To reproduce the results, run the scripts in the following general order:

1. **Disparity calculations**

   * `rtr_disparity_numbers.R`: Main disparity calculations (historical and 2050)
   * `rtr_disparity_numbers_ssp.R`: Alternate SSP scenario results

2. **Figure generation (main text)**

   * `rtr_fig2.R` → Figure 2
   * `rtr_fig3_maps.R` → Figure 3
   * `rtr_fig4_boxplots.R` → Figure 4
   * `rtr_fig5_arrow.R` → Figure 5
   * `rtr_fig6.R` → Figure 6

3. **Supplementary figures**

   * `rtr_boxplots_si.R` → SI Boxplot figures
   * `rtr_arrow_si_v2.R` → SI state-level arrow diagram figures
   * `rtr_si_race_distro.R` → SI distributions of population across racial groups

Each script is self-contained and will read from the `input_data/` folder and write outputs (figures and processed results) to the working directory.

## Runtime

* Typical runtime: 10–30 minutes depending on system specs

## Reproducibility

All input data and R scripts are included to reproduce the key results and figures in the manuscript and supplementary materials.

## License

This repository is licensed under the [MIT License](https://opensource.org/licenses/MIT). You are free to reuse and adapt the code with appropriate attribution.


