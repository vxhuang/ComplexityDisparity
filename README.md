
# Complex factors influencing US air pollution equity under a Net-Zero transition

This repository contains the R code and input data used in our study analyzing disparities in air pollution exposure across different racial/ethnic groups under net-zero decarbonization scenarios in the United States.

## Air Quality Modeling Framework

The modeling framework used in this study is based on WRF-Chem version 3.7.1, which was obtained from the official NCAR repository:
https://www2.mmm.ucar.edu/wrf/src/WRFV3-Chem-3.7.1.TAR.gz

Meteorological inputs were derived from the NCEP FNL Operational Model Global Tropospheric Analyses:
https://rda.ucar.edu/datasets/d083002/#

These were processed using the WRF Preprocessing System (WPS) version 3.7.1.

Emissions for the base year 2017 were based on the U.S. Environmental Protection Agency’s National Emissions Inventory (NEI 2017) and were processed using the Sparse Matrix Operator Kernel Emissions (SMOKE) modeling system to generate gridded, speciated, and temporally resolved emission inputs for WRF-Chem.

## Repository Structure

- `input_data/` — All input files used for the analysis (e.g., emissions, population, and spatial data)
- `rtr_disparity_numbers.R`, `rtr_disparity_numbers_ssp.R` — Scripts for calculating disparity metrics
- `rtr_fig2.R` to `rtr_fig6.R` — Scripts for generating manuscript figures
- `rtr_fig4_si.R`, `rtr_fig5_si_*.R` — Scripts for generating Supplementary Information (SI) figures
- `README.md` — This file

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
These versions were tested in our analysis. Newer versions may also work but are not guaranteed.

## Running the Code

To reproduce the results, run the scripts in the following general order:

1. **Disparity calculations**

   * `rtr_disparity_numbers.R`: Main disparity calculations (historical and 2050)
   * `rtr_disparity_numbers_ssp.R`: Alternate SSP scenario results

2. **Figure generation (main text)**

   * `rtr_fig2.R` → Figure 2
   * `rtr_fig3.R` → Figure 3
   * `rtr_fig4.R` → Figure 4
   * `rtr_fig5.R` → Figure 5
   * `rtr_fig6.R` → Figure 6

3. **Supplementary figures**

   * `rtr_fig4_si.R` → SI Figure 4
   * `rtr_fig5_si_ref2050.R`, `rtr_fig5_si_ssp3.R`, etc. → scenario-specific SI versions of Figure 5

Each script is self-contained and will read from the `input_data/` folder and write outputs (figures and processed results) to the working directory.

## Runtime

* Typical runtime: 10–30 minutes depending on system specs

## Reproducibility

All input data and R scripts are included to reproduce the key results and figures in the manuscript and supplementary materials.

## License

This repository is licensed under the [MIT License](https://opensource.org/licenses/MIT). You are free to reuse and adapt the code with appropriate attribution.


