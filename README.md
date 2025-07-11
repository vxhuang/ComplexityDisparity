````markdown
# Complex factors influencing US air pollution equity under a Net-Zero transition

This repository contains the R code and input data used in our study analyzing disparities in air pollution exposure across different racial/ethnic groups under net-zero decarbonization scenarios in the United States.

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

## R Package Dependencies

Install required packages using:

```r
install.packages(c(
  "tidyverse", "viridis", "tidycensus", "raster", "sf", "exactextractr", 
  "ggridges", "forcats", "ggpubr", "readxl", "gridExtra", 
  "ggtext", "scales", "ggnewscale"
))
````

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

## 🔁 Reproducibility

All input data and R scripts are included to reproduce the key results and figures in the manuscript and supplementary materials.

## 📜 License

This repository is licensed under the [MIT License](https://opensource.org/licenses/MIT). You are free to reuse and adapt the code with appropriate attribution.



