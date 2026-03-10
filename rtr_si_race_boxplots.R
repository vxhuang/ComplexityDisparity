library(viridis)
library(tidycensus)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)
library(ggridges)
library(forcats)
library(ggpubr)
# set to local directory where the repo is stored
setwd(repo_directory)
data_dir <- c("./input_data/")
plot_dir <- c("./outputs/")

all_data_new <- read.csv("./input_data/combined_data.csv")
mydat <- all_data_new

scenario_cols <- c('ref2017', 
                   'ref2050_2017', 
                   'nz2050_2017', 
                   'nz_equity_2017',
                   'elec_2017')
scenario_cols <- unique(all_data_new$SCENARIO)

all_pm_new <- filter(all_data_new, TIMESCALE == 'annual', POLLUTANT == 'pm25',
                     SCENARIO %in% scenario_cols) %>%
  dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT) %>%
  pivot_wider(values_from = VALUE, names_from = SCENARIO)
all_oz_new <- filter(all_data_new, TIMESCALE == 'july', POLLUTANT == 'ozone',
                     SCENARIO %in% scenario_cols) %>%
  dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT) %>%
  pivot_wider(values_from = VALUE, names_from = SCENARIO)
head(all_pm_new)

data('fips_codes')
fips_v2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
  dplyr::select(FIPS, STNAME = state_name)
all_pm_new <- left_join(all_pm_new, fips_v2) %>%
  filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))

all_oz_new <- left_join(all_oz_new, fips_v2) %>%
  filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))

get_demo_df <- function(pollutant_df) {
  # urban-rural
  urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
  urban_pct$GEOID <- as.numeric(paste0(sprintf("%02d", urban_pct$STATE),
                                       sprintf("%03d", urban_pct$COUNTY)))
  urban_pct <- dplyr::select(urban_pct, GEOID, POPPCT_URB)
  
  # demography
  demo <- get_acs(variables = c(
    'B01003_001', 
    
    'B03002_003', 'B03002_004',
    'B03002_005', 'B03002_006', 'B03002_007',
    
    'B03001_003', 
    
    'B06011_001',
    
    'B19001_002', 'B19001_003', 'B19001_004', 'B19001_005', 'B19001_006', 'B19001_007',
    'B19001_008', 'B19001_009', 'B19001_010', 'B19001_011', 'B19001_012', 'B19001_013',
    'B19001_014', 'B19001_015', 'B19001_016', 'B19001_017'
    
  ), 
  year = 2017, geography = 'county')
  var_df <- data.frame(
    variable = c('B01003_001', 
                 
                 
                 'B03002_003', 'B03002_004',
                 'B03002_005', 'B03002_006', 'B03002_007',
                 
                 'B03001_003', 'B06011_001',
                 
                 
                 'B19001_002', 'B19001_003', 'B19001_004', 'B19001_005', 'B19001_006', 'B19001_007',
                 'B19001_008', 'B19001_009', 'B19001_010', 'B19001_011', 'B19001_012', 'B19001_013',
                 'B19001_014', 'B19001_015', 'B19001_016', 'B19001_017'
                 
    ),
    var_name = c('total', 'white', 'black', 'native_am', 'asian', 'pi', 'hispanic', 'income',
                 paste0('i', 2:17))
  )
  demo_df <- left_join(demo, var_df) %>%
    dplyr::select(-moe, -variable) %>%
    pivot_wider(names_from = var_name, values_from = estimate) %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    left_join(dplyr::select(pollutant_df, GEOID = FIPS, dplyr::all_of(scenario_cols))) %>%
    mutate(non_hispanic = total - hispanic) %>%
    mutate(race_cat = ifelse(black/total > 0.67, 'Black',
                             ifelse(white/total > 0.67, 'White', 'Mixed'))) %>% 
    left_join(urban_pct) %>%
    mutate(urban = total*POPPCT_URB, rural = total - urban)
  demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]
}

get_race_df <- function(demo_df) {
  race_wt_df <- demo_df %>%
    dplyr::select(GEOID, dplyr::all_of(scenario_cols),
                  white, black, native_am, asian, pi) %>%
    pivot_longer(cols = c('white', 'black', 'native_am', 'asian', 
                          'pi'), names_to = "race",
                 values_to = 'count') %>%
    pivot_longer(cols = scenario_cols, names_to = "scenario",
                 values_to = 'pm')
  race_wt_df <- mutate(race_wt_df, 
                       scenario = fct_relevel(scenario, scenario_cols))
  race_wt_df$race <- ifelse(race_wt_df$race == 'white', 'White',
                            ifelse(race_wt_df$race == 'black', 'Black',
                                   ifelse(race_wt_df$race == 'native_am', 'Native Am.',
                                          ifelse(race_wt_df$race == 'asian', 'Asian', 'P.I.'))))
  
  ##### Clump all non-black and non-white into "other"
  race_wt_df_2 <- race_wt_df
  return(race_wt_df_2)
}

get_race_df_ssp <- function(demo_df) {
  race_wt_df <- demo_df %>%
    dplyr::select(GEOID, dplyr::all_of(scenario_cols),
                  white, black, other) %>%
    pivot_longer(cols = c('white', 'black', 'other'), names_to = "race",
                 values_to = 'count') %>%
    pivot_longer(cols = scenario_cols, names_to = "scenario",
                 values_to = 'pm')
  race_wt_df <- mutate(race_wt_df, 
                       scenario = fct_relevel(scenario, scenario_cols))
  race_wt_df$race <- ifelse(race_wt_df$race == 'white', 'White', 
                            ifelse(race_wt_df$race == 'black', 'Black', race_wt_df$race))
  
  ##### Clump all non-black and non-white into "other"
  race_wt_df_2 <- race_wt_df
  race_wt_df_2$race <- ifelse(race_wt_df_2$race %in% c('White', 'Black'), race_wt_df_2$race, 'Other')
  return(race_wt_df_2)
}

get_income_df <- function(demo_df) {
  income_wt_df <- as.data.frame(demo_df) %>%
    mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
           high_income = (i16+i17)) %>%
    dplyr::select(GEOID, dplyr::all_of(scenario_cols), low_income, mid_income,
                  high_income) %>%
    pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
                 values_to = 'count') %>%
    pivot_longer(cols = scenario_cols, names_to = "scenario",
                 values_to = 'pm')
  income_wt_df <- mutate(income_wt_df, 
                         scenario = fct_relevel(scenario, scenario_cols))
  income_wt_df$income <- ifelse(income_wt_df$income == 'high_income', 'High Income',
                                ifelse(income_wt_df$income == 'mid_income', 'Middle Income', 'Low Income'))
  income_wt_df$income <- fct_relevel(income_wt_df$income, 'High Income', 'Middle Income', 'Low Income')
  income_wt_df <- filter(income_wt_df, income != 'Middle Income')
  return(income_wt_df)
}

get_urban_df <- function(demo_df) {
  urban_wt_df <- demo_df %>%
    dplyr::select(GEOID, dplyr::all_of(scenario_cols),
                  urban, rural) %>%
    pivot_longer(cols = c('urban', 'rural'), names_to = "Geography",
                 values_to = 'count') %>%
    pivot_longer(cols = scenario_cols, names_to = "scenario",
                 values_to = 'pm')
  urban_wt_df <- mutate(urban_wt_df, 
                        scenario = fct_relevel(scenario, scenario_cols))
  urban_wt_df$Geography <- ifelse(urban_wt_df$Geography == 'urban', 'Urban', 'Rural')
  return(urban_wt_df)
}

calc_freq_median <- function(values, frequencies) {
  # Check if inputs are valid
  if (length(values) != length(frequencies)) {
    stop("The length of values and frequencies must be the same.")
  }
  if (any(frequencies < 0)) {
    stop("Frequencies must be non-negative.")
  }
  
  # highly critical step of sorting to ensure cumsum picks the right
  # midpoint
  sort_df <- data.frame(values, frequencies) %>%
    arrange(values)
  values <- sort_df$values
  frequencies <- sort_df$frequencies
  
  # Calculate cumulative frequencies
  cumulative_freq <- cumsum(frequencies)
  total <- sum(frequencies)
  
  # Find the position of the median
  median_position <- (total + 1) / 2
  
  # Find the interval containing the median
  median_interval <- which(cumulative_freq >= median_position)[1]
  
  # Calculate the median
  if (median_interval > 1) {
    lower_bound <- values[median_interval - 1]
    freq_before <- cumulative_freq[median_interval - 1]
  } else {
    lower_bound <- values[1]
    freq_before <- 0
  }
  
  median_value <- lower_bound + 
    (median_position - freq_before) / frequencies[median_interval] * 
    (values[median_interval] - lower_bound)
  
  return(median_value)
}

get_disp_numbers <- function(input_df, df_type) {
  input_df <- dplyr::rename(input_df, strata = any_of(df_type))
  input_df2 <- input_df[complete.cases(input_df),]
  input_df2 <- input_df2 %>%
    group_by(strata, scenario) %>%
    summarize(median = calc_freq_median(values = pm, frequencies = count)) %>%
    ungroup()
  if(df_type == 'race') {
    input_df2 <- filter(input_df2, strata %in% c('Black', 'White'))
  }
  input_df2 <- input_df2 %>%
    group_by(scenario) %>%
    summarize(med_diff = abs(diff(median)))
  return(input_df2)
}

get_demo_df_ssp3 <- function(pollutant_df) {
  ssp3 <- read.csv("./input_data/demo_data_ssp3.csv")
  ssp3_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH) %>%
    mutate(other = total - black - white)
  ssp3_urban <- read.csv("./input_data/urban_pct_ssp3.csv")
  ssp3_urban_v2 <- mutate(ssp3_urban, GEOID = fips) %>%
    dplyr::select(-fips)
  demo_df <- left_join(ssp3_v2, ssp3_urban_v2) %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    left_join(dplyr::select(pollutant_df, GEOID = FIPS, dplyr::all_of(scenario_cols))) %>%
    mutate(urban = total*POPPCT_URB, rural = total - urban)
  return(demo_df)
}

get_demo_df_ssp5 <- function(pollutant_df) {
  ssp3 <- read.csv("./input_data/demo_data_ssp5.csv")
  ssp3_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH) %>%
    mutate(other = total - black - white)
  ssp3_urban <- read.csv('./input_data/urban_pct_ssp5.csv')
  ssp3_urban_v2 <- mutate(ssp3_urban, GEOID = fips) %>%
    dplyr::select(-fips)
  demo_df <- left_join(ssp3_v2, ssp3_urban_v2) %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    left_join(dplyr::select(pollutant_df, GEOID = FIPS, dplyr::all_of(scenario_cols))) %>%
    mutate(urban = total*POPPCT_URB, rural = total - urban)
  return(demo_df)
}

pol_df <- all_pm_new
demo_df <- get_demo_df(all_pm_new)
race_df_pm <- get_race_df(demo_df) %>% mutate(pol = 'PM2.5')
race_df_pm_ssp <- get_demo_df_ssp3(all_pm_new) %>%
  get_race_df_ssp() %>%
  filter(scenario == 'nz2050_2017') %>%
  mutate(scenario = 'SSP3', pol = 'PM2.5')

demo_df <- get_demo_df(all_oz_new)
race_df_oz <- get_race_df(demo_df) %>% mutate(pol = 'Ozone')
race_df_oz_ssp <- get_demo_df_ssp3(all_oz_new) %>%
  get_race_df_ssp() %>%
  filter(scenario == 'nz2050_2017') %>%
  mutate(scenario = 'SSP3', pol = 'Ozone')

race_df <- bind_rows(race_df_pm, race_df_oz)
race_df <- filter(race_df, 
                  !(scenario %in% c('nz_2016meteo', 'ref_2016meteo')))
race_df$scenario <- as.character(race_df$scenario)

SCEN <- c(
  "2017"               = "ref2017",
  "Reference 2050"           = "ref2050_2017",
  "Net-Zero 2050"            = "nz2050_2017",
  "Net-Zero 2050 (ELE)"        = "elec_2017",
  "Net-Zero 2050 (Equity)"     = "nz_equity_2017",
  "Net-Zero 2050 (2016 MET)" = "nz_2016meteo",
  "2017 (2016 MET)"    = "ref_2016meteo",
  "Net-Zero 2050 (SSP3)" = 'SSP3'
)
scen_labels <- setNames(names(SCEN), SCEN)
race_df$scenario2 <- scen_labels[race_df$scenario]
race_df_medians <- race_df %>%
  group_by(pol, race, scenario2) %>%
  summarize(median = calc_freq_median(values = pm, frequencies = count)) %>%
  ungroup()
race_df$scenario2 <- fct_relevel(race_df$scenario2,
                                 c('2017', 'Reference 2050', 'Net-Zero 2050',
                                   'Net-Zero 2050 (ELE)', 'Net-Zero 2050 (Equity)'))
race_df_medians$scenario2 <- fct_relevel(race_df_medians$scenario2,
                                 c('2017', 'Reference 2050', 'Net-Zero 2050',
                                   'Net-Zero 2050 (ELE)', 'Net-Zero 2050 (Equity)'))


levels(race_df$scenario2)
ggplot(filter(race_df, pol == 'PM2.5')) +
  geom_boxplot(aes(x = race, y = pm, weight = count), outliers = FALSE) +
  geom_text(data = filter(race_df_medians, pol == 'PM2.5'),
            aes(x = race, y = 0.25, label = round(median, 1))) +
  theme_bw() +
  xlab('') + ylab(bquote(μg/m^3)) +
  scale_color_manual(name = 'Race', values = c('#66a61e', '#e6ab02', '#1f78b4')) +
  facet_wrap(scenario2 ~ .) 


ggplot(filter(race_df, pol == 'Ozone')) +
  geom_boxplot(aes(x = race, y = pm, weight = count), outliers = FALSE) +
  geom_text(data = filter(race_df_medians, pol == 'Ozone'),
            aes(x = race, y = 15, label = round(median, 1))) +
  theme_bw() +
  xlab('') + ylab('ppb') +
  scale_color_manual(name = 'Race', values = c('#66a61e', '#e6ab02', '#1f78b4')) +
  scale_linetype_manual(values = c("Main" = "solid", "Sensitivity" = "twodash"), name = '') +
  facet_wrap(scenario2 ~ .) 

