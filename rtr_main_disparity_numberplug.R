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

get_wei_plot_race <- function(demo, poll, base_scenario, comparison_scenario) {
  demo_df <- demo
  st_pct_black <- demo_df %>%
    group_by(STNAME) %>%
    summarize(black = sum(black, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
    mutate(pct_black = 100*(black/total))
  
  race_wt_df <- demo_df %>%
    dplyr::select(GEOID, STNAME, ref2017, ref2050_2017, nz2050_2017, elec_2017, nz_equity_2017, nz_2016meteo, 
                  ref_2016meteo,
                  white, black, total) %>%
    pivot_longer(cols = c('white', 'black', 'total'), names_to = "race",
                 values_to = 'count') %>%
    pivot_longer(cols = c('ref2017', 'ref2050_2017', 'nz2050_2017', 'elec_2017', 'nz_equity_2017', 'nz_2016meteo', 
                          'ref_2016meteo'), names_to = "scenario",
                 values_to = 'pm')
  race_wt_df$race <- ifelse(race_wt_df$race == 'white', 'White',
                            ifelse(race_wt_df$race == 'black', 'Black', 'Total'))
  race_wt_df$race <- fct_relevel(race_wt_df$race, 'Black', 'White', 'Total')
  race_wt_df <- race_wt_df %>%
    group_by(race, STNAME, scenario) %>%
    summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
    filter(race != 'Total') %>%
    pivot_wider(values_from = pm, names_from = race) %>%
    mutate(disparity = (Black-White))
  race_wt_df$higher <- ifelse(race_wt_df$Black > race_wt_df$White, 'Black', 'White')
  wei_plot_df <- race_wt_df
  wei_plot_df <- mutate(wei_plot_df, scenario_higher = paste0(scenario, '_higher'))
  wei_plot_df <- dplyr::select(wei_plot_df, STNAME, scenario, disparity, higher, scenario_higher) %>%
    pivot_wider(names_from = 'scenario', values_from = 'disparity') %>%
    pivot_wider(names_from = 'scenario_higher', values_from = 'higher') %>%
    ungroup() %>%
    summarize(`ref2017` = first(na.omit(`ref2017`)),
              `ref2050_2017` = first(na.omit(`ref2050_2017`)),
              `nz2050_2017` = first(na.omit(`nz2050_2017`)),
              `elec_2017` = first(na.omit(`elec_2017`)),
              `nz_equity_2017` = first(na.omit(`nz_equity_2017`)),
              `nz_2016meteo` = first(na.omit(`nz_2016meteo`)),
              `ref_2016meteo` = first(na.omit(`ref_2016meteo`)),
              
              `ref2017_higher` = first(na.omit(`ref2017_higher`)),
              `ref2050_2017_higher` = first(na.omit(`ref2050_2017_higher`)),
              `nz2050_2017_higher` = first(na.omit(`nz2050_2017_higher`)),
              `elec_2017_higher` = first(na.omit(`elec_2017_higher`)),
              `nz_equity_2017_higher` = first(na.omit(`nz_equity_2017_higher`)),
              `nz_2016meteo_higher` = first(na.omit(`nz_2016meteo_higher`)),
              `ref_2016meteo_higher` = first(na.omit(`ref_2016meteo_higher`)),
              .by = STNAME)
  
  wei_plot_df[['base']] <- wei_plot_df[[base_scenario]]
  wei_plot_df[['comparison']] <- wei_plot_df[[comparison_scenario]]
  
  wei_plot_df$direction <- ifelse(abs(wei_plot_df$base) > abs(wei_plot_df$comparison),
                                  'decreasing', 'increasing')
  
  return((wei_plot_df))
}

get_wei_plot_income <- function(demo, poll, base_scenario, comparison_scenario) {
  demo_df <- demo
  st_pct_lowincome <- demo_df %>%
    mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
           high_income = (i16+i17)) %>%
    group_by(STNAME) %>%
    summarize(low_income = sum(low_income, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
    mutate(pct_low_income = 100*(low_income/total))
  
  income_wt_df <- as.data.frame(demo_df) %>%
    mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
           high_income = (i16+i17)) %>%
    dplyr::select(GEOID, STNAME, ref2017, ref2050_2017, nz2050_2017, elec_2017, nz_equity_2017, nz_2016meteo, 
                  ref_2016meteo,
                  low_income, mid_income,
                  high_income) %>%
    pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
                 values_to = 'count') %>%
    pivot_longer(cols = c('ref2017', 'ref2050_2017', 'nz2050_2017', 'elec_2017', 'nz_equity_2017', 'nz_2016meteo', 
                          'ref_2016meteo'), names_to = "scenario",
                 values_to = 'pm')
  income_wt_df$income <- ifelse(income_wt_df$income == 'high_income', 'High Income',
                                ifelse(income_wt_df$income == 'mid_income', 'Middle Income', 'Low Income'))
  income_wt_df$income <- fct_relevel(income_wt_df$income, 'High Income', 'Middle Income', 'Low Income')
  income_wt_df <- filter(income_wt_df, income != 'Middle Income')
  income_wt_df <- income_wt_df %>%
    group_by(income, STNAME, scenario) %>%
    summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
    pivot_wider(values_from = pm, names_from = income) %>%
    mutate(disparity = (`Low Income` - `High Income`))
  income_wt_df$higher <- ifelse(income_wt_df$`Low Income` > income_wt_df$`High Income`, 'Low Income', 'High Income')
  wei_plot_df <- income_wt_df
  
  wei_plot_df <- mutate(wei_plot_df, scenario_higher = paste0(scenario, '_higher'))
  wei_plot_df <- dplyr::select(wei_plot_df, STNAME, scenario, disparity, higher, scenario_higher) %>%
    pivot_wider(names_from = 'scenario', values_from = 'disparity') %>%
    pivot_wider(names_from = 'scenario_higher', values_from = 'higher') %>%
    ungroup() %>%
    summarize(`ref2017` = first(na.omit(`ref2017`)),
              `ref2050_2017` = first(na.omit(`ref2050_2017`)),
              `nz2050_2017` = first(na.omit(`nz2050_2017`)),
              `elec_2017` = first(na.omit(`elec_2017`)),
              `nz_equity_2017` = first(na.omit(`nz_equity_2017`)),
              `nz_2016meteo` = first(na.omit(`nz_2016meteo`)),
              `ref_2016meteo` = first(na.omit(`ref_2016meteo`)),
              
              `ref2017_higher` = first(na.omit(`ref2017_higher`)),
              `ref2050_2017_higher` = first(na.omit(`ref2050_2017_higher`)),
              `nz2050_2017_higher` = first(na.omit(`nz2050_2017_higher`)),
              `elec_2017_higher` = first(na.omit(`elec_2017_higher`)),
              `nz_equity_2017_higher` = first(na.omit(`nz_equity_2017_higher`)),
              `nz_2016meteo_higher` = first(na.omit(`nz_2016meteo_higher`)),
              `ref_2016meteo_higher` = first(na.omit(`ref_2016meteo_higher`)),
              .by = STNAME)
  
  wei_plot_df[['base']] <- wei_plot_df[[base_scenario]]
  wei_plot_df[['comparison']] <- wei_plot_df[[comparison_scenario]]
  
  wei_plot_df$direction <- ifelse(abs(wei_plot_df$base) > abs(wei_plot_df$comparison),
                                  'decreasing', 'increasing')
  
  return((wei_plot_df))
}

get_wei_plot_urban <- function(demo, poll, base_scenario, comparison_scenario) {
  demo_df <- demo
  st_pct_rural <- demo_df %>%
    group_by(STNAME) %>%
    summarize(rural = sum(rural, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
    mutate(pct_rural = 100*(rural/total))
  
  urban_wt_df <- demo_df %>%
    dplyr::select(GEOID, STNAME, 
                  ref2017, ref2050_2017, nz2050_2017, elec_2017, nz_equity_2017, nz_2016meteo, 
                  ref_2016meteo,
                  urban, rural, total) %>%
    pivot_longer(cols = c('urban', 'rural', 'total'), names_to = "urban",
                 values_to = 'count') %>%
    pivot_longer(cols = c('ref2017', 'ref2050_2017', 'nz2050_2017', 'elec_2017', 'nz_equity_2017', 'nz_2016meteo', 
                          'ref_2016meteo'), names_to = "scenario",
                 values_to = 'pm')
  urban_wt_df$urban <- ifelse(urban_wt_df$urban == 'urban', 'Urban',
                              ifelse(urban_wt_df$urban == 'rural', 'Rural', 'Total'))
  urban_wt_df$urban <- fct_relevel(urban_wt_df$urban, 'Rural', 'Urban', 'Total')
  urban_wt_df <- urban_wt_df %>%
    group_by(urban, STNAME, scenario) %>%
    summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
    filter(urban != 'Total') %>%
    pivot_wider(values_from = pm, names_from = urban) %>%
    mutate(disparity = Rural-Urban)
  urban_wt_df$higher <- ifelse(urban_wt_df$Rural > urban_wt_df$Urban, 'Rural', 'Urban')
  
  wei_plot_df <- urban_wt_df
  wei_plot_df <- mutate(wei_plot_df, scenario_higher = paste0(scenario, '_higher'))
  wei_plot_df <- dplyr::select(wei_plot_df, STNAME, scenario, disparity, higher, scenario_higher) %>%
    pivot_wider(names_from = 'scenario', values_from = 'disparity') %>%
    pivot_wider(names_from = 'scenario_higher', values_from = 'higher') %>%
    ungroup() %>%
    summarize(`ref2017` = first(na.omit(`ref2017`)),
              `ref2050_2017` = first(na.omit(`ref2050_2017`)),
              `nz2050_2017` = first(na.omit(`nz2050_2017`)),
              `elec_2017` = first(na.omit(`elec_2017`)),
              `nz_equity_2017` = first(na.omit(`nz_equity_2017`)),
              `nz_2016meteo` = first(na.omit(`nz_2016meteo`)),
              `ref_2016meteo` = first(na.omit(`ref_2016meteo`)),
              
              `ref2017_higher` = first(na.omit(`ref2017_higher`)),
              `ref2050_2017_higher` = first(na.omit(`ref2050_2017_higher`)),
              `nz2050_2017_higher` = first(na.omit(`nz2050_2017_higher`)),
              `elec_2017_higher` = first(na.omit(`elec_2017_higher`)),
              `nz_equity_2017_higher` = first(na.omit(`nz_equity_2017_higher`)),
              `nz_2016meteo_higher` = first(na.omit(`nz_2016meteo_higher`)),
              `ref_2016meteo_higher` = first(na.omit(`ref_2016meteo_higher`)),
              .by = STNAME)
  
  wei_plot_df[['base']] <- wei_plot_df[[base_scenario]]
  wei_plot_df[['comparison']] <- wei_plot_df[[comparison_scenario]]
  
  wei_plot_df$direction <- ifelse(abs(wei_plot_df$base) > abs(wei_plot_df$comparison),
                                  'decreasing', 'increasing')
  return((wei_plot_df))
}

get_demo_df <- function() {
  
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
  return(list('demo' = demo, 'var_df' = var_df))
}

process_poll_df <- function(demo_list, pollutant_df) {
  
  demo <- demo_list[['demo']]
  var_df <- demo_list[['var_df']]
  urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
  urban_pct$GEOID <- as.numeric(paste0(sprintf("%02d", urban_pct$STATE),
                                       sprintf("%03d", urban_pct$COUNTY)))
  urban_pct <- dplyr::select(urban_pct, GEOID, POPPCT_URB)
  demo_df <- left_join(demo, var_df) %>%
    dplyr::select(-moe, -variable) %>%
    pivot_wider(names_from = var_name, values_from = estimate) %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    left_join(dplyr::select(pollutant_df, GEOID = FIPS, STNAME, ref2017, ref2050_2017, nz2050_2017, elec_2017, nz_equity_2017, nz_2016meteo, 
                            ref_2016meteo)) %>%
    mutate(non_hispanic = total - hispanic) %>%
    mutate(race_cat = ifelse(black/total > 0.67, 'Black',
                             ifelse(white/total > 0.67, 'White', 'Mixed'))) %>% 
    left_join(urban_pct) %>%
    mutate(urban = total*POPPCT_URB, rural = total - urban)
  demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]
  return(demo_df)
}

data_dir <- c("./input_data/")
plot_dir <- c("./outputs/")
all_data_new <- read.csv("./input_data/combined_data.csv")
mydat <- all_data_new
all_pm_new <- filter(all_data_new, TIMESCALE == 'annual', POLLUTANT == 'pm25',
                     SCENARIO %in% c('ref2017', 
                                     'ref2050_2017', 
                                     'nz2050_2017', 
                                     'elec_2017', 
                                     'nz_equity_2017', 
                                     'nz_2016meteo', 
                                     'ref_2016meteo')) %>%
  dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT) %>%
  pivot_wider(values_from = VALUE, names_from = SCENARIO)
all_oz_new <- filter(all_data_new, TIMESCALE == 'july', POLLUTANT == 'ozone',
                     SCENARIO %in% c('ref2017', 
                                     'ref2050_2017', 
                                     'nz2050_2017', 
                                     'elec_2017', 
                                     'nz_equity_2017', 
                                     'nz_2016meteo', 
                                     'ref_2016meteo')) %>%
  dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT) %>%
  pivot_wider(values_from = VALUE, names_from = SCENARIO)
data('fips_codes')
fips_v2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
  dplyr::select(FIPS, STNAME = state_name)
all_pm_new <- left_join(all_pm_new, fips_v2) %>%
  filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))
all_oz_new <- left_join(all_oz_new, fips_v2) %>%
  filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))

demo_list <- get_demo_df()
oz_demo <- process_poll_df(demo_list = demo_list, pollutant_df = all_oz_new)
pm_demo <- process_poll_df(demo_list = demo_list, pollutant_df = all_pm_new)

results_df_oz <- get_wei_plot_urban(oz_demo, poll = 'Ozone', 
                  base_scenario = 'ref2017', 
                  comparison_scenario = 'nz2050_2017')

results_df_pm <- get_wei_plot_urban(pm_demo, poll = 'PM2.5', 
                                   base_scenario = 'ref2017', 
                                   comparison_scenario = 'nz2050_2017')

filter(results_df_oz, STNAME == 'New Jersey') %>%
  mutate(test = abs(ref2017) - abs(nz2050_2017)) %>%
  glimpse()

filter(results_df_pm, STNAME == 'New Jersey') %>%
  mutate(test = abs(ref2017) - abs(nz2050_2017)) %>%
  glimpse()

pm_states <- results_df_pm %>%
  mutate(ref2017 = abs(ref2017), 
         nz2050_2017 = abs(nz2050_2017),
         mz) %>%
  filter(ref2017 > nz2050_2017)

oz_states <- results_df_oz %>%
  mutate(ref2017 = abs(ref2017), nz2050_2017 = abs(nz2050_2017)) %>%
  filter(ref2017 > nz2050_2017)
pm_states$STNAME
oz_states$STNAME
oz_states$STNAME[oz_states$STNAME %in% pm_states$STNAME]


filter(results_df, direction == 'decreasing')
