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

get_race_disp_df <- function(demo, ssp3_df, poll, base_scenario, comparison_scenario) {
  demo_df <- demo
  st_pct_black <- demo_df %>%
    group_by(STNAME) %>%
    summarize(black = sum(black, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
    mutate(pct_black = 100*(black/total))
  
  race_wt_df <- demo_df %>%
    dplyr::select(GEOID, STNAME, scenario = SCENARIO, pm = VALUE,
                  white, black, total) %>%
    pivot_longer(cols = c('white', 'black', 'total'), names_to = "race",
                 values_to = 'count')
  race_wt_df$race <- ifelse(race_wt_df$race == 'white', 'White',
                            ifelse(race_wt_df$race == 'black', 'Black', 'Total'))
  race_wt_df$race <- fct_relevel(race_wt_df$race, 'Black', 'White', 'Total')
  race_wt_df <- race_wt_df %>%
    group_by(race, STNAME, scenario) %>%
    summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
    filter(race != 'Total') %>%
    pivot_wider(values_from = pm, names_from = race) %>%
    mutate(disparity = (Black-White))
  wei_plot_df <- race_wt_df
  wei_plot_df <- dplyr::select(wei_plot_df, STNAME, scenario, disparity) %>%
    pivot_wider(names_from = 'scenario', values_from = 'disparity')
  wei_plot_df[['base']] <- wei_plot_df[[base_scenario]]
  wei_plot_df[['comparison']] <- wei_plot_df[[comparison_scenario]]
  
  wei_plot_df$direction <- ifelse(abs(wei_plot_df$base) > abs(wei_plot_df$comparison),
                                  'decreasing', 'increasing')
  
  SCEN <- c(
    "2017"               = "ref2017",
    "Reference 2050"           = "ref2050_2017",
    "Net-Zero 2050"            = "nz2050_2017",
    "Net-Zero 2050 (ELE)"        = "elec_2017",
    "Net-Zero 2050 (Equity)"     = "nz_equity_2017",
    "NZ 2050 (2016 met)" = "nz_2016meteo",
    "2017 (2016 met)"    = "ref_2016meteo",
    "NZ 2050 (SSP3)" = 'SSP3'
  )
  scen_labels <- setNames(names(SCEN), SCEN)

  gg_wei2 <- ggplot(wei_plot_df) +
    geom_vline(xintercept = 0) +
    geom_segment(aes(x = base, xend = comparison, y = fct_reorder(STNAME, base),
                     col = direction), 
                 arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
    geom_point(aes(x = base, y = fct_reorder(STNAME, base)), size = 2) +
    theme_bw() +
    scale_color_manual(values = c('blue', 'red'))
  
  xlab_text <- paste0('Black - White Disparity in ', poll, ' Concentration')
  
  if (poll == 'Ozone') {
    gg_wei2 <- gg_wei2 +
      ggtitle("b) Ozone exposure disparity: Black - White") +
      xlab('ppb') +
      ylab('') +
      theme(legend.position = 'none')
    
  } else {
    gg_wei2 <- gg_wei2 +
      ggtitle("a) PM2.5 exposure disparity: Black - White") +
      xlab(bquote(μg/m^3)) +
      ylab('') +
      theme(legend.position = 'none')  +
      # Custom annotation: Point symbol + label
      annotate("rect", xmin = 1, xmax = 3.25, ymin = 1, ymax = 7, fill = 'white', color = 'black') +
      
      annotate("point", x = 1.1, y = 6, size = 3) +
      annotate("text", x = 1.3, y = 6, hjust = 0, label = scen_labels[base_scenario], size = 4) +
      
      # Custom annotation: Arrow symbol + label
      annotate("segment", x = 1.1, y = 4, xend = 1.2, yend = 4,
               arrow = arrow(type = 'closed', length = unit(0.015, 'npc')), col = 'red') +
      annotate("text", x = 1.3, y = 4, hjust = 0, label = paste0(scen_labels[comparison_scenario],
                                                                 ', (Disparity Increases)'),
               size = 4) + 
      
      annotate("segment", x = 1.1, y = 2, xend = 1.2, yend = 2,
               arrow = arrow(type = 'closed',  length = unit(0.015, 'npc')), col = 'blue') +
      annotate("text", x = 1.3, y = 2, hjust = 0, label = paste0(scen_labels[comparison_scenario],
                                                                 ', (Disparity Decreases)'),
               size = 4)  
    
  }
  
  return(list(gg_wei2))
}

get_income_disp_df <- function(demo, poll, base_scenario, comparison_scenario) {
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
    dplyr::select(GEOID, STNAME, scenario = SCENARIO, pm = VALUE,
                  low_income, mid_income,
                  high_income) %>%
    pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
                 values_to = 'count')
  income_wt_df$income <- ifelse(income_wt_df$income == 'high_income', 'High Income',
                                ifelse(income_wt_df$income == 'mid_income', 'Middle Income', 'Low Income'))
  income_wt_df$income <- fct_relevel(income_wt_df$income, 'High Income', 'Middle Income', 'Low Income')
  income_wt_df <- filter(income_wt_df, income != 'Middle Income')
  income_wt_df <- income_wt_df %>%
    group_by(income, STNAME, scenario) %>%
    summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
    pivot_wider(values_from = pm, names_from = income) %>%
    mutate(disparity = (`Low Income` - `High Income`))
  wei_plot_df <- income_wt_df
  wei_plot_df <- dplyr::select(wei_plot_df, STNAME, scenario, disparity) %>%
    pivot_wider(names_from = 'scenario', values_from = 'disparity')
  wei_plot_df[['base']] <- wei_plot_df[[base_scenario]]
  wei_plot_df[['comparison']] <- wei_plot_df[[comparison_scenario]]
  
  wei_plot_df$direction <- ifelse(abs(wei_plot_df$base) > abs(wei_plot_df$comparison),
                                  'decreasing', 'increasing')
  
  SCEN <- c(
    "2017"               = "ref2017",
    "Reference 2050"           = "ref2050_2017",
    "Net-Zero 2050"            = "nz2050_2017",
    "Net-Zero 2050 (ELE)"        = "elec_2017",
    "Net-Zero 2050 (Equity)"     = "nz_equity_2017",
    "NZ 2050 (2016 met)" = "nz_2016meteo",
    "2017 (2016 met)"    = "ref_2016meteo",
    "Net-Zero 2050 (SSP3)" = 'SSP3'
  )
  scen_labels <- setNames(names(SCEN), SCEN)
  gg_wei2 <- ggplot(wei_plot_df) +
    geom_vline(xintercept = 0) +
    geom_segment(aes(x = base, xend = comparison, y = fct_reorder(STNAME, base),
                     col = direction), 
                 arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
    geom_point(aes(x = base, y = fct_reorder(STNAME, base)), size = 2) +
    theme_bw() +
    scale_color_manual(values = c('blue', 'red'))
  
  
  xlab_text <- paste0('Low Income - High Income Disparity in ', poll, ' Concentration')
  SCEN <- c(
    "2017"               = "ref2017",
    "Ref 2050"           = "ref2050_2017",
    "NZ 2050"            = "nz2050_2017",
    "NZ 2050 ELE"        = "elec_2017",
    "NZ 2050 Equity"     = "nz_equity_2017",
    "NZ 2050 (2016 met)" = "nz_2016meteo",
    "2017 (2016 met)"    = "ref_2016meteo"
  )
  scen_labels <- setNames(names(SCEN), SCEN)
  
  if (poll == 'Ozone') {
    gg_wei2 <- gg_wei2 +
      ggtitle("d) Ozone exposure disparity: Low Income - High Income") +
      xlab('ppb') +
      ylab('') +
      theme(legend.position = 'none')
  } else {
    gg_wei2 <- gg_wei2 +
      ggtitle("c) PM2.5 exposure disparity: Low Income - High Income") +
      xlab(bquote(μg/m^3)) +
      ylab('') +
      theme(legend.position = 'none')
    
  }
  
  return(list(gg_wei2))
}

get_urban_disp_df <- function(demo, poll, base_scenario, comparison_scenario) {
  demo_df <- demo
  st_pct_rural <- demo_df %>%
    group_by(STNAME) %>%
    summarize(rural = sum(rural, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
    mutate(pct_rural = 100*(rural/total))
  
  urban_wt_df <- demo_df %>%
    dplyr::select(GEOID, STNAME, , scenario = SCENARIO, pm = VALUE,
                  urban, rural, total) %>%
    pivot_longer(cols = c('urban', 'rural', 'total'), names_to = "urban",
                 values_to = 'count')
  urban_wt_df$urban <- ifelse(urban_wt_df$urban == 'urban', 'Urban',
                              ifelse(urban_wt_df$urban == 'rural', 'Rural', 'Total'))
  urban_wt_df$urban <- fct_relevel(urban_wt_df$urban, 'Rural', 'Urban', 'Total')
  urban_wt_df <- urban_wt_df %>%
    group_by(urban, STNAME, scenario) %>%
    summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
    filter(urban != 'Total') %>%
    pivot_wider(values_from = pm, names_from = urban) %>%
    mutate(disparity = Rural-Urban)
  wei_plot_df <- urban_wt_df
  wei_plot_df <- dplyr::select(wei_plot_df, STNAME, scenario, disparity) %>%
    pivot_wider(names_from = 'scenario', values_from = 'disparity')
  wei_plot_df[['base']] <- wei_plot_df[[base_scenario]]
  wei_plot_df[['comparison']] <- wei_plot_df[[comparison_scenario]]
  
  wei_plot_df$direction <- ifelse(abs(wei_plot_df$base) > abs(wei_plot_df$comparison),
                                  'decreasing', 'increasing')
  
  SCEN <- c(
    "2017"               = "ref2017",
    "Reference 2050"           = "ref2050_2017",
    "Net-Zero 2050"            = "nz2050_2017",
    "Net-Zero 2050 (ELE)"        = "elec_2017",
    "Net-Zero 2050 (Equity)"     = "nz_equity_2017",
    "NZ 2050 (2016 met)" = "nz_2016meteo",
    "2017 (2016 met)"    = "ref_2016meteo",
    "Net-Zero 2050 (SSP3)" = 'SSP3'
  )
  scen_labels <- setNames(names(SCEN), SCEN)
  wei_plot_df <- filter(wei_plot_df, !is.na(base))
  
  gg_wei2 <- ggplot(wei_plot_df) +
    geom_vline(xintercept = 0) +
    geom_segment(aes(x = base, xend = comparison, y = fct_reorder(STNAME, base),
                     col = direction), 
                 arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
    geom_point(aes(x = base, y = fct_reorder(STNAME, base)), size = 2) +
    theme_bw() +
    scale_color_manual(values = c('blue', 'red'))
  
  xlab_text <- paste0('Rural - Urban Income Disparity in ', poll, ' Concentration')
  
  if (poll == 'Ozone') {
    gg_wei2 <- gg_wei2 +
      ggtitle("f) Ozone exposure disparity: Rural - Urban") +
      xlab('ppb') +
      ylab('') +
      theme(legend.position = 'none') 
    
  } else {
    gg_wei2 <- gg_wei2 +
      ggtitle("e) PM2.5 exposure disparity: Rural - Urban") +
      xlab(bquote(μg/m^3)) +
      ylab('') +
      theme(legend.position = 'none')
    
  }
  
  return(list(gg_wei2))
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

get_demo_df_ssp3 <- function(pollutant_df) {
  ssp3 <- read.csv("./input_data/demo_data_ssp3.csv")
  ssp3_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH) %>%
    mutate(other = total - black - white)
  ssp3_urban <- read.csv("./input_data/urban_pct_ssp3.csv")
  ssp3_urban_v2 <- mutate(ssp3_urban, GEOID = fips) %>%
    dplyr::select(-fips)
  demo_df <- left_join(ssp3_v2, ssp3_urban_v2) %>%
    mutate(GEOID = as.numeric(GEOID)) %>%
    left_join(dplyr::select(pollutant_df, GEOID = FIPS, VALUE, SCENARIO, STNAME)) %>%
    mutate(urban = total*POPPCT_URB, rural = total - urban)
  return(demo_df)
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
    left_join(dplyr::select(pollutant_df, GEOID = FIPS, STNAME, 
                            VALUE, SCENARIO)) %>%
    mutate(non_hispanic = total - hispanic) %>%
    mutate(race_cat = ifelse(black/total > 0.67, 'Black',
                             ifelse(white/total > 0.67, 'White', 'Mixed'))) %>% 
    left_join(urban_pct) %>%
    mutate(urban = total*POPPCT_URB, rural = total - urban)
  demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]
  return(demo_df)
}


data_dir <- c("./input_data/data/")
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
                                     'ref_2016meteo'
                     )) %>%
  dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT)
all_oz_new <- filter(all_data_new, TIMESCALE == 'july', POLLUTANT == 'ozone',
                     SCENARIO %in% c('ref2017', 
                                     'ref2050_2017', 
                                     'nz2050_2017', 
                                     'elec_2017', 
                                     'nz_equity_2017', 
                                     'nz_2016meteo',
                                     'ref_2016meteo'
                     )) %>%
  dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT)

data('fips_codes')
fips_v2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
  dplyr::select(FIPS, STNAME = state_name)
all_pm_new <- left_join(all_pm_new, fips_v2) %>%
  filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))
all_oz_new <- left_join(all_oz_new, fips_v2) %>%
  filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))
scenario_cols <- c('ref2017', 
                   'ref2050_2017', 
                   'nz2050_2017', 
                   'nz_equity_2017',
                   'elec_2017')


demo_list <- get_demo_df()
oz_demo_og <- process_poll_df(demo_list = demo_list, pollutant_df = all_oz_new)
oz_demo_ssp3 <- get_demo_df_ssp3(pollutant_df = all_oz_new) %>%
  filter(SCENARIO == 'nz2050_2017') %>%
  mutate(SCENARIO = 'SSP3')
oz_demo <- bind_rows(oz_demo_og, oz_demo_ssp3)


pm_demo_og <- process_poll_df(demo_list = demo_list, pollutant_df = all_pm_new)
pm_demo_ssp3 <- get_demo_df_ssp3(pollutant_df = all_pm_new) %>%
  filter(SCENARIO == 'nz2050_2017') %>%
  mutate(SCENARIO = 'SSP3')
pm_demo <- bind_rows(pm_demo_og, pm_demo_ssp3)


scen_comps <- list(
  c('nz2050_2017', 'ref2017'),
  c('nz_2016meteo', 'ref_2016meteo'),
  c('nz_2016meteo', 'nz2050_2017'),
  c('ref_2016meteo', 'ref2017'),
  c('nz2050_2017', 'ref2050_2017'),
  
  c('nz_equity_2017', 'ref2017'),
  c('nz_equity_2017', 'nz2050_2017'),
  c('nz_equity_2017', 'ref2050_2017'),
  c('elec_2017', 'ref2017'),
  c('elec_2017', 'nz2050_2017'),
  c('elec_2017', 'ref2050_2017'),
  c('SSP3', 'ref2017')
)


get_race_disp_df(pm_demo, poll = 'PM2.5', base_scenario = 'ref2017',
                  comparison_scenario = 'SSP3')

for (i in 1:length(scen_comps)) {
  gg_pm_race <- get_race_disp_df(pm_demo, poll = 'PM2.5', 
                                  base_scenario = scen_comps[[i]][2], 
                                  comparison_scenario = scen_comps[[i]][1])
  gg_oz_race <- get_race_disp_df(oz_demo, poll = 'Ozone', 
                                  base_scenario = scen_comps[[i]][2], 
                                  comparison_scenario = scen_comps[[i]][1])
  gg_pm_income <- get_income_disp_df(pm_demo, poll = 'PM2.5', 
                                     base_scenario = scen_comps[[i]][2], 
                                     comparison_scenario = scen_comps[[i]][1])
  gg_oz_income <- get_income_disp_df(oz_demo, poll = 'Ozone', 
                                      base_scenario = scen_comps[[i]][2], 
                                      comparison_scenario = scen_comps[[i]][1])
  gg_pm_urban <- get_urban_disp_df(pm_demo, poll = 'PM2.5', 
                                    base_scenario = scen_comps[[i]][2], 
                                    comparison_scenario = scen_comps[[i]][1])
  gg_oz_urban <- get_urban_disp_df(oz_demo, poll = 'Ozone', 
                                    base_scenario = scen_comps[[i]][2], 
                                    comparison_scenario = scen_comps[[i]][1])
  gg_plotlist_race <- c(gg_pm_race, gg_oz_race)
  gg_plotlist_income <- c(gg_pm_income, gg_oz_income)
  gg_plotlist_urban <- c(gg_pm_urban, gg_oz_urban)
  scen_plotlist <- c(gg_plotlist_race, gg_plotlist_income, gg_plotlist_urban)

  pdf(paste0(plot_dir, '/rtr_arrows_', scen_comps[[i]][2], '_', scen_comps[[i]][1], '_',
             Sys.Date(), '.pdf'), width = 15, height = 20)
  print(
    ggarrange(plotlist = c(gg_plotlist_race, gg_plotlist_income, gg_plotlist_urban), ncol = 2, nrow = 3)
  )
  dev.off()
}
