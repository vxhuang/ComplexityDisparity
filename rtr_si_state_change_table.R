library(tidycensus)
library(tidyverse)

# set to local directory where the repo is stored
setwd(repo_directory)
data_dir <- c("./input_data/")


get_demo_df <- function() {
  # demography
  demo <- get_acs(variables = c(
    'B01003_001', 
    
    # 'B02001_002', 'B02001_003',
    # 'B02001_004', 'B02001_005', 'B02001_006',
    
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
                 
                 # 'B02001_002', 'B02001_003',
                 # 'B02001_004', 'B02001_005', 'B02001_006',
                 
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
    mutate(GEOID = as.numeric(GEOID))
  demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]
  return(demo_df)
}
data('fips_codes')
fips <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
  dplyr::select(FIPS, STNAME = state_name)
all_data_new <- read.csv("./input_data/combined_data.csv")
mydat <- all_data_new
demo_df <- get_demo_df() %>%
  rename(FIPS = GEOID)
all_dat <- left_join(mydat, fips) %>%
  left_join(demo_df)
urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
urban_pct$FIPS <- as.numeric(paste0(sprintf("%02d", urban_pct$STATE),
                                     sprintf("%03d", urban_pct$COUNTY)))
urban_pct <- dplyr::select(urban_pct, POPPCT_URB, FIPS)
all_dat2 <- left_join(all_dat, urban_pct) %>%
  mutate(urban = total*POPPCT_URB, rural = total - urban) %>%
  mutate(low_income = (i2+i3+i4+i5+i6), 
       high_income = (i16+i17)) %>%
  group_by(STNAME, SCENARIO, POLLUTANT) %>%
  summarize(
    black = weighted.mean(x = VALUE, w = black),
    white = weighted.mean(x = VALUE, w = white),
    low_income = weighted.mean(x = VALUE, w = low_income),
    high_income = weighted.mean(x = VALUE, w = high_income),
    urban = weighted.mean(x = VALUE, w = urban),
    rural = weighted.mean(x = VALUE, w = rural)
    
  ) %>%
  arrange(desc(POLLUTANT), STNAME, SCENARIO) %>%
  rename(State = STNAME, Scenario = SCENARIO, Pollutant = POLLUTANT,
         Black = black, White = white, `Low Income` = low_income,
         `High Income` = high_income, Urban = urban, Rural = rural) %>%
  ungroup() %>%
  filter(Scenario %in% c('elec_2017', 'nz2050_2017', 'nz_equity_2017', 'ref2017', 'ref2050_2017'))

all_dat2$Scenario <- ifelse(all_dat2$Scenario == 'elec_2017', 'Net-Zero 2050 (ELE)',
                    ifelse(all_dat2$Scenario == 'nz2050_2017', 'Net-Zero 2050',
                    ifelse(all_dat2$Scenario == 'nz_equity_2017', 'Net-Zero 2050 (Equity)',
                    ifelse(all_dat2$Scenario == 'ref2017', '2017',
                    ifelse(all_dat2$Scenario == 'ref2050_2017', 'Reference 2050', all_dat2$Scenario)))))

all_dat3 <- left_join(all_dat, urban_pct) %>%
  mutate(urban = total*POPPCT_URB, rural = total - urban) %>%
  mutate(low_income = (i2+i3+i4+i5+i6), 
         high_income = (i16+i17)) %>%
  group_by(SCENARIO, POLLUTANT) %>%
  summarize(
    black = weighted.mean(x = VALUE, w = black),
    white = weighted.mean(x = VALUE, w = white),
    low_income = weighted.mean(x = VALUE, w = low_income),
    high_income = weighted.mean(x = VALUE, w = high_income),
    urban = weighted.mean(x = VALUE, w = urban),
    rural = weighted.mean(x = VALUE, w = rural)
    
  ) %>%
  arrange(desc(POLLUTANT), SCENARIO) %>%
  rename(Scenario = SCENARIO, Pollutant = POLLUTANT,
         Black = black, White = white, `Low Income` = low_income,
         `High Income` = high_income, Urban = urban, Rural = rural) %>%
  ungroup() %>%
  filter(Scenario %in% c('elec_2017', 'nz2050_2017', 'nz_equity_2017', 'ref2017', 'ref2050_2017')) %>%
  mutate(State = 'National')

all_dat3$Scenario <- ifelse(all_dat3$Scenario == 'elec_2017', 'Net-Zero 2050 (ELE)',
                            ifelse(all_dat3$Scenario == 'nz2050_2017', 'Net-Zero 2050',
                                   ifelse(all_dat3$Scenario == 'nz_equity_2017', 'Net-Zero 2050 (Equity)',
                                          ifelse(all_dat3$Scenario == 'ref2017', '2017',
                                                 ifelse(all_dat3$Scenario == 'ref2050_2017', 'Reference 2050', all_dat3$Scenario)))))

all_dat4 <- bind_rows(all_dat2, all_dat3)

write.csv(all_dat4, "./ouputs/state_change_table_v2.csv")
