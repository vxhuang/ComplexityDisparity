library(viridis)
library(tidycensus)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)
library(ggridges)
library(forcats)
library(ggpubr)

#########

get_ssp_disparity_pm_race <- function(ssp_df, ssp_urban_df) {
	data('fips_codes')
	data_dir <- c('../air_health/data/')

	all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
	pm_new <- filter(all_data_new, POLLUTANT == 'pm25', TIMESCALE == 'annual') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	oz_new <- filter(all_data_new, POLLUTANT == 'ozone', TIMESCALE == 'july') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	fips_codes2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
		dplyr::select(FIPS, STATE_NAME = state_name)

	pm_new <- left_join(pm_new, fips_codes2) %>%
		rename(GEOID = FIPS)
	oz_new <- left_join(oz_new, fips_codes2) %>%
		rename(GEOID = FIPS)



	pm_demo <- left_join(ssp_df, pm_new)
	pm_demo <- left_join(pm_demo, ssp_urban_df) %>% 
		mutate(urban = total*POPPCT_URB, rural = total - urban)	

	demo_df <- pm_demo
	st_pct_black <- demo_df %>%
		group_by(STATE_NAME) %>%
		summarize(black = sum(black, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_black = 100*(black/total)) %>%
		rename(STNAME = STATE_NAME)

	race_wt_df <- demo_df %>%
		dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, jie_2017NZ2050,
			pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050, jie_equityNZ2050scaled,
			white, black, total) %>%
		pivot_longer(cols = c('white', 'black', 'total'), names_to = "race",
			values_to = 'count') %>%
		pivot_longer(cols = c('jie_NEI2017', 'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled', 
			'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 'jie_2017NZ2050'), 
		names_to = "scenario",
			values_to = 'pm')
	race_wt_df <- race_wt_df %>%
		group_by(race, STNAME, scenario) %>%
		summarize(pm = weighted.mean(x = pm, w = count)) %>%
		filter(race != 'Total') %>%
		pivot_wider(values_from = pm, names_from = race) %>%
		mutate(disparity = (black-white))
	return(race_wt_df)	
}

get_ssp_disparity_oz_race <- function(ssp_df, ssp_urban_df) {
	data('fips_codes')
	data_dir <- c('../air_health/data/')

	all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
	pm_new <- filter(all_data_new, POLLUTANT == 'pm25', TIMESCALE == 'annual') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	oz_new <- filter(all_data_new, POLLUTANT == 'ozone', TIMESCALE == 'july') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	fips_codes2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
		dplyr::select(FIPS, STATE_NAME = state_name)

	pm_new <- left_join(pm_new, fips_codes2) %>%
		rename(GEOID = FIPS)
	oz_new <- left_join(oz_new, fips_codes2) %>%
		rename(GEOID = FIPS)



	pm_demo <- left_join(ssp_df, oz_new)
	pm_demo <- left_join(pm_demo, ssp_urban_df) %>% 
		mutate(urban = total*POPPCT_URB, rural = total - urban)	

	demo_df <- pm_demo
	st_pct_black <- demo_df %>%
		group_by(STATE_NAME) %>%
		summarize(black = sum(black, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_black = 100*(black/total)) %>%
		rename(STNAME = STATE_NAME)

	race_wt_df <- demo_df %>%
		dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, jie_2017NZ2050,
			pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050, jie_equityNZ2050scaled,
			white, black, total) %>%
		pivot_longer(cols = c('white', 'black', 'total'), names_to = "race",
			values_to = 'count') %>%
		pivot_longer(cols = c('jie_NEI2017', 'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled', 
			'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 'jie_2017NZ2050'), 
		names_to = "scenario",
			values_to = 'pm')
	race_wt_df <- race_wt_df %>%
		group_by(race, STNAME, scenario) %>%
		summarize(pm = weighted.mean(x = pm, w = count)) %>%
		filter(race != 'Total') %>%
		pivot_wider(values_from = pm, names_from = race) %>%
		mutate(disparity = (black-white))
	return(race_wt_df)	
}

#########

get_ssp_disparity_pm_income <- function(ssp_df, ssp_urban_df) {
	data('fips_codes')
	data_dir <- c('../air_health/data/')

	all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
	pm_new <- filter(all_data_new, POLLUTANT == 'pm25', TIMESCALE == 'annual') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	oz_new <- filter(all_data_new, POLLUTANT == 'ozone', TIMESCALE == 'july') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	fips_codes2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
		dplyr::select(FIPS, STATE_NAME = state_name)

	pm_new <- left_join(pm_new, fips_codes2) %>%
		rename(GEOID = FIPS)
	oz_new <- left_join(oz_new, fips_codes2) %>%
		rename(GEOID = FIPS)



	pm_demo <- left_join(ssp_df, pm_new)
	pm_demo <- left_join(pm_demo, ssp_urban_df) %>% 
		mutate(urban = total*POPPCT_URB, rural = total - urban)	

	demo_df <- pm_demo
	st_pct_lowincome <- demo_df %>%
		mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
			high_income = (i16+i17)) %>%
		group_by(STATE_NAME) %>%
		summarize(low_income = sum(low_income, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_low_income = 100*(low_income/total))

	income_wt_df <- as.data.frame(demo_df) %>%
		mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
			high_income = (i16+i17)) %>%
		dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
			jie_2017NZ2050, jie_equityNZ2050scaled,
			pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050, low_income, mid_income,
			high_income) %>%
		pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
			values_to = 'count') %>%
		pivot_longer(cols = c('jie_NEI2017', 'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled',
			'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 'jie_2017NZ2050'), names_to = "Scenario",
			values_to = 'pm')
	income_wt_df$income <- ifelse(income_wt_df$income == 'high_income', 'High Income',
		ifelse(income_wt_df$income == 'mid_income', 'Middle Income', 'Low Income'))
	income_wt_df$income <- fct_relevel(income_wt_df$income, 'High Income', 'Middle Income', 'Low Income')
	income_wt_df <- filter(income_wt_df, income != 'Middle Income')
	income_wt_df <- income_wt_df %>%
		group_by(income, STNAME, Scenario) %>%
		summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
		pivot_wider(values_from = pm, names_from = income) %>%
		mutate(disparity = (`Low Income` - `High Income`))
	return(income_wt_df)	
}

get_ssp_disparity_oz_income <- function(ssp_df, ssp_urban_df) {
	data('fips_codes')
	data_dir <- c('../air_health/data/')

	all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
	pm_new <- filter(all_data_new, POLLUTANT == 'pm25', TIMESCALE == 'annual') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	oz_new <- filter(all_data_new, POLLUTANT == 'ozone', TIMESCALE == 'july') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	fips_codes2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
		dplyr::select(FIPS, STATE_NAME = state_name)

	pm_new <- left_join(pm_new, fips_codes2) %>%
		rename(GEOID = FIPS)
	oz_new <- left_join(oz_new, fips_codes2) %>%
		rename(GEOID = FIPS)



	pm_demo <- left_join(ssp_df, oz_new)
	pm_demo <- left_join(pm_demo, ssp_urban_df) %>% 
		mutate(urban = total*POPPCT_URB, rural = total - urban)	

	demo_df <- pm_demo
	st_pct_lowincome <- demo_df %>%
		mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
			high_income = (i16+i17)) %>%
		group_by(STATE_NAME) %>%
		summarize(low_income = sum(low_income, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_low_income = 100*(low_income/total))

	income_wt_df <- as.data.frame(demo_df) %>%
		mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
			high_income = (i16+i17)) %>%
		dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
			jie_2017NZ2050, jie_equityNZ2050scaled,
			pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050, low_income, mid_income,
			high_income) %>%
		pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
			values_to = 'count') %>%
		pivot_longer(cols = c('jie_NEI2017', 'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled',
			'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 'jie_2017NZ2050'), names_to = "Scenario",
			values_to = 'pm')
	income_wt_df$income <- ifelse(income_wt_df$income == 'high_income', 'High Income',
		ifelse(income_wt_df$income == 'mid_income', 'Middle Income', 'Low Income'))
	income_wt_df$income <- fct_relevel(income_wt_df$income, 'High Income', 'Middle Income', 'Low Income')
	income_wt_df <- filter(income_wt_df, income != 'Middle Income')
	income_wt_df <- income_wt_df %>%
		group_by(income, STNAME, Scenario) %>%
		summarize(pm = weighted.mean(x = pm, w = count, na.rm = TRUE)) %>%
		pivot_wider(values_from = pm, names_from = income) %>%
		mutate(disparity = (`Low Income` - `High Income`))
	return(income_wt_df)	
}

#########

get_ssp_disparity_pm_urban <- function(ssp_df, ssp_urban_df) {
	data('fips_codes')
	data_dir <- c('../air_health/data/')

	all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
	pm_new <- filter(all_data_new, POLLUTANT == 'pm25', TIMESCALE == 'annual') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	oz_new <- filter(all_data_new, POLLUTANT == 'ozone', TIMESCALE == 'july') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	fips_codes2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
		dplyr::select(FIPS, STATE_NAME = state_name)

	pm_new <- left_join(pm_new, fips_codes2) %>%
		rename(GEOID = FIPS)
	oz_new <- left_join(oz_new, fips_codes2) %>%
		rename(GEOID = FIPS)

	pm_demo <- left_join(ssp_df, pm_new)
	pm_demo <- left_join(pm_demo, ssp_urban_df) %>% 
		mutate(urban = total*POPPCT_URB, rural = total - urban)	

	demo_df <- pm_demo
	st_pct_rural <- demo_df %>%
		group_by(STATE_NAME) %>%
		summarize(rural = sum(rural, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_rural = 100*(rural/total))

	urban_wt_df <- demo_df %>%
		dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
			jie_2017NZ2050, jie_equityNZ2050scaled,
			pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050,
			urban, rural, total) %>%
		pivot_longer(cols = c('urban', 'rural', 'total'), names_to = "urban",
			values_to = 'count') %>%
		pivot_longer(cols = c('jie_NEI2017', 'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled',
			'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 'jie_2017NZ2050'), names_to = "scenario",
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
	return(urban_wt_df)	
}
get_ssp_disparity_oz_urban <- function(ssp_df, ssp_urban_df) {
	data('fips_codes')
	data_dir <- c('../air_health/data/')

	all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
	pm_new <- filter(all_data_new, POLLUTANT == 'pm25', TIMESCALE == 'annual') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	oz_new <- filter(all_data_new, POLLUTANT == 'ozone', TIMESCALE == 'july') %>%
		mutate(scenario2 = paste0(VERSION, '_', SCENARIO)) %>%
		dplyr::select(FIPS, VALUE, POLLUTANT, scenario2, TIMESCALE) %>%
		pivot_wider(values_from = VALUE, names_from = scenario2)

	fips_codes2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
		dplyr::select(FIPS, STATE_NAME = state_name)

	pm_new <- left_join(pm_new, fips_codes2) %>%
		rename(GEOID = FIPS)
	oz_new <- left_join(oz_new, fips_codes2) %>%
		rename(GEOID = FIPS)



	pm_demo <- left_join(ssp_df, oz_new)
	pm_demo <- left_join(pm_demo, ssp_urban_df) %>% 
		mutate(urban = total*POPPCT_URB, rural = total - urban)	

	demo_df <- pm_demo
	st_pct_rural <- demo_df %>%
		group_by(STATE_NAME) %>%
		summarize(rural = sum(rural, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_rural = 100*(rural/total))

	urban_wt_df <- demo_df %>%
		dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
			jie_2017NZ2050, jie_equityNZ2050scaled,
			pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050,
			urban, rural, total) %>%
		pivot_longer(cols = c('urban', 'rural', 'total'), names_to = "urban",
			values_to = 'count') %>%
		pivot_longer(cols = c('jie_NEI2017', 'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled',
			'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 'jie_2017NZ2050'), names_to = "scenario",
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
	return(urban_wt_df)	
}


ssp3 <- read.csv('../air_health/data/demo_data_ssp3.csv')
ssp3_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH)
ssp_urban <- read.csv('../air_health/data/urban_pct_ssp3.csv')
ssp_urban <- dplyr::rename(ssp_urban, GEOID = fips)
results_pm_ssp3_race <- get_ssp_disparity_pm_race(ssp3_v2, ssp_urban)
results_oz_ssp3_race <- get_ssp_disparity_oz_race(ssp3_v2, ssp_urban)
results_pm_ssp3_income <- get_ssp_disparity_pm_income(ssp3_v2, ssp_urban)
results_oz_ssp3_income <- get_ssp_disparity_oz_income(ssp3_v2, ssp_urban)
results_pm_ssp3_urban <- get_ssp_disparity_pm_urban(ssp3_v2, ssp_urban)
results_oz_ssp3_urban <- get_ssp_disparity_oz_urban(ssp3_v2, ssp_urban)



ssp5 <- read.csv('../air_health/data/demo_data_ssp5.csv')
ssp5_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH)
ssp_urban <- read.csv('../air_health/data/urban_pct_ssp5.csv')
ssp_urban <- dplyr::rename(ssp_urban, GEOID = fips)
results_pm_ssp5_race <- get_ssp_disparity_pm_race(ssp5_v2, ssp_urban)
results_oz_ssp5_race <- get_ssp_disparity_oz_race(ssp5_v2, ssp_urban)
results_pm_ssp5_income <- get_ssp_disparity_pm_income(ssp5_v2, ssp_urban)
results_oz_ssp5_income <- get_ssp_disparity_oz_income(ssp5_v2, ssp_urban)
results_pm_ssp5_urban <- get_ssp_disparity_pm_urban(ssp5_v2, ssp_urban)
results_oz_ssp5_urban <- get_ssp_disparity_oz_urban(ssp5_v2, ssp_urban)


write.csv(results_pm_ssp3_race, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_race_ssp3.csv')
write.csv(results_pm_ssp5_race, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_race_ssp5.csv')
write.csv(results_oz_ssp3_race, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_race_ssp3.csv')
write.csv(results_oz_ssp5_race, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_race_ssp5.csv')

write.csv(results_pm_ssp3_urban, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_urban_ssp3.csv')
write.csv(results_pm_ssp5_urban, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_urban_ssp5.csv')
write.csv(results_oz_ssp3_urban, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_urban_ssp3.csv')
write.csv(results_oz_ssp5_urban, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_urban_ssp5.csv')

write.csv(results_pm_ssp3_income, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_income_ssp3.csv')
write.csv(results_pm_ssp5_income, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_income_ssp5.csv')
write.csv(results_oz_ssp3_income, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_income_ssp3.csv')
write.csv(results_oz_ssp5_income, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_income_ssp5.csv')
