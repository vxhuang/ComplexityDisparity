library(viridis)
library(tidycensus)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)
library(ggridges)
library(forcats)
library(ggpubr)

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

	return(list('demo' = demo, 'var_df' = var_df))
}

process_poll_df <- function(demo_list, pollutant_df) {
	
	data_dir <- c('../air_health/data/')


	demo <- demo_list[['demo']]
	var_df <- demo_list[['var_df']]

	# epa regions
	# states_epa <- read.csv(paste0(data_dir, 'states_epa.csv'))

	# urban-rural
	urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
	urban_pct$GEOID <- as.numeric(paste0(sprintf("%02d", urban_pct$STATE),
		sprintf("%03d", urban_pct$COUNTY)))
	# urban_pct$COUNTY_NAME <- gsub(' *', '', urban_pct$COUNTY_NAME, fixed = TRUE)
	# urban_pct$NAME <- paste0(urban_pct$COUNTY_NAME, ' County, ', urban_pct$STATE_NAME)
	urban_pct <- dplyr::select(urban_pct, GEOID, POPPCT_URB)

	demo_df <- left_join(demo, var_df) %>%
		dplyr::select(-moe, -variable) %>%
		pivot_wider(names_from = var_name, values_from = estimate) %>%
		mutate(GEOID = as.numeric(GEOID)) %>%
		left_join(pollutant_df) %>%
		# mutate(non_hispanic = total - hispanic) %>%
		# mutate(race_cat = ifelse(black/total > 0.67, 'Black',
		# 	ifelse(white/total > 0.67, 'White', 'Mixed'))) %>% 
		left_join(urban_pct) %>%
		# mutate(POPPCT_URB = as.numeric(substr(POPPCT_URB, 1, 4))/100) %>%
		mutate(urban = total*POPPCT_URB, rural = total - urban)
	demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]

	return(demo_df)
}

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

demo_list <- get_demo_df()
pm_demo <- process_poll_df(demo_list = demo_list, pollutant_df = pm_new)

demo_df <- pm_demo
st_pct_black <- demo_df %>%
	group_by(STATE_NAME) %>%
	summarize(black = sum(black, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
	mutate(pct_black = 100*(black/total)) %>%
	rename(STNAME = STATE_NAME)

race_wt_df <- demo_df %>%
	dplyr::select(GEOID, STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
		jie_2017NZ2050, jie_equityNZ2050scaled,
		pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050,
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
head(race_wt_df)

write.csv(race_wt_df, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_race.csv')

demo_df <- pm_demo
st_pct_rural <- demo_df %>%
	group_by(STATE_NAME) %>%
	summarize(rural = sum(rural, na.rm = TRUE), 
		total = sum(total, na.rm = TRUE)) %>%
	mutate(pct_rural = 100*(rural/total))

urban_wt_df <- demo_df %>%
	dplyr::select(GEOID, STNAME = STATE_NAME, 
		jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
		jie_2017NZ2050, jie_equityNZ2050scaled,
		pengfei_2017, pengfei_NZ2050, pengfei_NZ2050RCP, pengfei_REF2050,
		urban, rural, total) %>%
	pivot_longer(cols = c('urban', 'rural', 'total'), names_to = "urban",
		values_to = 'count') %>%
	pivot_longer(cols = c('jie_NEI2017', 
		'jie_newNZ2050', 'jie_RCP45NZ2050', 'jie_equityNZ2050scaled',
		'pengfei_2017', 'pengfei_NZ2050', 'pengfei_NZ2050RCP', 'pengfei_REF2050', 
		'jie_2017NZ2050'), names_to = "scenario",
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
write.csv(urban_wt_df, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_urban.csv')

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
write.csv(income_wt_df, '../air_health/data/wrf_data/shared/disparity_numbers/pm_disparity_income.csv')

oz_demo <- process_poll_df(demo_list = demo_list, pollutant_df = oz_new)

demo_df <- oz_demo
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

write.csv(race_wt_df, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_race.csv')

demo_df <- oz_demo
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
write.csv(urban_wt_df, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_urban.csv')

demo_df <- oz_demo
st_pct_lowincome <- demo_df %>%
	mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
		high_income = (i16+i17)) %>%
	group_by(STATE_NAME) %>%
	summarize(low_income = sum(low_income, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
	mutate(pct_low_income = 100*(low_income/total))

income_wt_df <- as.data.frame(demo_df) %>%
	mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
		high_income = (i16+i17)) %>%
	dplyr::select(GEOID , STNAME = STATE_NAME, jie_NEI2017, jie_newNZ2050, jie_RCP45NZ2050, 
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
write.csv(income_wt_df, '../air_health/data/wrf_data/shared/disparity_numbers/oz_disparity_income.csv')

