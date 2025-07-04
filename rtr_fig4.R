library(viridis)
library(tidycensus)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)
library(ggridges)
library(forcats)
library(ggpubr)

data_dir <- c('../air_health/data/')
plot_dir <- c('../air_health/plots/rtr_figs/')

all_data_new <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
all_pm_new <- filter(all_data_new, TIMESCALE == 'annual', POLLUTANT == 'pm25',
	SCENARIO %in% c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled')) %>%
	dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT) %>%
	pivot_wider(values_from = VALUE, names_from = SCENARIO)
all_oz_new <- filter(all_data_new, TIMESCALE == 'july', POLLUTANT == 'ozone',
	SCENARIO %in% c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled')) %>%
	dplyr::select(-VERSION, -X, -TIMESCALE, -POLLUTANT) %>%
	pivot_wider(values_from = VALUE, names_from = SCENARIO)
head(all_pm_new)

data('fips_codes')
fips_v2 <- mutate(fips_codes, FIPS = as.numeric(paste0(state_code, county_code))) %>%
	dplyr::select(FIPS, STNAME = state_name)
all_pm_new <- left_join(all_pm_new, fips_v2) %>%
	filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))

# all_pm_new_clean <- filter(all_pm_new, !c(STNAME %in% c('Louisiana', 'Virginia', 'Missouri', 'Maryland', 'Nevada')))
# all_pm_new_la <- filter(all_pm_new, STNAME == 'Louisiana')
# all_pm_new_la$NAME <- gsub('Parish', 'County', all_pm_new_la$NAME)

# all_pm_new_va <- filter(all_pm_new, STNAME == 'Virginia')
# all_pm_new_va$NAME <- gsub('city', 'County', all_pm_new_va$NAME)

# all_pm_new_mo <- filter(all_pm_new, STNAME == 'Missouri')
# all_pm_new_mo$NAME <- gsub('city', 'County', all_pm_new_mo$NAME)

# all_pm_new_md <- filter(all_pm_new, STNAME == 'Maryland')
# all_pm_new_md$NAME <- gsub('city', 'County', all_pm_new_md$NAME)

# all_pm_new_nv <- filter(all_pm_new, STNAME == 'Nevada')
# all_pm_new_nv$NAME <- ifelse(all_pm_new_nv$NAME == 'Carson City, Nevada', 'Carson City County, Nevada', all_pm_new_nv$NAME)

# all_pm_new <- bind_rows(all_pm_new_clean, all_pm_new_la, all_pm_new_va, all_pm_new_mo, all_pm_new_md, all_pm_new_nv)

all_oz_new <- left_join(all_oz_new, fips_v2) %>%
	filter(!(STNAME %in% c('Puerto Rico', 'Hawaii', 'Alaska', 'U.S. Virgin Islands')))

# all_oz_new_clean <- filter(all_oz_new, !c(STNAME %in% c('Louisiana', 'Virginia', 'Missouri', 'Maryland', 'Nevada')))
# all_oz_new_la <- filter(all_oz_new, STNAME == 'Louisiana')
# all_oz_new_la$NAME <- gsub('Parish', 'County', all_oz_new_la$NAME)

# all_oz_new_va <- filter(all_oz_new, STNAME == 'Virginia')
# all_oz_new_va$NAME <- gsub('city', 'County', all_oz_new_va$NAME)

# all_oz_new_mo <- filter(all_oz_new, STNAME == 'Missouri')
# all_oz_new_mo$NAME <- gsub('city', 'County', all_oz_new_mo$NAME)

# all_oz_new_md <- filter(all_oz_new, STNAME == 'Maryland')
# all_oz_new_md$NAME <- gsub('city', 'County', all_oz_new_md$NAME)

# all_oz_new_nv <- filter(all_pm_new, STNAME == 'Nevada')
# all_oz_new_nv$NAME <- ifelse(all_oz_new_nv$NAME == 'Carson City, Nevada', 'Carson City County, Nevada', all_oz_new_nv$NAME)

# all_oz_new <- bind_rows(all_oz_new_clean, all_oz_new_la, all_oz_new_va, all_oz_new_mo, all_oz_new_md, all_oz_new_nv)

get_demo_df <- function(pollutant_df) {
	# epa regions
	# states_epa <- read.csv(paste0(data_dir, 'states_epa.csv'))

	# urban-rural
	urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
	urban_pct$GEOID <- as.numeric(paste0(sprintf("%02d", urban_pct$STATE),
		sprintf("%03d", urban_pct$COUNTY)))
	# urban_pct$COUNTY_NAME <- gsub(' *', '', urban_pct$COUNTY_NAME, fixed = TRUE)
	# urban_pct$NAME <- paste0(urban_pct$COUNTY_NAME, ' County, ', urban_pct$STATE_NAME)
	urban_pct <- dplyr::select(urban_pct, GEOID, POPPCT_URB)

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
		mutate(GEOID = as.numeric(GEOID)) %>%
		left_join(dplyr::select(pollutant_df, GEOID = FIPS, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled)) %>%
		mutate(non_hispanic = total - hispanic) %>%
		mutate(race_cat = ifelse(black/total > 0.67, 'Black',
			ifelse(white/total > 0.67, 'White', 'Mixed'))) %>% 
		# left_join(states_epa) %>%
		left_join(urban_pct) %>%
		# mutate(POPPCT_URB = as.numeric(substr(POPPCT_URB, 1, 4))/100) %>%
		mutate(urban = total*POPPCT_URB, rural = total - urban)
	demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]
}

get_race_df <- function(demo_df) {
	race_wt_df <- demo_df %>%
	dplyr::select(GEOID, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled,
		white, black, native_am, asian, pi) %>%
	pivot_longer(cols = c('white', 'black', 'native_am', 'asian', 
		'pi'), names_to = "race",
		values_to = 'count') %>%
	pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "scenario",
		values_to = 'pm')
	# race_wt_df$scenario <- ifelse(race_wt_df$scenario == 'X2017', '2017', 
	# 	ifelse(race_wt_df$scenario == 'NZ2050RCP', 'NZ2050RCP8.5', race_wt_df$scenario))
	race_wt_df <- mutate(race_wt_df, 
		scenario = fct_relevel(scenario, '2017', 'REF2050', 'NZ2050', 'newNZ2050', 'NZ2050RCP', 'RCP45NZ2050', 'equityNZ2050scaled'))
	race_wt_df$race <- ifelse(race_wt_df$race == 'white', 'White',
		ifelse(race_wt_df$race == 'black', 'Black',
			ifelse(race_wt_df$race == 'native_am', 'Native Am.',
				ifelse(race_wt_df$race == 'asian', 'Asian', 'P.I.'))))

	##### Clump all non-black and non-white into "other"
	race_wt_df_2 <- race_wt_df
	race_wt_df_2$race <- ifelse(race_wt_df_2$race %in% c('White', 'Black'), race_wt_df_2$race, 'Other')
	race_wt_df_2 <- filter(race_wt_df_2, race != 'Other')
}

get_race_df_ssp <- function(demo_df) {
	race_wt_df <- demo_df %>%
	dplyr::select(GEOID, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled,
		white, black) %>%
	pivot_longer(cols = c('white', 'black'), names_to = "race",
		values_to = 'count') %>%
	pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "scenario",
		values_to = 'pm')
	# race_wt_df$scenario <- ifelse(race_wt_df$scenario == 'X2017', '2017', 
	# 	ifelse(race_wt_df$scenario == 'NZ2050RCP', 'NZ2050RCP8.5', race_wt_df$scenario))
	race_wt_df <- mutate(race_wt_df, 
		scenario = fct_relevel(scenario, '2017', 'REF2050', 'NZ2050', 'newNZ2050', 'NZ2050RCP', 'RCP45NZ2050', 'equityNZ2050scaled'))
	race_wt_df$race <- ifelse(race_wt_df$race == 'white', 'White',
		ifelse(race_wt_df$race == 'black', 'Black', race_wt_df$race))

	##### Clump all non-black and non-white into "other"
	race_wt_df_2 <- race_wt_df
	race_wt_df_2$race <- ifelse(race_wt_df_2$race %in% c('White', 'Black'), race_wt_df_2$race, 'Other')
	race_wt_df_2 <- filter(race_wt_df_2, race != 'Other')
}

get_income_df <- function(demo_df) {
	income_wt_df <- as.data.frame(demo_df) %>%
		mutate(low_income = (i2+i3+i4+i5+i6), mid_income = (i7+i8+i9+i10+i11+i12+i13+i14+i15),
			high_income = (i16+i17)) %>%
		dplyr::select(GEOID, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled, low_income, mid_income,
			high_income) %>%
		pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
			values_to = 'count') %>%
		pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "scenario",
			values_to = 'pm')
	# income_wt_df$scenario <- ifelse(income_wt_df$scenario == 'X2017', '2017', 
	# 	ifelse(income_wt_df$scenario == 'NZ2050RCP', 'NZ2050RCP8.5', income_wt_df$scenario))
	income_wt_df <- mutate(income_wt_df, 
		scenario = fct_relevel(scenario, '2017', 'REF2050', 'NZ2050', 'newNZ2050', 'NZ2050RCP', 'RCP45NZ2050', 'equityNZ2050scaled'))
	income_wt_df$income <- ifelse(income_wt_df$income == 'high_income', 'High Income',
		ifelse(income_wt_df$income == 'mid_income', 'Middle Income', 'Low Income'))
	income_wt_df$income <- fct_relevel(income_wt_df$income, 'High Income', 'Middle Income', 'Low Income')
	income_wt_df <- filter(income_wt_df, income != 'Middle Income')
	return(income_wt_df)
}

get_urban_df <- function(demo_df) {
	urban_wt_df <- demo_df %>%
		dplyr::select(GEOID, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled,
			urban, rural) %>%
		pivot_longer(cols = c('urban', 'rural'), names_to = "Geography",
			values_to = 'count') %>%
		pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "scenario",
			values_to = 'pm')
	# urban_wt_df$scenario <- ifelse(urban_wt_df$scenario == 'X2017', '2017', 
	# 	ifelse(urban_wt_df$scenario == 'NZ2050RCP', 'NZ2050RCP8.5', urban_wt_df$scenario))
	urban_wt_df <- mutate(urban_wt_df, 
		scenario = fct_relevel(scenario, '2017', 'REF2050', 'NZ2050', 'newNZ2050', 'NZ2050RCP', 'RCP45NZ2050', 'equityNZ2050scaled'))
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
		ungroup() %>%
		group_by(scenario) %>%
		summarize(med_diff = abs(diff(median)))
	return(input_df2)
}

get_demo_df_ssp3 <- function(pollutant_df) {
	ssp3 <- read.csv('../air_health/data/demo_data_ssp3.csv')
	ssp3_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH)
	ssp3_urban <- read.csv('../air_health/data/urban_pct_ssp3.csv')
	ssp3_urban_v2 <- mutate(ssp3_urban, GEOID = fips) %>%
	dplyr::select(-fips)
	demo_df <- left_join(ssp3_v2, ssp3_urban_v2) %>%
		mutate(GEOID = as.numeric(GEOID)) %>%
		left_join(dplyr::select(pollutant_df, GEOID = FIPS, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled)) %>%
		mutate(urban = total*POPPCT_URB, rural = total - urban)
	return(demo_df)
}

get_demo_df_ssp5 <- function(pollutant_df) {
	ssp3 <- read.csv('../air_health/data/demo_data_ssp5.csv')
	ssp3_v2 <- dplyr::rename(ssp3, white = WNH, total = TOT_POP, black = BNH)
	ssp3_urban <- read.csv('../air_health/data/urban_pct_ssp5.csv')
	ssp3_urban_v2 <- mutate(ssp3_urban, GEOID = fips) %>%
	dplyr::select(-fips)
	demo_df <- left_join(ssp3_v2, ssp3_urban_v2) %>%
		mutate(GEOID = as.numeric(GEOID)) %>%
		left_join(dplyr::select(pollutant_df, GEOID = FIPS, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled)) %>%
		mutate(urban = total*POPPCT_URB, rural = total - urban)
	return(demo_df)
}


get_plotlist <- function(pol_df, pol_type) {
	message('BEEEEEEEP1')
	demo_df <- get_demo_df(pol_df)
	message('BEEEEEEEP2')
	urban_df <- get_urban_df(demo_df)
	message('BEEEEEEEP3')
	race_df <- get_race_df(demo_df)
	message('BEEEEEEEP4')
	income_df <- get_income_df(demo_df)
	message('BEEEEEEEP5')
	race_disp <- get_disp_numbers(race_df, 'race')
	message('BEEEEEEEP6')
	urban_disp <- get_disp_numbers(urban_df, 'Geography')
	message('BEEEEEEEP7')
	income_disp <- get_disp_numbers(income_df, 'income')

	message('BEEEEEEEP8')
	race_df_ssp3 <- get_race_df_ssp(get_demo_df_ssp3(pol_df)) %>%
		filter(scenario == 'NZ2050') %>%
		mutate(scenario = 'SSP3')
	message('BEEEEEEEP9')
	race_disp_ssp3 <- get_disp_numbers(race_df_ssp3, 'race')
	message('BEEEEEEEP10')

	urban_df_ssp3 <- get_urban_df(get_demo_df_ssp3(pol_df)) %>%
		filter(scenario == 'NZ2050') %>%
		mutate(scenario = 'SSP3')
	message('BEEEEEEEP13')

	urban_disp_ssp3 <- get_disp_numbers(urban_df_ssp3, 'Geography')
	message('BEEEEEEEP14')

	income_df_ssp3 <- get_income_df(get_demo_df_ssp3(pol_df)) %>%
		filter(scenario == 'NZ2050') %>%
		mutate(scenario = 'SSP3')
	message('BEEEEEEEP17')

	income_disp_ssp3 <- get_disp_numbers(income_df_ssp3, 'income')
	message('BEEEEEEEP18')


	race_df_ssp5 <- get_race_df_ssp(get_demo_df_ssp5(pol_df)) %>%
		filter(scenario == 'NZ2050') %>%
		mutate(scenario = 'SSP5')
	message('BEEEEEEEP11')

	race_disp_ssp5 <- get_disp_numbers(race_df_ssp5, 'race')
	message('BEEEEEEEP12')

	urban_df_ssp5 <- get_urban_df(get_demo_df_ssp5(pol_df)) %>%
		filter(scenario == 'NZ2050') %>%
		mutate(scenario = 'SSP5')
	message('BEEEEEEEP15')

	urban_disp_ssp5 <- get_disp_numbers(urban_df_ssp5, 'Geography')
	message('BEEEEEEEP16')


	income_df_ssp5 <- get_income_df(get_demo_df_ssp5(pol_df)) %>%
		filter(scenario == 'NZ2050') %>%
		mutate(scenario = 'SSP5')
	message('BEEEEEEEP19')

	income_disp_ssp5 <- get_disp_numbers(income_df_ssp5, 'income')
	message('BEEEEEEEP20')

	race_df <- bind_rows(race_df, race_df_ssp3, race_df_ssp5)
	urban_df <- bind_rows(urban_df, urban_df_ssp3, urban_df_ssp5)
	income_df <- bind_rows(income_df, income_df_ssp3, income_df_ssp5)

	race_disp <- bind_rows(race_disp, race_disp_ssp3, race_disp_ssp5)
	urban_disp <- bind_rows(urban_disp, urban_disp_ssp3, urban_disp_ssp5)
	income_disp <- bind_rows(income_disp, income_disp_ssp3, income_disp_ssp5)


	race_df$scenario <- recode(race_df$scenario,
	  'REF2050' = 'Reference 2050',
	  'NZ2050' = 'Net-Zero 2050',
	  'NZ2050RCP' = 'Net-Zero 2050 (RCP8.5)',
	  'newNZ2050' = 'Net-Zero 2050 (Equity, old)',
	  'RCP45NZ2050' = 'Net-Zero 2050 (RCP4.5)',
	  '2017' = '2017',
	  'SSP3' = 'Net-Zero 2050 (SSP3)',
	  'SSP5' = 'Net-Zero 2050 (SSP5)',
	  'equityNZ2050scaled' = 'Net-Zero 2050 (Equity)')

	urban_df$scenario <- recode(urban_df$scenario,
	  'REF2050' = 'Reference 2050',
	  'NZ2050' = 'Net-Zero 2050',
	  'NZ2050RCP' = 'Net-Zero 2050 (RCP8.5)',
	  'newNZ2050' = 'Net-Zero 2050 (Equity, old)',
	  'RCP45NZ2050' = 'Net-Zero 2050 (RCP4.5)',
	  '2017' = '2017',
	  'SSP3' = 'Net-Zero 2050 (SSP3)',
	  'SSP5' = 'Net-Zero 2050 (SSP5)',
	  'equityNZ2050scaled' = 'Net-Zero 2050 (Equity)')

	income_df$scenario <- recode(income_df$scenario,
	  'REF2050' = 'Reference 2050',
	  'NZ2050' = 'Net-Zero 2050',
	  'NZ2050RCP' = 'Net-Zero 2050 (RCP8.5)',
	  'newNZ2050' = 'Net-Zero 2050 (Equity, old)',
	  'RCP45NZ2050' = 'Net-Zero 2050 (RCP4.5)',
	  '2017' = '2017',
	  'SSP3' = 'Net-Zero 2050 (SSP3)',
	  'SSP5' = 'Net-Zero 2050 (SSP5)',
	  'equityNZ2050scaled' = 'Net-Zero 2050 (Equity)')
	  		
	race_disp$scenario <- recode(race_disp$scenario,
	  'REF2050' = 'Reference 2050',
	  'NZ2050' = 'Net-Zero 2050',
	  'NZ2050RCP' = 'Net-Zero 2050 (RCP8.5)',
	  'newNZ2050' = 'Net-Zero 2050 (Equity, old)',
	  'RCP45NZ2050' = 'Net-Zero 2050 (RCP4.5)',
	  '2017' = '2017',
	  'SSP3' = 'Net-Zero 2050 (SSP3)',
	  'SSP5' = 'Net-Zero 2050 (SSP5)',
	  'equityNZ2050scaled' = 'Net-Zero 2050 (Equity)')
	  			
	urban_disp$scenario <- recode(urban_disp$scenario,
	  'REF2050' = 'Reference 2050',
	  'NZ2050' = 'Net-Zero 2050',
	  'NZ2050RCP' = 'Net-Zero 2050 (RCP8.5)',
	  'newNZ2050' = 'Net-Zero 2050 (Equity, old)',
	  'RCP45NZ2050' = 'Net-Zero 2050 (RCP4.5)',
	  '2017' = '2017',
	  'SSP3' = 'Net-Zero 2050 (SSP3)',
	  'SSP5' = 'Net-Zero 2050 (SSP5)',
	  'equityNZ2050scaled' = 'Net-Zero 2050 (Equity)')
	  				
	income_disp$scenario <- recode(income_disp$scenario,
	  'REF2050' = 'Reference 2050',
	  'NZ2050' = 'Net-Zero 2050',
	  'NZ2050RCP' = 'Net-Zero 2050 (RCP8.5)',
	  'newNZ2050' = 'Net-Zero 2050 (Equity, old)',
	  'RCP45NZ2050' = 'Net-Zero 2050 (RCP4.5)',
	  '2017' = '2017',
	  'SSP3' = 'Net-Zero 2050 (SSP3)',
	  'SSP5' = 'Net-Zero 2050 (SSP5)',
	  'equityNZ2050scaled' = 'Net-Zero 2050 (Equity)')

	race_df$scenario <- fct_relevel(race_df$scenario, '2017', 'Reference 2050', 'Net-Zero 2050', 
		'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (RCP4.5)', 'Net-Zero 2050 (SSP3)', 'Net-Zero 2050 (SSP5)', 'Net-Zero 2050 (Equity, old)')
	urban_df$scenario <- fct_relevel(urban_df$scenario, '2017', 'Reference 2050', 'Net-Zero 2050', 
		'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (RCP4.5)', 'Net-Zero 2050 (SSP3)', 'Net-Zero 2050 (SSP5)', 'Net-Zero 2050 (Equity, old)')
	income_df$scenario <- fct_relevel(income_df$scenario, '2017', 'Reference 2050', 'Net-Zero 2050', 
		'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (RCP4.5)', 'Net-Zero 2050 (SSP3)', 'Net-Zero 2050 (SSP5)', 'Net-Zero 2050 (Equity, old)')
	
	race_disp$scenario <- fct_relevel(race_disp$scenario, '2017', 'Reference 2050', 'Net-Zero 2050', 
		'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (RCP4.5)', 'Net-Zero 2050 (SSP3)', 'Net-Zero 2050 (SSP5)', 'Net-Zero 2050 (Equity, old)')
	urban_disp$scenario <- fct_relevel(urban_disp$scenario, '2017', 'Reference 2050', 'Net-Zero 2050', 
		'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (RCP4.5)', 'Net-Zero 2050 (SSP3)', 'Net-Zero 2050 (SSP5)', 'Net-Zero 2050 (Equity, old)')
	income_disp$scenario <- fct_relevel(income_disp$scenario, '2017', 'Reference 2050', 'Net-Zero 2050', 
		'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (RCP4.5)', 'Net-Zero 2050 (SSP3)', 'Net-Zero 2050 (SSP5)', 'Net-Zero 2050 (Equity, old)')

	urban_df$style <- ifelse(urban_df$scenario %in% c('2017', 'Reference 2050', 'Net-Zero 2050'), 'Main', 'Sensitivity')
	income_df$style <- ifelse(income_df$scenario %in% c('2017', 'Reference 2050', 'Net-Zero 2050'), 'Main', 'Sensitivity')
	race_df$style <- ifelse(race_df$scenario %in% c('2017', 'Reference 2050', 'Net-Zero 2050'), 'Main', 'Sensitivity')

	final_scenario_list <- c('2017', 'Reference 2050', 'Net-Zero 2050', 'Net-Zero 2050 (RCP8.5)', 'Net-Zero 2050 (Equity)', 'Net-Zero 2050 (SSP3)')

	gg_urban <- ggplot(filter(urban_df, scenario %in% final_scenario_list)) +
		geom_boxplot(aes(x = scenario, y = pm, weight = count, col = fct_relevel(Geography, 'Urban', 'Rural'), linetype = style), outliers = FALSE) +
		theme_bw() +
		xlab('') + ylab('') +
		scale_color_manual(name = 'Geography', values = c('#1b9e77', '#d95f02')) +
		scale_linetype_manual(values = c("Main" = "solid", "Sensitivity" = "twodash"), name = '')


	gg_income <- ggplot(filter(income_df, scenario %in% final_scenario_list)) +
		geom_boxplot(aes(x = scenario, y = pm, weight = count, col = income, linetype = style), outliers = FALSE) +
		theme_bw() +
		xlab('') + ylab('') +
		scale_color_manual(name = 'Income', values = c('#7570b3', '#e7298a')) +
		scale_linetype_manual(values = c("Main" = "solid", "Sensitivity" = "twodash"), name = '')

	gg_race <- ggplot(filter(race_df, scenario %in% final_scenario_list)) +
		geom_boxplot(aes(x = scenario, y = pm, weight = count, col = race, linetype = style), outliers = FALSE) +
		theme_bw() +
		xlab('') + ylab('') +
		scale_color_manual(name = 'Race', values = c('#66a61e', '#e6ab02')) +
		scale_linetype_manual(values = c("Main" = "solid", "Sensitivity" = "twodash"), name = '')

		print(filter(race_df, is.na(pm)) %>% count(scenario))
		print(filter(urban_df, is.na(pm)) %>% count(scenario))
		print(filter(income_df, is.na(pm)) %>% count(scenario))


	if (pol_type == 'pm') {
		gg_race <- gg_race +
			ylab(bquote(μg/m^3)) +
			annotate("text", x = 1, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == '2017')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 2, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Reference 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 3, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 4, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (RCP8.5)')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 5, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (Equity)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 6, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (RCP4.5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			annotate("text", x = 6, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (SSP3)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 8, y = 1, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (SSP5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			# labs(tag = 'a)') + 
			ggtitle('a) Distribution of PM2.5 Exposure Across Racial Groups')
		gg_urban <- gg_urban +
			annotate("text", x = 1, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == '2017')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 2, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Reference 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 3, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 4, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (RCP8.5)')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 5, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (Equity)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 6, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (RCP4.5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			annotate("text", x = 6, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (SSP3)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 8, y = 1, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (SSP5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			# labs(tag = 'c)') + 
			ggtitle('c) Distribution of PM2.5 Exposure Across Geography')
		gg_income <- gg_income +
			annotate("text", x = 1, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == '2017')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 2, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Reference 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 3, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 4, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (RCP8.5)')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 5, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (Equity)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 6, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (RCP4.5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			annotate("text", x = 6, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (SSP3)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 8, y = 1, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (SSP5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			# labs(tag = 'b)') + 
			ggtitle('b) Distribution of PM2.5 Exposure Across Income Groups')

	}

	if (pol_type == 'ozone') {
		gg_race <- gg_race + 
			ylab('ppb') +
			annotate("text", x = 1, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == '2017')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 2, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Reference 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 3, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 4, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (RCP8.5)')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 5, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (Equity)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 6, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (RCP4.5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			annotate("text", x = 6, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (SSP3)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 8, y = 15, label = as.character(sprintf('%.1f', round(filter(race_disp, scenario == 'Net-Zero 2050 (SSP5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			# labs(tag = 'd)') + 
			ggtitle('d) Distribution of Ozone Exposure Across Racial Groups')
		gg_urban <- gg_urban +
			annotate("text", x = 1, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == '2017')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 2, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Reference 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 3, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 4, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (RCP8.5)')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 5, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (Equity)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 6, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (RCP4.5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			annotate("text", x = 6, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (SSP3)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 8, y = 15, label = as.character(sprintf('%.1f', round(filter(urban_disp, scenario == 'Net-Zero 2050 (SSP5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			# labs(tag = 'f)') + 
			ggtitle('f) Distribution of Ozone Exposure Across Geography')
		gg_income <- gg_income +
			annotate("text", x = 1, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == '2017')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 2, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Reference 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 3, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 4, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (RCP8.5)')$med_diff, 1))),
				col = 'Black', size = 3) +
			annotate("text", x = 5, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (Equity)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 6, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (RCP4.5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			annotate("text", x = 6, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (SSP3)')$med_diff, 1))),
				col = 'Black', size = 3) +
			# annotate("text", x = 8, y = 15, label = as.character(sprintf('%.1f', round(filter(income_disp, scenario == 'Net-Zero 2050 (SSP5)')$med_diff, 1))),
			# 	col = 'Black', size = 3) +
			theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
			# labs(tag = 'e)') +
			ggtitle('e) Distribution of Ozone Exposure Across Income Groups')
	}


	return(list(gg_race, gg_income, gg_urban))
}

pm_plots <- get_plotlist(all_pm_new, pol_type = 'pm')
oz_plots <- get_plotlist(all_oz_new, pol_type = 'ozone')
all_plots <- c(pm_plots, oz_plots)

jpeg(paste0(plot_dir, '/fig4_', Sys.Date(), '.jpg'), width = 20, height = 9, quality = 1, units = 'in', res = 300)
print(ggarrange(plotlist = all_plots))
dev.off()
