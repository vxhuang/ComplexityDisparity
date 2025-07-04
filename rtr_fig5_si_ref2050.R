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
plot_dir <- c('../air_health/plots/')

# exposure
oz <- read.csv(paste0(data_dir, 'ozone_summer.csv'))
pm <- read.csv(paste0(data_dir, 'county_PM2.5.csv'))

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
fips_v2 <- mutate(fips_codes, NAME = paste0(county, ', ', state_name), FIPS = as.numeric(paste0(state_code, county_code))) %>%
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
	
	demo <- demo_list[['demo']]
	var_df <- demo_list[['var_df']]

	# epa regions
	# states_epa <- read.csv(paste0(data_dir, 'states_epa.csv'))

	# urban-rural
	# urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
	# urban_pct$COUNTY_NAME <- gsub(' *', '', urban_pct$COUNTY_NAME, fixed = TRUE)
	# urban_pct$NAME <- paste0(urban_pct$COUNTY_NAME, ' County, ', urban_pct$STATE_NAME)
	# urban_pct <- dplyr::select(urban_pct, NAME, POPPCT_URB)
	urban_pct <- read.csv(paste0(data_dir, '2020_UA_COUNTY.csv'))
	urban_pct$GEOID <- as.numeric(paste0(sprintf("%02d", urban_pct$STATE),
		sprintf("%03d", urban_pct$COUNTY)))
	# urban_pct$COUNTY_NAME <- gsub(' *', '', urban_pct$COUNTY_NAME, fixed = TRUE)
	# urban_pct$NAME <- paste0(urban_pct$COUNTY_NAME, ' County, ', urban_pct$STATE_NAME)
	urban_pct <- dplyr::select(urban_pct, GEOID, POPPCT_URB)

	# glimpse(filter(urban_pct, GEOID == 1007))
	# glimpse(filter(urban_pct, STATE == 1, COUNTY == 7))

	demo_df <- left_join(demo, var_df) %>%
		dplyr::select(-moe, -variable) %>%
		pivot_wider(names_from = var_name, values_from = estimate) %>%
		mutate(GEOID = as.numeric(GEOID)) %>%
		left_join(dplyr::select(pollutant_df, GEOID = FIPS, `2017`, STNAME, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled)) %>%
		mutate(non_hispanic = total - hispanic) %>%
		mutate(race_cat = ifelse(black/total > 0.67, 'Black',
			ifelse(white/total > 0.67, 'White', 'Mixed'))) %>% 
		# left_join(states_epa) %>%
		left_join(urban_pct) %>%
		# mutate(POPPCT_URB = as.numeric(substr(POPPCT_URB, 1, 4))/100) %>%
		mutate(urban = total*POPPCT_URB, rural = total - urban)
	demo_df <- demo_df[!grepl('Alaska|Hawaii|Puerto Rico', demo_df$NAME),]
	return(demo_df)
}

get_wei_plot_race <- function(demo, poll) {
	demo_df <- demo
	st_pct_black <- demo_df %>%
		group_by(STNAME) %>%
		summarize(black = sum(black, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_black = 100*(black/total))

	race_wt_df <- demo_df %>%
		dplyr::select(GEOID, STNAME, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled,
			white, black, total) %>%
		pivot_longer(cols = c('white', 'black', 'total'), names_to = "race",
			values_to = 'count') %>%
		pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "scenario",
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
	wei_plot_df <- race_wt_df

	wei_plot_df_ranker <- filter(wei_plot_df, scenario == 'REF2050')
	wei_plot_df_ranker$higher <- ifelse(wei_plot_df_ranker$Black > wei_plot_df_ranker$White, 'Black', 'White')
	wei_plot_df_ranker$ranker <- ifelse(wei_plot_df_ranker$higher == 'Black', 20000+(wei_plot_df_ranker$disparity),
		10000+(wei_plot_df_ranker$disparity))
	wei_plot_df_ranker <- left_join(wei_plot_df_ranker, st_pct_black)
	wei_plot_df_ranker$ranker <- ifelse(wei_plot_df_ranker$pct_black < 14.2, (wei_plot_df_ranker$ranker),
		1000+wei_plot_df_ranker$ranker)
	wei_plot_df_ranker <- dplyr::select(ungroup(wei_plot_df_ranker), STNAME, ranker)
	wei_plot_df_plot <- left_join(wei_plot_df, wei_plot_df_ranker) %>%
		filter(!is.na(STNAME), STNAME != 'District of Columbia')

	wei_plot_df_plot_v2 <- wei_plot_df_plot %>%
		dplyr::select(STNAME, scenario, disparity, ranker) %>%
		mutate(disparity = disparity) %>%
		filter(scenario %in% c('REF2050', 'NZ2050')) %>%
		pivot_wider(names_from = scenario, values_from = disparity) %>%
		filter(!is.na(STNAME))
	
	wei_plot_df_plot_v2$direction <- ifelse(wei_plot_df_plot_v2$REF2050 > wei_plot_df_plot_v2$NZ2050,
		'decreasing', 'increasing')
	
	gg_wei2 <- ggplot(wei_plot_df_plot_v2) +
					geom_vline(xintercept = 0) +
		geom_segment(aes(x = REF2050, xend = NZ2050, y = fct_reorder(STNAME, REF2050)), 
			arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
		geom_point(data = filter(wei_plot_df_plot, scenario == 'REF2050', !is.na(STNAME)),
			aes(x = (disparity), y = fct_reorder(STNAME, disparity))) +
		theme_bw() +
		scale_color_manual(values = c('blue', 'red')) 

	xlab_text <- paste0('Black - White Disparity in ', poll, ' Exposure')

	if (poll == 'Ozone') {
		gg_wei2 <- gg_wei2 +
			ggtitle("b) Ozone concentration disparity: Black - White") +
			xlab('ppb') +
			ylab('') +
			theme(legend.position = 'none')

	} else {
		gg_wei2 <- gg_wei2 +
			ggtitle("a) PM2.5 concentration disparity: Black - White") +
			xlab(bquote(μg/m^3)) +
			ylab('') +
			theme(legend.position = 'none') +
			  # Custom annotation: Point symbol + label
			  annotate("rect", xmin = 1, xmax = 3, ymin = 1, ymax = 5, fill = 'white', color = 'black') +
			  
			  annotate("point", x = 1.1, y = 4, size = 3) +
			  annotate("text", x = 1.3, y = 4, hjust = 0, label = "Reference 2050", size = 4) +
			  
			  # Custom annotation: Arrow symbol + label
			  annotate("segment", x = 1.1, y = 2, xend = 1.2, yend = 2,
			           arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
			  annotate("text", x = 1.3, y = 2, hjust = 0, label = "Changes in Net-Zero 2050", size = 4)

	}

	return(list(gg_wei2))
}

get_wei_plot_income <- function(demo, poll) {
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
		dplyr::select(GEOID, STNAME, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled, low_income, mid_income,
			high_income) %>%
		pivot_longer(cols = c('low_income', 'mid_income', 'high_income'), names_to = "income",
			values_to = 'count') %>%
		pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "Scenario",
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
	wei_plot_df <- income_wt_df

	wei_plot_df_ranker <- filter(wei_plot_df, Scenario == 'REF2050')
	wei_plot_df_ranker$higher <- ifelse(wei_plot_df_ranker$`Low Income` > wei_plot_df_ranker$`High Income`, 'Low', 'High')
	wei_plot_df_ranker$ranker <- ifelse(wei_plot_df_ranker$higher == 'Low', 20000+(wei_plot_df_ranker$disparity),
		10000+(wei_plot_df_ranker$disparity))
	wei_plot_df_ranker <- left_join(wei_plot_df_ranker, st_pct_lowincome)
	wei_plot_df_ranker$ranker <- ifelse(wei_plot_df_ranker$pct_low_income < 20, (wei_plot_df_ranker$ranker),
		1000+wei_plot_df_ranker$ranker)
	wei_plot_df_ranker <- dplyr::select(ungroup(wei_plot_df_ranker), STNAME, ranker)
	wei_plot_df_plot <- left_join(wei_plot_df, wei_plot_df_ranker) %>%
		filter(!is.na(STNAME), STNAME != 'District of Columbia')

	wei_plot_df_plot_v2 <- wei_plot_df_plot %>%
		dplyr::select(STNAME, Scenario, disparity, ranker) %>%
		mutate(disparity = (disparity)) %>%
		filter(Scenario %in% c('REF2050', 'NZ2050')) %>%
		pivot_wider(names_from = Scenario, values_from = disparity) %>%
		filter(!is.na(STNAME))
	
	wei_plot_df_plot_v2$direction <- ifelse(wei_plot_df_plot_v2$NZ2050 > wei_plot_df_plot_v2$REF2050,
		'increasing', 'decreasing')
	
	gg_wei2 <- ggplot(wei_plot_df_plot_v2) +
					geom_vline(xintercept = 0) +
		geom_segment(aes(x = REF2050, xend = NZ2050, y = fct_reorder(STNAME, REF2050)), 
			arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
		geom_point(data = filter(wei_plot_df_plot, Scenario == 'REF2050', !is.na(STNAME)),
			aes(x = (disparity), y = fct_reorder(STNAME, disparity))) +
		theme_bw() +
		scale_color_manual(values = c('blue', 'red')) 

	xlab_text <- paste0('Low Income - High Income Disparity in ', poll, ' Exposure')

	if (poll == 'Ozone') {
		gg_wei2 <- gg_wei2 +
			ggtitle("d) Ozone concentration disparity: Low Income - High Income") +
			xlab('ppb') +
			ylab('') +
			theme(legend.position = 'none') 
	} else {
		gg_wei2 <- gg_wei2 +
			ggtitle("c) PM2.5 concentration disparity: Low Income - High Income") +
			xlab(bquote(μg/m^3)) +
			ylab('') +
			theme(legend.position = 'none')

	}

	return(list(gg_wei2))
}

get_wei_plot_urban <- function(demo, poll) {
	demo_df <- demo
	st_pct_rural <- demo_df %>%
		group_by(STNAME) %>%
		summarize(rural = sum(rural, na.rm = TRUE), total = sum(total, na.rm = TRUE)) %>%
		mutate(pct_rural = 100*(rural/total))

	urban_wt_df <- demo_df %>%
		dplyr::select(GEOID, STNAME, `2017`, REF2050, NZ2050, NZ2050RCP, newNZ2050, RCP45NZ2050, equityNZ2050scaled,
			urban, rural, total) %>%
		pivot_longer(cols = c('urban', 'rural', 'total'), names_to = "urban",
			values_to = 'count') %>%
		pivot_longer(cols = c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050', 'equityNZ2050scaled'), names_to = "scenario",
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
	wei_plot_df <- urban_wt_df

	wei_plot_df_ranker <- filter(wei_plot_df, scenario == 'REF2050')
	wei_plot_df_ranker$higher <- ifelse(wei_plot_df_ranker$Rural > wei_plot_df_ranker$Urban, 'Rural', 'Urban')
	wei_plot_df_ranker$ranker <- ifelse(wei_plot_df_ranker$higher == 'Rural', 20000+(wei_plot_df_ranker$disparity),
		10000+(wei_plot_df_ranker$disparity))
	wei_plot_df_ranker <- left_join(wei_plot_df_ranker, st_pct_rural)
	wei_plot_df_ranker$ranker <- ifelse(wei_plot_df_ranker$pct_rural < 14.2, (wei_plot_df_ranker$ranker),
		1000+wei_plot_df_ranker$ranker)
	wei_plot_df_ranker <- dplyr::select(ungroup(wei_plot_df_ranker), STNAME, ranker)
	wei_plot_df_plot <- left_join(wei_plot_df, wei_plot_df_ranker) %>%
		filter(!is.na(STNAME), STNAME != 'District of Columbia')

	wei_plot_df_plot_v2 <- wei_plot_df_plot %>%
		dplyr::select(STNAME, scenario, disparity, ranker) %>%
		mutate(disparity = (disparity)) %>%
		filter(scenario %in% c('REF2050', 'NZ2050')) %>%
		pivot_wider(names_from = scenario, values_from = disparity) %>%
		filter(!is.na(STNAME))
	
	wei_plot_df_plot_v2$direction <- ifelse(wei_plot_df_plot_v2$NZ2050 > wei_plot_df_plot_v2$REF2050,
		'increasing', 'decreasing')
	
	gg_wei2 <- ggplot(wei_plot_df_plot_v2) +
				geom_vline(xintercept = 0) +
		geom_segment(aes(x = REF2050, xend = NZ2050, y = fct_reorder(STNAME, REF2050)), 
			arrow = arrow(type = 'closed', length = unit(0.015, 'npc'))) +
		geom_point(data = filter(wei_plot_df_plot, scenario == 'REF2050', !is.na(STNAME)),
			aes(x = (disparity), y = fct_reorder(STNAME, disparity))) +
		theme_bw() +
		scale_color_manual(values = c('blue', 'red')) 

	xlab_text <- paste0('Rural - Urban Income Disparity in ', poll, ' Exposure')

	if (poll == 'Ozone') {
		gg_wei2 <- gg_wei2 +
			ggtitle("f) Ozone concentration disparity: Rural - Urban") +
			xlab('ppb') +
			ylab('') +
			theme(legend.position = 'none')

	} else {
		gg_wei2 <- gg_wei2 +
			ggtitle("e) PM2.5 concentration disparity: Rural - Urban") +
			xlab(bquote(μg/m^3)) +
			ylab('') +
			theme(legend.position = 'none')
	}

	return(list(gg_wei2))
}

demo_list <- get_demo_df()
oz_demo <- process_poll_df(demo_list = demo_list, pollutant_df = all_oz_new)
pm_demo <- process_poll_df(demo_list = demo_list, pollutant_df = all_pm_new)

gg_oz_race <- get_wei_plot_race(oz_demo, poll = 'Ozone') 
gg_pm_race <- get_wei_plot_race(pm_demo, poll = 'PM2.5') 
gg_plotlist_race <- c(gg_pm_race, gg_oz_race)

gg_oz_income <- get_wei_plot_income(oz_demo, poll = 'Ozone')
gg_pm_income <- get_wei_plot_income(pm_demo, poll = 'PM2.5')
gg_plotlist_income <- c(gg_pm_income, gg_oz_income)

gg_oz_urban <- get_wei_plot_urban(oz_demo, poll = 'Ozone')
gg_pm_urban <- get_wei_plot_urban(pm_demo, poll = 'PM2.5')
gg_plotlist_urban <- c(gg_pm_urban, gg_oz_urban)

jpeg(paste0('../air_health/plots/rtr_figs', '/rtr_fig5_si1_', Sys.Date(), '.jpg'), width = 16, height = 16, quality = 1, units = 'in', res = 300)
ggarrange(plotlist = c(gg_plotlist_race, gg_plotlist_income, gg_plotlist_urban), ncol = 2, nrow = 3)
dev.off()