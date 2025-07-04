options(width = 200)
libs <- c('tidyverse', 'sf', 'raster', 'tidycensus', 'viridis', 'ggpubr')
lapply(libs, library, character.only = TRUE)

mydat <- read.csv('../air_health/data/wrf_data/v0.1/processed_wrf_data.csv')
county_sf <- get_acs(variables = c('B01003_001'), 
		year = 2017, geography = 'county', geometry = TRUE)

plot_dat_pm <- mydat %>%
	dplyr::select(-X) %>%
	rename(GEOID = FIPS) %>%
	filter(SCENARIO %in% c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050'), 
		POLLUTANT == 'pm25', TIMESCALE == 'annual')
plot_dat_pm <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_pm)

plot_dat_popw_pm <- as.data.frame(plot_dat_pm) %>%
	group_by(SCENARIO) %>%
	summarize(exposure = round(weighted.mean(x = VALUE, w = estimate, na.rm = TRUE), 1))

pm_lims <- quantile(plot_dat_pm$VALUE, c(0.05, 0.95), na.rm = TRUE)
plot_dat_pm$VALUE <- ifelse(plot_dat_pm$VALUE > max(pm_lims), max(pm_lims),
	ifelse(plot_dat_pm$VALUE < min(pm_lims), min(pm_lims), plot_dat_pm$VALUE))
plot_dat_pm <- filter(plot_dat_pm, !is.na(VALUE))
plot_dat_pm_17 <- filter(plot_dat_pm, SCENARIO == '2017')
plot_dat_pm_50 <- filter(plot_dat_pm, SCENARIO != '2017') %>%
	dplyr::select(-VERSION) %>%
	pivot_wider(names_from = 'SCENARIO', values_from = 'VALUE')
plot_dat_pm_50 <- left_join(plot_dat_pm_17, plot_dat_pm_50) %>%
	mutate(REF2050 = REF2050 - VALUE,
		NZ2050 = NZ2050 - VALUE,
		NZ2050RCP = NZ2050RCP - VALUE,
		newNZ2050 = newNZ2050 - VALUE,
		RCP45NZ2050 = RCP45NZ2050 - VALUE) %>%
	dplyr::select(-VALUE, -SCENARIO) %>%
	pivot_longer(cols = all_of(c('REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050')),
	 names_to = 'SCENARIO', values_to = 'VALUE')

plot_dat_oz <- mydat %>%
	dplyr::select(-X) %>%
	rename(GEOID = FIPS) %>%
	filter(SCENARIO %in% c('2017', 'REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050'), 
		POLLUTANT == 'ozone', TIMESCALE == 'july')
plot_dat_oz <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_oz)

plot_dat_popw_oz <- as.data.frame(plot_dat_oz) %>%
	group_by(SCENARIO) %>%
	summarize(exposure = round(weighted.mean(x = VALUE, w = estimate, na.rm = TRUE), 1))

oz_lims <- quantile(plot_dat_oz$VALUE, c(0.05, 0.95), na.rm = TRUE)
plot_dat_oz$VALUE <- ifelse(plot_dat_oz$VALUE > max(oz_lims), max(oz_lims),
	ifelse(plot_dat_oz$VALUE < min(oz_lims), min(oz_lims), plot_dat_oz$VALUE))
plot_dat_oz <- filter(plot_dat_oz, !is.na(VALUE))

plot_dat_oz_17 <- filter(plot_dat_oz, SCENARIO == '2017')
plot_dat_oz_50 <- filter(plot_dat_oz, SCENARIO != '2017') %>%
	dplyr::select(-VERSION) %>%
	pivot_wider(names_from = 'SCENARIO', values_from = 'VALUE')
plot_dat_oz_50 <- left_join(plot_dat_oz_17, plot_dat_oz_50) %>%
	mutate(REF2050 = REF2050 - VALUE,
		NZ2050 = NZ2050 - VALUE,
		NZ2050RCP = NZ2050RCP - VALUE,
		newNZ2050 = newNZ2050 - VALUE,
		RCP45NZ2050 = RCP45NZ2050 - VALUE) %>%
	dplyr::select(-VALUE, -SCENARIO) %>%
	pivot_longer(cols = all_of(c('REF2050', 'NZ2050', 'NZ2050RCP', 'newNZ2050', 'RCP45NZ2050')),
	 names_to = 'SCENARIO', values_to = 'VALUE')

diff_plot_limits_pm <- quantile(c(
		filter(plot_dat_pm_50, SCENARIO == 'REF2050')$VALUE,
		filter(plot_dat_pm_50, SCENARIO == 'NZ2050')$VALUE,
		filter(plot_dat_pm_50, SCENARIO == 'newNZ2050')$VALUE,
		filter(plot_dat_pm_50, SCENARIO == 'NZ2050RCP')$VALUE,
		filter(plot_dat_pm_50, SCENARIO == 'RCP45NZ2050')$VALUE), c(0.025, 0.975), na.rm = TRUE)
diff_plot_limits_pm <- round(diff_plot_limits_pm, 2)
plot_dat_pm_50$VALUE <- ifelse(plot_dat_pm_50$VALUE > max(diff_plot_limits_pm), max(diff_plot_limits_pm),
	ifelse(plot_dat_pm_50$VALUE < min(diff_plot_limits_pm), min(diff_plot_limits_pm), plot_dat_pm_50$VALUE))

diff_plot_limits_oz <- quantile(c(
		filter(plot_dat_oz_50, SCENARIO == 'REF2050')$VALUE,
		filter(plot_dat_oz_50, SCENARIO == 'NZ2050')$VALUE,
		filter(plot_dat_oz_50, SCENARIO == 'newNZ2050')$VALUE,
		filter(plot_dat_oz_50, SCENARIO == 'NZ2050RCP')$VALUE,
		filter(plot_dat_oz_50, SCENARIO == 'RCP45NZ2050')$VALUE), c(0.025, 0.975), na.rm = TRUE)
diff_plot_limits_oz <- round(diff_plot_limits_oz, 2)
plot_dat_oz_50$VALUE <- ifelse(plot_dat_oz_50$VALUE > max(diff_plot_limits_oz), max(diff_plot_limits_oz),
	ifelse(plot_dat_oz_50$VALUE < min(diff_plot_limits_oz), min(diff_plot_limits_oz), plot_dat_oz_50$VALUE))

gg_pm_17 <-ggplot(plot_dat_pm_17) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_viridis(name = 'μg/m3') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == '2017')$exposure, ' μg/m³'))

gg_pm_50_ref <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'REF2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'μg/m3', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_pm) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'REF2050')$exposure, ' μg/m³'))

gg_pm_50_nz <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'NZ2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'μg/m3', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_pm) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'NZ2050')$exposure, ' μg/m³'))

gg_pm_50_nzh <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'NZ2050RCP')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'μg/m3', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_pm) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'NZ2050RCP')$exposure, ' μg/m³'))

gg_pm_50_newnz <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'newNZ2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'μg/m3', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_pm) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'newNZ2050')$exposure, ' μg/m³'))

gg_pm_50_nz45 <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'RCP45NZ2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'μg/m3', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_pm) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'RCP45NZ2050')$exposure, ' μg/m³'))

gg_oz_17 <-ggplot(plot_dat_oz_17) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_viridis(name = 'ppb') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == '2017')$exposure, ' ppb'))

gg_oz_50_ref <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'REF2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'ppb', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_oz) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'REF2050')$exposure, ' ppb'))

gg_oz_50_nz <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'NZ2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'ppb', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_oz) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'NZ2050')$exposure, ' ppb'))

gg_oz_50_nzh <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'NZ2050RCP')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'ppb', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_oz) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'NZ2050RCP')$exposure, ' ppb'))

gg_oz_50_newnz <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'newNZ2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'ppb', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_oz) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'newNZ2050')$exposure, ' ppb'))

gg_oz_50_nz45 <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'RCP45NZ2050')) +
	geom_sf(aes(fill = VALUE), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_gradient2(name = 'ug/m3', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
		limits = diff_plot_limits_oz) +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank()) +
	xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'RCP45NZ2050')$exposure, ' ppb'))

plot_dat_both_50_ref <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'REF2050'),
	filter(plot_dat_pm_50, SCENARIO == 'REF2050')) %>%
	dplyr::select(-TIMESCALE) %>%
	pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_ref$plot_var <- ifelse(plot_dat_both_50_ref$pm25 < -0.05 & plot_dat_both_50_ref$ozone < -0.1, 'Both decrease',
	ifelse(plot_dat_both_50_ref$pm25 > 0.05 & plot_dat_both_50_ref$ozone < -0.1, 'PM2.5 increases & ozone decreases',
		ifelse(plot_dat_both_50_ref$pm25 < -0.05 & plot_dat_both_50_ref$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
			ifelse(plot_dat_both_50_ref$pm25 > 0.05 & plot_dat_both_50_ref$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_ref <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_both_50_ref)
gg_both_50_ref <- ggplot(plot_dat_both_50_ref) +
	geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
		'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
	theme(legend.position = 'none') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank())

plot_dat_both_50_nz <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'NZ2050'),
	filter(plot_dat_pm_50, SCENARIO == 'NZ2050')) %>%
	dplyr::select(-TIMESCALE) %>%
	pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_nz$plot_var <- ifelse(plot_dat_both_50_nz$pm25 < -0.05 & plot_dat_both_50_nz$ozone < -0.1, 'Both decrease',
	ifelse(plot_dat_both_50_nz$pm25 > 0.05 & plot_dat_both_50_nz$ozone < -0.1, 'PM2.5 increases & ozone decreases',
		ifelse(plot_dat_both_50_nz$pm25 < -0.05 & plot_dat_both_50_nz$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
			ifelse(plot_dat_both_50_nz$pm25 > 0.05 & plot_dat_both_50_nz$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_nz <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_both_50_nz)
gg_both_50_nz <- ggplot(plot_dat_both_50_nz) +
	geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
		'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
	theme(legend.position = 'none') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank())

plot_dat_both_50_newnz <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'newNZ2050'),
	filter(plot_dat_pm_50, SCENARIO == 'newNZ2050')) %>%
	dplyr::select(-TIMESCALE) %>%
	pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_newnz$plot_var <- ifelse(plot_dat_both_50_newnz$pm25 < -0.05 & plot_dat_both_50_newnz$ozone < -0.1, 'Both decrease',
	ifelse(plot_dat_both_50_newnz$pm25 > 0.05 & plot_dat_both_50_newnz$ozone < -0.1, 'PM2.5 increases & ozone decreases',
		ifelse(plot_dat_both_50_newnz$pm25 < -0.05 & plot_dat_both_50_newnz$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
			ifelse(plot_dat_both_50_newnz$pm25 > 0.05 & plot_dat_both_50_newnz$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_newnz <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_both_50_newnz)
gg_both_50_newnz <- ggplot(plot_dat_both_50_newnz) +
	geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
		'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
	theme(legend.position = 'none') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank())

plot_dat_both_50_nzh <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'NZ2050RCP'),
	filter(plot_dat_pm_50, SCENARIO == 'NZ2050RCP')) %>%
	dplyr::select(-TIMESCALE) %>%
	pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_nzh$plot_var <- ifelse(plot_dat_both_50_nzh$pm25 < -0.05 & plot_dat_both_50_nzh$ozone < -0.1, 'Both decrease',
	ifelse(plot_dat_both_50_nzh$pm25 > 0.05 & plot_dat_both_50_nzh$ozone < -0.1, 'PM2.5 increases & ozone decreases',
		ifelse(plot_dat_both_50_nzh$pm25 < -0.05 & plot_dat_both_50_nzh$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
			ifelse(plot_dat_both_50_nzh$pm25 > 0.05 & plot_dat_both_50_nzh$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_nzh <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_both_50_nzh)
gg_both_50_nzh <- ggplot(plot_dat_both_50_nzh) +
	geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
		'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
	theme(legend.position = 'none') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank())


# - No major changes: White
# - Both decrease: Current green
# - Both increase: Current red
# - PM2.5 decrease and ozone increase: yellow
# - PM2.5 decrease and ozone increase: orange

plot_dat_both_50_nz45 <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'RCP45NZ2050'),
	filter(plot_dat_pm_50, SCENARIO == 'RCP45NZ2050')) %>%
	dplyr::select(-TIMESCALE) %>%
	pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_nz45$plot_var <- ifelse(
	plot_dat_both_50_nz45$pm25 < -0.05 & plot_dat_both_50_nz45$ozone < -0.1, 'Both decrease',
	ifelse(plot_dat_both_50_nz45$pm25 > 0.05 & plot_dat_both_50_nz45$ozone < -0.1, 'PM2.5 increases & ozone decreases',
		ifelse(plot_dat_both_50_nz45$pm25 < -0.05 & plot_dat_both_50_nz45$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
			ifelse(plot_dat_both_50_nz45$pm25 > 0.05 & plot_dat_both_50_nz45$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_nz45 <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_both_50_nz45)
gg_both_50_nz45 <- ggplot(plot_dat_both_50_nz45) +
	geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
	theme_bw() +
	xlim(-125, -65) +
	ylim(23, 50) +
	scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
		'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
	theme(legend.position = 'none') +
	theme(panel.grid = element_blank(),
		axis.text = element_blank(),
		axis.ticks = element_blank())


blank <- ggplot() + theme_void()

jpeg(paste0('../air_health/plots/rtr_figs/fig2_', 
	Sys.Date(), '.jpg'), width = 16, height = 8, units = 'in', res = 300)
print(
	ggarrange(gg_pm_17, gg_pm_50_ref, gg_pm_50_nz, gg_pm_50_nzh,
		gg_oz_17, gg_oz_50_ref, gg_oz_50_nz, gg_oz_50_nzh,
		blank, gg_both_50_ref, gg_both_50_nz, gg_both_50_nzh, nrow = 3, ncol = 4)
		# labels = list('a)', 'b)', 'C', 'D', 'E', 'F', 'G', 'H', '', 'I', 'J', 'K'))
	)
dev.off()