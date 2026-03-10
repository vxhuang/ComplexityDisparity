options(tigris_use_cache = TRUE)
libs <- c('tidyverse', 'sf', 'raster', 'tidycensus', 'viridis', 'ggpubr')
lapply(libs, library, character.only = TRUE)
# set to local directory where the repo is stored
setwd(repo_directory)

all_data_new <- read.csv("./input_data/combined_data.csv")
mydat <- all_data_new

county_sf <- get_acs(variables = c('B01003_001'), 
                     year = 2017, geography = 'county', geometry = TRUE)

plot_dat_pm <- mydat %>%
  dplyr::select(-X) %>%
  rename(GEOID = FIPS) %>%
  filter(SCENARIO %in% c(
    'ref2017', 'nz2050_2017', 'ref2050_2017', 'nz_equity_2017', 'elec_2017'
  ), 
         POLLUTANT == 'pm25', TIMESCALE == 'annual')
plot_dat_pm <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_pm)

plot_dat_popw_pm <- as.data.frame(plot_dat_pm) %>%
  group_by(SCENARIO) %>%
  summarize(exposure = round(weighted.mean(x = VALUE, w = estimate, na.rm = TRUE), 1))

pm_lims <- quantile(plot_dat_pm$VALUE, c(0.05, 0.95), na.rm = TRUE)
plot_dat_pm$VALUE <- ifelse(plot_dat_pm$VALUE > max(pm_lims), max(pm_lims),
                            ifelse(plot_dat_pm$VALUE < min(pm_lims), min(pm_lims), plot_dat_pm$VALUE))
plot_dat_pm <- filter(plot_dat_pm, !is.na(VALUE))
plot_dat_pm_17 <- filter(plot_dat_pm, SCENARIO == 'ref2017')
plot_dat_pm_50 <- filter(plot_dat_pm, SCENARIO != 'ref2017') %>%
  dplyr::select(-VERSION) %>%
  pivot_wider(names_from = 'SCENARIO', values_from = 'VALUE')
plot_dat_pm_50 <- left_join(plot_dat_pm_17, as.data.frame(plot_dat_pm_50)) %>%
  mutate(ref2050_2017 = ref2050_2017 - VALUE,
         nz2050_2017 = nz2050_2017 - VALUE,
         elec_2017 = elec_2017 - VALUE,
         nz_equity_2017 = nz_equity_2017 - VALUE) %>%
  dplyr::select(-VALUE, -SCENARIO) %>%
  pivot_longer(cols = all_of(c(
    'nz2050_2017', 'ref2050_2017', 'nz_equity_2017', 'elec_2017')),
               names_to = 'SCENARIO', values_to = 'VALUE')

plot_dat_oz <- mydat %>%
  dplyr::select(-X) %>%
  rename(GEOID = FIPS) %>%
  filter(SCENARIO %in% c(    
    'ref2017', 'nz2050_2017', 'ref2050_2017', 'nz_equity_2017', 'elec_2017'), 
         POLLUTANT == 'ozone', TIMESCALE == 'july')
plot_dat_oz <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), plot_dat_oz)

plot_dat_popw_oz <- as.data.frame(plot_dat_oz) %>%
  group_by(SCENARIO) %>%
  summarize(exposure = round(weighted.mean(x = VALUE, w = estimate, na.rm = TRUE), 1))

oz_lims <- quantile(plot_dat_oz$VALUE, c(0.05, 0.95), na.rm = TRUE)
plot_dat_oz$VALUE <- ifelse(plot_dat_oz$VALUE > max(oz_lims), max(oz_lims),
                            ifelse(plot_dat_oz$VALUE < min(oz_lims), min(oz_lims), plot_dat_oz$VALUE))
plot_dat_oz <- filter(plot_dat_oz, !is.na(VALUE))

plot_dat_oz_17 <- filter(plot_dat_oz, SCENARIO == 'ref2017')
plot_dat_oz_50 <- filter(plot_dat_oz, SCENARIO != 'ref2017') %>%
  dplyr::select(-VERSION) %>%
  pivot_wider(names_from = 'SCENARIO', values_from = 'VALUE')
plot_dat_oz_50 <- left_join(plot_dat_oz_17, as.data.frame(plot_dat_oz_50)) %>%
  mutate(ref2050_2017 = ref2050_2017 - VALUE,
         nz2050_2017 = nz2050_2017 - VALUE,
         elec_2017 = elec_2017 - VALUE,
         nz_equity_2017 = nz_equity_2017 - VALUE) %>%
  dplyr::select(-VALUE, -SCENARIO) %>%
  pivot_longer(cols = all_of(c(
      'nz2050_2017', 'ref2050_2017', 'nz_equity_2017', 'elec_2017')),
               names_to = 'SCENARIO', values_to = 'VALUE')

diff_plot_limits_pm <- quantile(c(
  filter(plot_dat_pm_50, SCENARIO == 'ref2050_2017')$VALUE,
  filter(plot_dat_pm_50, SCENARIO == 'nz2050_2017')$VALUE,
  filter(plot_dat_pm_50, SCENARIO == 'nz_equity_2017')$VALUE,
  filter(plot_dat_pm_50, SCENARIO == 'elec_2017')$VALUE), c(0.025, 0.975), na.rm = TRUE)
diff_plot_limits_pm <- round(diff_plot_limits_pm, 2)
plot_dat_pm_50$VALUE <- ifelse(plot_dat_pm_50$VALUE > max(diff_plot_limits_pm), max(diff_plot_limits_pm),
                               ifelse(plot_dat_pm_50$VALUE < min(diff_plot_limits_pm), min(diff_plot_limits_pm), plot_dat_pm_50$VALUE))

diff_plot_limits_oz <- quantile(c(
  filter(plot_dat_oz_50, SCENARIO == 'ref2050_2017')$VALUE,
  filter(plot_dat_oz_50, SCENARIO == 'nz2050_2017')$VALUE,
  filter(plot_dat_oz_50, SCENARIO == 'nz_equity_2017')$VALUE,
  filter(plot_dat_oz_50, SCENARIO == 'elec_2017')$VALUE), c(0.025, 0.975), na.rm = TRUE)
diff_plot_limits_oz <- round(diff_plot_limits_oz, 2)
plot_dat_oz_50$VALUE <- ifelse(plot_dat_oz_50$VALUE > max(diff_plot_limits_oz), max(diff_plot_limits_oz),
                               ifelse(plot_dat_oz_50$VALUE < min(diff_plot_limits_oz), min(diff_plot_limits_oz), plot_dat_oz_50$VALUE))

gg_pm_17 <-ggplot(plot_dat_pm_17) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_viridis(name = 'PM2.5\n(μg/m3)') +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'ref2017')$exposure, ' μg/m³'))

gg_pm_50_ref <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'ref2050_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'PM2.5\n(μg/m3)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_pm) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'ref2050_2017')$exposure, ' μg/m³'))

gg_pm_50_nz <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'nz2050_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'PM2.5\n(μg/m3)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_pm) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'nz2050_2017')$exposure, ' μg/m³'))

gg_pm_50_nzh <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'elec_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'PM2.5\n(μg/m3)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_pm) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'elec_2017')$exposure, ' μg/m³'))

gg_pm_50_newnz <-ggplot(filter(plot_dat_pm_50, SCENARIO == 'nz_equity_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'PM2.5\n(μg/m3)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_pm) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_pm, SCENARIO == 'nz_equity_2017')$exposure, ' μg/m³'))

gg_oz_17 <-ggplot(plot_dat_oz_17) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_viridis(name = 'Ozone\n(ppb)') +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'ref2017')$exposure, ' ppb'))

gg_oz_50_ref <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'ref2050_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'Ozone\n(ppb)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_oz) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'ref2050_2017')$exposure, ' ppb'))

gg_oz_50_nz <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'nz2050_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'Ozone\n(ppb)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_oz) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'nz2050_2017')$exposure, ' ppb'))

gg_oz_50_nzh <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'elec_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'Ozone\n(ppb)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_oz) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'elec_2017')$exposure, ' ppb'))

gg_oz_50_newnz <-ggplot(filter(plot_dat_oz_50, SCENARIO == 'nz_equity_2017')) +
  geom_sf(aes(fill = VALUE), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_gradient2(name = 'Ozone\n(ppb)', midpoint = 0, low = '#1c1c84', high = '#ff79c2', mid = '#f3f2f2',
                       limits = diff_plot_limits_oz) +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  theme(legend.position = 'right') +
  xlab(paste0('Population-weighted concentration: ', filter(plot_dat_popw_oz, SCENARIO == 'nz_equity_2017')$exposure, ' ppb'))

plot_dat_both_50_ref <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'ref2050_2017'),
                                  filter(plot_dat_pm_50, SCENARIO == 'ref2050_2017')) %>%
  dplyr::select(-TIMESCALE) %>%
  pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_ref$plot_var <- ifelse(plot_dat_both_50_ref$pm25 < -0.05 & plot_dat_both_50_ref$ozone < -0.1, 'Both decrease',
                                        ifelse(plot_dat_both_50_ref$pm25 > 0.05 & plot_dat_both_50_ref$ozone < -0.1, 'PM2.5 increases & ozone decreases',
                                               ifelse(plot_dat_both_50_ref$pm25 < -0.05 & plot_dat_both_50_ref$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
                                                      ifelse(plot_dat_both_50_ref$pm25 > 0.05 & plot_dat_both_50_ref$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_ref <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), as.data.frame(plot_dat_both_50_ref))
gg_both_50_ref <- ggplot(plot_dat_both_50_ref) +
  geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
                               'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
  theme(legend.position = 'right') +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

plot_dat_both_50_nz <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'nz2050_2017'),
                                 filter(plot_dat_pm_50, SCENARIO == 'nz2050_2017')) %>%
  dplyr::select(-TIMESCALE) %>%
  pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_nz$plot_var <- ifelse(plot_dat_both_50_nz$pm25 < -0.05 & plot_dat_both_50_nz$ozone < -0.1, 'Both decrease',
                                       ifelse(plot_dat_both_50_nz$pm25 > 0.05 & plot_dat_both_50_nz$ozone < -0.1, 'PM2.5 increases & ozone decreases',
                                              ifelse(plot_dat_both_50_nz$pm25 < -0.05 & plot_dat_both_50_nz$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
                                                     ifelse(plot_dat_both_50_nz$pm25 > 0.05 & plot_dat_both_50_nz$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_nz <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), as.data.frame(plot_dat_both_50_nz))
gg_both_50_nz <- ggplot(plot_dat_both_50_nz) +
  geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
                               'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
  theme(legend.position = 'right') +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

plot_dat_both_50_newnz <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'elec_2017'),
                                    filter(plot_dat_pm_50, SCENARIO == 'elec_2017')) %>%
  dplyr::select(-TIMESCALE) %>%
  pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_newnz$plot_var <- ifelse(plot_dat_both_50_newnz$pm25 < -0.05 & plot_dat_both_50_newnz$ozone < -0.1, 'Both decrease',
                                          ifelse(plot_dat_both_50_newnz$pm25 > 0.05 & plot_dat_both_50_newnz$ozone < -0.1, 'PM2.5 increases & ozone decreases',
                                                 ifelse(plot_dat_both_50_newnz$pm25 < -0.05 & plot_dat_both_50_newnz$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
                                                        ifelse(plot_dat_both_50_newnz$pm25 > 0.05 & plot_dat_both_50_newnz$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_newnz <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), as.data.frame(plot_dat_both_50_newnz))
gg_both_50_newnz <- ggplot(plot_dat_both_50_newnz) +
  geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
                               'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
  theme(legend.position = 'right') +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

plot_dat_both_50_nzh <- bind_rows(filter(plot_dat_oz_50, SCENARIO == 'nz_equity_2017'),
                                  filter(plot_dat_pm_50, SCENARIO == 'nz_equity_2017')) %>%
  dplyr::select(-TIMESCALE) %>%
  pivot_wider(names_from = 'POLLUTANT', values_from = 'VALUE')
plot_dat_both_50_nzh$plot_var <- ifelse(plot_dat_both_50_nzh$pm25 < -0.05 & plot_dat_both_50_nzh$ozone < -0.1, 'Both decrease',
                                        ifelse(plot_dat_both_50_nzh$pm25 > 0.05 & plot_dat_both_50_nzh$ozone < -0.1, 'PM2.5 increases & ozone decreases',
                                               ifelse(plot_dat_both_50_nzh$pm25 < -0.05 & plot_dat_both_50_nzh$ozone > 0.1, 'PM2.5 decreases & ozone increases', 
                                                      ifelse(plot_dat_both_50_nzh$pm25 > 0.05 & plot_dat_both_50_nzh$ozone > 0.1, 'Both increase', 'No major changes'))))
plot_dat_both_50_nzh <- left_join(mutate(county_sf, GEOID = as.numeric(GEOID)), as.data.frame(plot_dat_both_50_nzh))
gg_both_50_nzh <- ggplot(plot_dat_both_50_nzh) +
  geom_sf(aes(fill = as.factor(plot_var)), lwd = 0) +
  theme_bw(base_size = 13) +
  xlim(-125, -65) +
  ylim(23, 50) +
  scale_fill_manual(values = c("No major changes" = "dark gray", "Both decrease" = "#1b9e77", 'Both increase' = '#e7298a', 
                               'PM2.5 decreases & ozone increases' = '#e6ab02', 'PM2.5 increases & ozone decreases' = '#d95f02'), name = '') +
  theme(legend.position = 'right') +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

blank <- ggplot() + theme_void()

theme(panel.border = element_blank())

jpeg(paste0("./outputs/fig2_", 
            Sys.Date(), '.jpg'), width = 10, height = 15, units = 'in', res = 300)
print(
  ggarrange(gg_pm_17 + theme(panel.border = element_blank()) + ggtitle('a) 2017, PM2.5'), 
            gg_oz_17 + theme(panel.border = element_blank()) + ggtitle('b) 2017, Ozone'), 
            gg_pm_50_ref + theme(panel.border = element_blank()) + ggtitle('c) Reference 2050 vs. 2017, PM2.5'), 
            gg_oz_50_ref + theme(panel.border = element_blank()) + ggtitle('d) Reference 2050 vs. 2017, Ozone'), 
            gg_pm_50_nz + theme(panel.border = element_blank()) + ggtitle('e) Net Zero 2050 vs. 2017, PM2.5'), 
            gg_oz_50_nz + theme(panel.border = element_blank()) + ggtitle('f) Net Zero 2050 vs. 2017, Ozone'), 
            gg_oz_50_nzh + theme(panel.border = element_blank()) + ggtitle('g) Net Zero (ELE) 2050 vs. 2017, PM2.5'), 
            gg_pm_50_nzh + theme(panel.border = element_blank()) + ggtitle('h) Net Zero (ELE) 2050 vs. 2017, Ozone'), 
            
            gg_oz_50_newnz + theme(panel.border = element_blank()) + ggtitle('i) Net Zero (Equity) 2050 vs. 2017, PM2.5'), 
            gg_pm_50_newnz + theme(panel.border = element_blank()) + ggtitle('j) Net Zero (Equity) 2050 vs. 2017, Ozone'), 
            
            nrow = 5, ncol = 2
  )
)
dev.off()


pdf(paste0("./outputs/fig2_", 
            Sys.Date(), '.pdf'), width = 10, height = 15)
print(
  ggarrange(gg_pm_17 + theme(panel.border = element_blank()) + ggtitle('a) 2017, PM2.5'), 
            gg_oz_17 + theme(panel.border = element_blank()) + ggtitle('b) 2017, Ozone'), 
            gg_pm_50_ref + theme(panel.border = element_blank()) + ggtitle('c) Reference 2050 vs. 2017, PM2.5'), 
            gg_oz_50_ref + theme(panel.border = element_blank()) + ggtitle('d) Reference 2050 vs. 2017, Ozone'), 
            gg_pm_50_nz + theme(panel.border = element_blank()) + ggtitle('e) Net Zero 2050 vs. 2017, PM2.5'), 
            gg_oz_50_nz + theme(panel.border = element_blank()) + ggtitle('f) Net Zero 2050 vs. 2017, Ozone'), 
            gg_oz_50_nzh + theme(panel.border = element_blank()) + ggtitle('g) Net Zero (ELE) 2050 vs. 2017, PM2.5'), 
            gg_pm_50_nzh + theme(panel.border = element_blank()) + ggtitle('h) Net Zero (ELE) 2050 vs. 2017, Ozone'), 
            
            gg_oz_50_newnz + theme(panel.border = element_blank()) + ggtitle('i) Net Zero (Equity) 2050 vs. 2017, PM2.5'), 
            gg_pm_50_newnz + theme(panel.border = element_blank()) + ggtitle('j) Net Zero (Equity) 2050 vs. 2017, Ozone'), 
            nrow = 5, ncol = 2
  )
)
dev.off()


svg(paste0("./outputs/fig2_", 
           Sys.Date(), '.svg'), width = 10, height = 15)
print(
  ggarrange(gg_pm_17 + theme(panel.border = element_blank()) + ggtitle('a) 2017, PM2.5'), 
            gg_oz_17 + theme(panel.border = element_blank()) + ggtitle('b) 2017, Ozone'), 
            gg_pm_50_ref + theme(panel.border = element_blank()) + ggtitle('c) Reference 2050 vs. 2017, PM2.5'), 
            gg_oz_50_ref + theme(panel.border = element_blank()) + ggtitle('d) Reference 2050 vs. 2017, Ozone'), 
            gg_pm_50_nz + theme(panel.border = element_blank()) + ggtitle('e) Net Zero 2050 vs. 2017, PM2.5'), 
            gg_oz_50_nz + theme(panel.border = element_blank()) + ggtitle('f) Net Zero 2050 vs. 2017, Ozone'), 
            gg_oz_50_nzh + theme(panel.border = element_blank()) + ggtitle('g) Net Zero (ELE) 2050 vs. 2017, PM2.5'), 
            gg_pm_50_nzh + theme(panel.border = element_blank()) + ggtitle('h) Net Zero (ELE) 2050 vs. 2017, Ozone'), 
            
            gg_oz_50_newnz + theme(panel.border = element_blank()) + ggtitle('i) Net Zero (Equity) 2050 vs. 2017, PM2.5'), 
            gg_pm_50_newnz + theme(panel.border = element_blank()) + ggtitle('j) Net Zero (Equity) 2050 vs. 2017, Ozone'), 
        
            nrow = 5, ncol = 2
  )
)
dev.off()


 
pdf(paste0("./outputs/fig2_scales_", 
            Sys.Date(), '.pdf'), width = 20, height = 10)
print(
  ggarrange(gg_pm_17 + theme(legend.position = 'right') + labs(fill = 'PM2.5\n(μg/m3)'), 
            gg_pm_50_ref,
            gg_pm_50_nz, 
            gg_pm_50_nzh, 
            gg_pm_50_newnz + theme(legend.position = 'right') + labs(fill = 'PM2.5\n(μg/m3)'),
            
            gg_oz_17 + theme(legend.position = 'right') + labs(fill = 'Ozone\n(ppb)'), 
            gg_oz_50_ref,
            gg_oz_50_nz,
            gg_oz_50_nzh, 
            gg_oz_50_newnz + theme(legend.position = 'right') + labs(fill = 'Ozone\n(ppb)'),
            
            blank,
            gg_both_50_ref, 
            gg_both_50_nz,
            gg_both_50_nzh,
            gg_both_50_newnz + theme(legend.position = 'right'),
            blank,
            nrow = 3, ncol = 5)
)
dev.off()
