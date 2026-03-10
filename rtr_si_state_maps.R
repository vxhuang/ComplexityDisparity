library(viridis)
library(tidycensus)
library(raster)
library(sf)
library(tidyverse)
library(exactextractr)
library(ggridges)
library(forcats)
library(ggpubr)
library(viridis)

# set to local directory where the repo is stored
setwd(repo_directory)

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

county_sf <- get_acs(variables = c('B01003_001'), 
                     year = 2017, geography = 'county', geometry = TRUE)
county_sf <- county_sf %>%
  dplyr::select(-NAME, -variable, -estimate, -moe) %>%
  rename(FIPS = GEOID) %>%
  mutate(FIPS = as.numeric(FIPS))
all_dat <- left_join(mydat, fips) %>%
  left_join(demo_df)
alldat_sf <- left_join(county_sf, all_dat)

scenario_cols <- c('ref2017',
                   'nz2050_2017')

alldat_change <- filter(all_dat, 
                        SCENARIO %in% scenario_cols) %>%
  dplyr::select(FIPS, VALUE, STNAME, POLLUTANT, TIMESCALE, SCENARIO) %>%
  pivot_wider(values_from = VALUE, names_from = SCENARIO) %>%
  mutate(change = nz2050_2017 - ref2017)
alldat_change_sf <-left_join(county_sf, alldat_change)

glimpse(all_dat)

all_dat_sum <- all_dat %>%
  group_by(STNAME, SCENARIO, POLLUTANT, TIMESCALE) %>%
  summarize(black_avg = weighted.mean(x = VALUE, w = black),
            white_avg = weighted.mean(x = VALUE, w = white))

pm_plot <- ggplot(filter(all_dat_sum, POLLUTANT == 'pm25', TIMESCALE == 'annual',
                         STNAME %in% c('Washington'), 
                         SCENARIO %in% scenario_cols)) +
  geom_bar(aes(x = SCENARIO, y = black_avg - white_avg), fill = 'red', col = 'black',
           stat = 'identity') +
  theme_bw() +
  facet_wrap(. ~ STNAME, scales = 'free_y') +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


all_dat_sf_demo <- dplyr::select(all_dat, FIPS, STNAME, black, white, total) %>%
  distinct() %>%
  group_by(STNAME) %>%
  mutate(pct_black = black/sum(black), pct_white = white/sum(white))
all_dat_sf_demo <- left_join(county_sf, all_dat_sf_demo)

##################
al_black <- ggplot(filter(all_dat_sf_demo, STNAME == 'Washington')) +
  geom_sf(aes(fill = pct_black), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('a) Proportion of State Black Population\n ')

al_white <- ggplot(filter(all_dat_sf_demo, STNAME == 'Washington')) +
  geom_sf(aes(fill = pct_white), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('b) Proportion of State White Population\n ')

al_pm_2017 <- ggplot(filter(alldat_sf, STNAME == 'Washington',
                            POLLUTANT == 'pm25', TIMESCALE == 'annual',
                            SCENARIO %in% c('ref2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'µg/m³        ', option = 'A') +
  ggtitle('c) PM2.5 Concentration\n(2017)')

al_pm_nz2050 <- ggplot(filter(alldat_sf, STNAME == 'Washington',
                              POLLUTANT == 'pm25', TIMESCALE == 'annual',
                              SCENARIO %in% c('nz2050_2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'µg/m³        ', option = 'A') +
  ggtitle('d) PM2.5 Concentration\n(Net-Zero 2050) ')

al_pm_nz50_change <- ggplot(filter(alldat_change_sf, STNAME == 'Washington',
                                   POLLUTANT == 'pm25', TIMESCALE == 'annual')) +
  geom_sf(aes(fill = change), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(name = 'µg/m³        ', 
                       low = 'blue', mid = 'yellow', high = 'red',
                       midpoint = 0) +
  ggtitle('e) Change in PM2.5 Concentration:\nNet-Zero 2050 - 2017')

gg_blank <- ggplot() +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pdf("./outputs/wa_maps.pdf",
    height = 8.5, width = 11)
print(
  ggarrange(plotlist = list(al_black, al_white, gg_blank, al_pm_2017, al_pm_nz2050, al_pm_nz50_change), 
            nrow = 2, ncol = 3)
)
dev.off()

##################
value_vec <- filter(alldat_sf, STNAME == 'New York',
                    POLLUTANT == 'ozone', TIMESCALE == 'july',
                    SCENARIO %in% c('ref2017', 'nz2050_2017'))$VALUE
limit_vec <- c(min(value_vec), max(value_vec))

al_black <- ggplot(filter(all_dat_sf_demo, STNAME == 'New York')) +
  geom_sf(aes(fill = pct_black), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('a) Proportion of State Black Population\n ')

al_white <- ggplot(filter(all_dat_sf_demo, STNAME == 'New York')) +
  geom_sf(aes(fill = pct_white), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('b) Proportion of State White Population\n ')

al_pm_2017 <- ggplot(filter(alldat_sf, STNAME == 'New York',
                            POLLUTANT == 'pm25', TIMESCALE == 'annual',
                            SCENARIO %in% c('ref2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'µg/m³        ', option = 'A') +
  ggtitle('c) PM2.5 Concentration\n(2017)', limits = limit_vec)

al_pm_nz2050 <- ggplot(filter(alldat_sf, STNAME == 'New York',
                              POLLUTANT == 'pm25', TIMESCALE == 'annual',
                              SCENARIO %in% c('nz2050_2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'µg/m³        ', option = 'A') +
  ggtitle('d) PM2.5 Concentration\n(Net-Zero 2050) ', limits = limit_vec)

al_pm_nz50_change <- ggplot(filter(alldat_change_sf, STNAME == 'New York',
                                   POLLUTANT == 'pm25', TIMESCALE == 'annual')) +
  geom_sf(aes(fill = change), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(name = 'µg/m³        ', 
                       low = 'blue', mid = 'yellow', high = 'red',
                       midpoint = 0) +
  ggtitle('e) Change in PM2.5 Concentration:\nNet-Zero 2050 - 2017')

gg_blank <- ggplot() +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pdf("./outputs/ny_pm_maps.pdf",
    height = 8.5, width = 11)
print(
  ggarrange(plotlist = list(al_black, al_white, gg_blank, al_pm_2017, al_pm_nz2050, al_pm_nz50_change), 
            nrow = 2, ncol = 3)
)
dev.off()

##################
value_vec <- filter(alldat_sf, STNAME == 'New York',
                    POLLUTANT == 'ozone', TIMESCALE == 'july',
                    SCENARIO %in% c('ref2017', 'nz2050_2017'))$VALUE
limit_vec <- c(min(value_vec), max(value_vec))

al_black <- ggplot(filter(all_dat_sf_demo, STNAME == 'New York')) +
  geom_sf(aes(fill = pct_black), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('a) Proportion of State Black Population\n ')

al_white <- ggplot(filter(all_dat_sf_demo, STNAME == 'New York')) +
  geom_sf(aes(fill = pct_white), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('b) Proportion of State White Population\n ')

al_pm_2017 <- ggplot(filter(alldat_sf, STNAME == 'New York',
                            POLLUTANT == 'ozone', TIMESCALE == 'july',
                            SCENARIO %in% c('ref2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'ppb        ', option = 'A') +
  ggtitle('c) Ozone Concentration\n(2017)', limits = limit_vec)

al_pm_nz2050 <- ggplot(filter(alldat_sf, STNAME == 'New York',
                              POLLUTANT == 'ozone', TIMESCALE == 'july',
                              SCENARIO %in% c('nz2050_2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'ppb        ', option = 'A') +
  ggtitle('d) Ozone Concentration\n(Net-Zero 2050) ', limits = limit_vec)

al_pm_nz50_change <- ggplot(filter(alldat_change_sf, STNAME == 'New York',
                                   POLLUTANT == 'ozone', TIMESCALE == 'july')) +
  geom_sf(aes(fill = change), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(name = 'ppb        ', 
                       low = 'blue', mid = 'yellow', high = 'red',
                       midpoint = 0) +
  ggtitle('e) Change in Ozone Concentration:\nNet-Zero 2050 - 2017')

gg_blank <- ggplot() +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pdf("./outputs/ny_oz_maps.pdf",
    height = 8.5, width = 11)
print(
  ggarrange(plotlist = list(al_black, al_white, gg_blank, al_pm_2017, al_pm_nz2050, al_pm_nz50_change), 
            nrow = 2, ncol = 3)
)
dev.off()


##################
value_vec <- filter(alldat_sf, STNAME == 'Alabama',
                    POLLUTANT == 'pm25', TIMESCALE == 'annual',
                    SCENARIO %in% c('ref2017', 'nz2050_2017'))$VALUE
limit_vec <- c(min(value_vec), max(value_vec))

al_black <- ggplot(filter(all_dat_sf_demo, STNAME == 'Alabama')) +
  geom_sf(aes(fill = pct_black), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('a) Proportion of State Black Population\n ')

al_white <- ggplot(filter(all_dat_sf_demo, STNAME == 'Alabama')) +
  geom_sf(aes(fill = pct_white), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('b) Proportion of State White Population\n ')

al_pm_2017 <- ggplot(filter(alldat_sf, STNAME == 'Alabama',
                            POLLUTANT == 'pm25', TIMESCALE == 'annual',
                            SCENARIO %in% c('ref2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'µg/m³        ', option = 'A') +
  ggtitle('c) PM2.5 Concentration\n(2017)', limits = limit_vec)

al_pm_nz2050 <- ggplot(filter(alldat_sf, STNAME == 'Alabama',
                              POLLUTANT == 'pm25', TIMESCALE == 'annual',
                              SCENARIO %in% c('nz2050_2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'µg/m³        ', option = 'A') +
  ggtitle('d) PM2.5 Concentration\n(Net-Zero 2050) ', limits = limit_vec)

al_pm_nz50_change <- ggplot(filter(alldat_change_sf, STNAME == 'Alabama',
                                   POLLUTANT == 'pm25', TIMESCALE == 'annual')) +
  geom_sf(aes(fill = change), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(name = 'µg/m³        ', 
                       low = 'blue', mid = 'yellow', high = 'red',
                       midpoint = 0) +
  ggtitle('e) Change in PM2.5 Concentration:\nNet-Zero 2050 - 2017')

gg_blank <- ggplot() +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pdf("./outputs/al_pm_maps.pdf",
    height = 8.5, width = 11)
print(
  ggarrange(plotlist = list(al_black, al_white, gg_blank, al_pm_2017, al_pm_nz2050, al_pm_nz50_change), 
            nrow = 2, ncol = 3)
)
dev.off()

##################
value_vec <- filter(alldat_sf, STNAME == 'New Jersey',
                    POLLUTANT == 'ozone', TIMESCALE == 'july',
                    SCENARIO %in% c('ref2017', 'nz2050_2017'))$VALUE
limit_vec <- c(min(value_vec), max(value_vec))

al_black <- ggplot(filter(all_dat_sf_demo, STNAME == 'New Jersey')) +
  geom_sf(aes(fill = pct_black), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('a) Proportion of State Black Population\n ')

al_white <- ggplot(filter(all_dat_sf_demo, STNAME == 'New Jersey')) +
  geom_sf(aes(fill = pct_white), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('b) Proportion of State White Population\n ')

al_pm_2017 <- ggplot(filter(alldat_sf, STNAME == 'New Jersey',
                            POLLUTANT == 'ozone', TIMESCALE == 'july',
                            SCENARIO %in% c('ref2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'ppb        ', option = 'A', limits = limit_vec) +
  ggtitle('c) Ozone Concentration\n(2017)')

al_pm_nz2050 <- ggplot(filter(alldat_sf, STNAME == 'New Jersey',
                              POLLUTANT == 'ozone', TIMESCALE == 'july',
                              SCENARIO %in% c('nz2050_2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'ppb        ', option = 'A', limits = limit_vec) +
  ggtitle('d) Ozone Concentration\n(Net-Zero 2050) ')

al_pm_nz50_change <- ggplot(filter(alldat_change_sf, STNAME == 'New Jersey',
                                   POLLUTANT == 'ozone', TIMESCALE == 'july')) +
  geom_sf(aes(fill = change), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(name = 'ppb        ', 
                       low = 'blue', mid = 'yellow', high = 'red',
                       midpoint = 0) +
  ggtitle('e) Change in Ozone Concentration:\nNet-Zero 2050 - 2017')

gg_blank <- ggplot() +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pdf("~/scovronick/air_health/air_health/plots/rtr_round3/nj_oz_maps.pdf",
    height = 8.5, width = 11)
print(
  ggarrange(plotlist = list(al_black, al_white, gg_blank, al_pm_2017, al_pm_nz2050, al_pm_nz50_change), 
            nrow = 2, ncol = 3)
)
dev.off()
##################
value_vec <- filter(alldat_sf, STNAME == 'Nevada',
       POLLUTANT == 'ozone', TIMESCALE == 'july',
       SCENARIO %in% c('ref2017', 'nz2050_2017'))$VALUE
limit_vec <- c(min(value_vec), max(value_vec))

al_black <- ggplot(filter(all_dat_sf_demo, STNAME == 'Nevada')) +
  geom_sf(aes(fill = pct_black), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('a) Proportion of State Black Population\n ')

al_white <- ggplot(filter(all_dat_sf_demo, STNAME == 'Nevada')) +
  geom_sf(aes(fill = pct_white), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'Percentage',
                     # breaks = seq(0, 0.55, 0.05),
                     limits = c(0, NA),
                     direction = 1) +
  ggtitle('b) Proportion of State White Population\n ')

al_pm_2017 <- ggplot(filter(alldat_sf, STNAME == 'Nevada',
                            POLLUTANT == 'ozone', TIMESCALE == 'july',
                            SCENARIO %in% c('ref2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'ppb        ', option = 'A', limits = limit_vec) +
  ggtitle('c) Ozone Concentration\n(2017)')

al_pm_nz2050 <- ggplot(filter(alldat_sf, STNAME == 'Nevada',
                              POLLUTANT == 'ozone', TIMESCALE == 'july',
                              SCENARIO %in% c('nz2050_2017'))) +
  geom_sf(aes(fill = VALUE), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_viridis(name = 'ppb        ', option = 'A', limits = limit_vec) +
  ggtitle('d) Ozone Concentration\n(Net-Zero 2050) ')

al_pm_nz50_change <- ggplot(filter(alldat_change_sf, STNAME == 'Nevada',
                                   POLLUTANT == 'ozone', TIMESCALE == 'july')) +
  geom_sf(aes(fill = change), lwd = 0)	+
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_gradient2(name = 'ppb        ', 
                       low = 'blue', mid = 'yellow', high = 'red',
                       midpoint = 0) +
  ggtitle('e) Change in Ozone Concentration:\nNet-Zero 2050 - 2017')

gg_blank <- ggplot() +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )

pdf("./outputs/nv_oz_maps.pdf",
    height = 8.5, width = 11)
print(
  ggarrange(plotlist = list(al_black, al_white, gg_blank, al_pm_2017, al_pm_nz2050, al_pm_nz50_change), 
            nrow = 2, ncol = 3)
)
dev.off()

