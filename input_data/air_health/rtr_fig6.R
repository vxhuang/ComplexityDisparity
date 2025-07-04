libs <- c("dplyr", "tidyr", "ggplot2", "grid", "ggtext", "scales", "ggpubr", "ggnewscale")
lapply(libs, library, character.only = TRUE)

draw_key_upper_hex <- function (data, params, size) {
  # upper hexagon vertex coordinates 
  v1 <- list(x = c(0.5, 0.5 + sqrt(3)/4*.9, 0.5 + cos(seq(pi/6, pi + pi/6, by = pi/180)) * 0.25, 0.5 - sqrt(3)/4*.9, 0.5 - sqrt(3)/4*.9),
             y = c(0.95, 0.725, 0.5 + sin(seq(pi/6, pi + pi/6, by = pi/180)) * 0.225, 0.275, 0.725))
  v2 <- list(x = c(0.5, 0.5 - sqrt(3)/4*.9, 0.5 + cos(seq(pi/6 - pi, pi/6, by = pi/180)) * 0.25, 0.5 + sqrt(3)/4*.9, 0.5 + sqrt(3)/4*.9),
             y = c(0.05, 0.275, 0.5 + sin(seq(pi/6 - pi, pi/6, by = pi/180)) * 0.225, 0.725, 0.275))
  # hexagon grob
  grobTree(
    polygonGrob(v1$x, v1$y, 
                gp = gpar(col = data$colour,
                          fill = alpha(data$fill, data$alpha))), 
    polygonGrob(v2$x, v2$y, 
                gp = gpar(col = "black",
                          fill = alpha("grey95", data$alpha))), 
    linesGrob(x = c(0.5 - sqrt(3)/4*.9, 0.5 + sqrt(3)/4*.9), y = c(0.275, 0.725), 
              gp = gpar(col = "white", lwd = 1)), 
    gp = gpar(lwd = (data$linewidth %||% 0.05) * .pt, 
              lty = data$linetype %||% 1)
  )
  
}

draw_key_lower_hex <- function (data, params, size) {
  # lower hexagon vertex coordinates 
  v1 <- list(x = c(0.5, 0.5 + sqrt(3)/4*.9, 0.5 + cos(seq(pi/6, pi + pi/6, by = pi/180)) * 0.25, 0.5 - sqrt(3)/4*.9, 0.5 - sqrt(3)/4*.9),
             y = c(0.95, 0.725, 0.5 + sin(seq(pi/6, pi + pi/6, by = pi/180)) * 0.225, 0.275, 0.725))
  v2 <- list(x = c(0.5, 0.5 - sqrt(3)/4*.9, 0.5 + cos(seq(pi/6 - pi, pi/6, by = pi/180)) * 0.25, 0.5 + sqrt(3)/4*.9, 0.5 + sqrt(3)/4*.9),
             y = c(0.05, 0.275, 0.5 + sin(seq(pi/6 - pi, pi/6, by = pi/180)) * 0.225, 0.725, 0.275))
  # hexagon grob
  grobTree(
    polygonGrob(v1$x, v1$y, 
                gp = gpar(col = data$colour,
                          fill = alpha("grey95", data$alpha))), 
    polygonGrob(v2$x, v2$y, 
                gp = gpar(col = "black",
                          fill = alpha(data$fill, data$alpha))), 
    linesGrob(x = c(0.5 - sqrt(3)/4*.9, 0.5 + sqrt(3)/4*.9), y = c(0.275, 0.725), 
              gp = gpar(col = "white", lwd = 1)), 
    gp = gpar(lwd = (data$linewidth %||% 0.05) * .pt, 
              lty = data$linetype %||% 1)
  )
}

spdf <- geojson_read("../air_health/data/us_states_hexgrid.geojson", what = "sp")
spdf@data = spdf@data %>% mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf@data$id <- 1:nrow(spdf@data)
spdf_fortified <- broom::tidy(spdf)
spdf_fortified$id <- as.numeric(spdf_fortified$id)
spdf_fortified <- spdf_fortified %>% left_join(spdf@data, by = "id")
spdf_fortified <- spdf_fortified %>% filter(!google_name %in% c("Alaska", "Hawaii"))
centers <- cbind.data.frame(data.frame(rgeos::gCentroid(spdf, byid=TRUE), id = spdf@data$iso3166_2))
states_fips <- pop_2015_for_y0 %>% select(STATE, STNAME) %>% distinct()

pm_dp_all <- read.csv("../air_health/data/wrf_data/pm_disparity_race.csv")

pm_dp_all <- pm_dp_all %>% select(STNAME, scenario, black, total, white) %>% 
  filter(scenario %in% c("pengfei_2017", "pengfei_NZ2050", "jie_equityNZ2050scaled", "pengfei_NZ2050RCP"),  
         !STNAME %in% c("Alaska", "Hawaii", "Puerto Rico", "NA")) %>% drop_na()
pm_dp_ssp3 <- read.csv("../air_health/data/wrf_data/pm_disparity_race_ssp3.csv")
pm_dp_ssp3 <- pm_dp_ssp3 %>% select(STNAME, scenario, black, total, white) %>% 
  filter(scenario %in% c("pengfei_NZ2050"), !STNAME %in% c("Alaska", "Hawaii", "Puerto Rico", "NA")) %>% 
  mutate(scenario = "pengfei_NZ2050_SSP3") %>% drop_na()
pm_dp_all <- bind_rows(pm_dp_all, pm_dp_ssp3)
pm_dp_all$disparity <- pm_dp_all$black - pm_dp_all$white
pm_dp_all$scenario <- factor(pm_dp_all$scenario, levels = c("pengfei_2017", "pengfei_NZ2050", "jie_equityNZ2050scaled", "pengfei_NZ2050RCP", "jie_RCP45NZ2050", "pengfei_NZ2050_SSP3", "pengfei_NZ2050_SSP5"))
pm_dp_all <- pm_dp_all %>% select(STNAME, scenario, disparity) %>% mutate(disparity = abs(disparity)) %>% spread(key = scenario, value = disparity)
pm_dp_all <- pm_dp_all %>% mutate(across(3:6, ~ .x - pengfei_2017))
# use 0.1 as the threshold
pm_dp_all <- pm_dp_all %>% mutate(across(3:6, ~round(.x, 1)))
pm_dp_all <- pm_dp_all %>% select(-pengfei_2017)
pm_dp_all <- pm_dp_all %>%
  rowwise() %>%
  mutate(all_equity = case_when(
    all(c_across(2:5) >= 0.1) ~ 1,
    all(c_across(2:5) <= -0.1) ~ -1,
    all(abs(c_across(2:5) < 0.1)) ~ 0,
    TRUE ~ 0.5
  )) %>%
  ungroup()
pm_dp_all_viz <- spdf_fortified %>% left_join(pm_dp_all %>% select(STNAME, all_equity), by = c("google_name" = "STNAME"))
pm_dp_all_viz$all_equity <- factor(pm_dp_all_viz$all_equity, levels = c(-1, 1, 0, 0.5))

fig_6a <-
  ggplot() +
  geom_polygon(data = pm_dp_all_viz %>% filter(!order == 7), aes(x = long, y = lat, group = group, fill = all_equity), color = "black", key_glyph = draw_key_hollowed_hex, lwd = 0.05) +
  scale_fill_manual(values = c("blue", "red2", "grey80", "grey30"), 
                    labels = c("Consistent narrowing of disparity", "Consistent widening of disparity", "No major changes", "Scenario dependent")) + 
  labs(fill = "Changes in four Net-Zero 2050 scenarios compared to 2017", 
       title = '<b>a) PM<sub>2.5</sub> exposure disparity <br>between White and Black populations</b>') +
  geom_polygon(data = pm_dp_all_viz, aes(x = long, y = lat, group = group), color = "black", fill = NA, lwd = 1.5) +
  geom_polygon(data = pm_dp_all_viz, aes(x = long, y = lat, group = group), color = "white", fill = NA, lwd = 1.2) +
  geom_point(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y), color = "white", size = 8) +
  geom_text(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y, label = id)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12), legend.position = "bottom", 
        plot.title = element_markdown(hjust = 0.5, halign = 0.1, size = 14),
        panel.background = element_blank(), legend.title = element_markdown(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), legend.box.background = element_blank()) +
  guides(fill = guide_legend(ncol = 1, title.position = "top")) +
  coord_map() 

oz_dp_all <- read.csv("../air_health/data/wrf_data/oz_disparity_race.csv")

oz_dp_all <- oz_dp_all %>% select(STNAME, scenario, black, total, white) %>% 
  filter(scenario %in% c("pengfei_2017", "pengfei_NZ2050", "jie_equityNZ2050scaled", "pengfei_NZ2050RCP"), 
         !STNAME %in% c("Alaska", "Hawaii", "Puerto Rico", "NA")) %>% drop_na()

oz_dp_ssp3 <- read.csv("../air_health/data/wrf_data/oz_disparity_race_ssp3.csv")
oz_dp_ssp3 <- oz_dp_ssp3 %>% select(STNAME, scenario, black, total, white) %>% 
  filter(scenario %in% c("pengfei_NZ2050"), !STNAME %in% c("Alaska", "Hawaii", "Puerto Rico", "NA")) %>% 
  mutate(scenario = "pengfei_NZ2050_SSP3") %>% drop_na()

oz_dp_all <- bind_rows(oz_dp_all, oz_dp_ssp3)
oz_dp_all$disparity <- oz_dp_all$black - oz_dp_all$white
oz_dp_all$scenario <- factor(oz_dp_all$scenario, levels = c("pengfei_2017", "pengfei_NZ2050", "jie_equityNZ2050scaled", "pengfei_NZ2050RCP", "jie_RCP45NZ2050", "pengfei_NZ2050_SSP3", "pengfei_NZ2050_SSP5"))
oz_dp_all <- oz_dp_all %>% select(STNAME, scenario, disparity) %>% mutate(disparity = abs(disparity)) %>% spread(key = scenario, value = disparity)
oz_dp_all <- oz_dp_all %>% mutate(across(3:6, ~ .x - pengfei_2017))
oz_dp_all <- oz_dp_all %>% mutate(across(3:6, ~round(.x, 1)))
oz_dp_all <- oz_dp_all %>% select(-pengfei_2017)

# use 0.2 as the threshold
oz_dp_all <- oz_dp_all %>% 
  rowwise() %>%
  mutate(all_equity = case_when(
    all(c_across(2:5) >= 0.2) ~ 1,
    all(c_across(2:5) <= -0.2) ~ -1,
    all(abs(c_across(2:5) < 0.2)) ~ 0,
    TRUE ~ 0.5
  )) %>%
  ungroup()
oz_dp_all_viz <- spdf_fortified %>% left_join(oz_dp_all %>% select(STNAME, all_equity), by = c("google_name" = "STNAME"))
oz_dp_all_viz$all_equity <- factor(oz_dp_all_viz$all_equity, levels = c(-1, 1, 0, 0.5))

fig_6b <-
  ggplot() +
  geom_polygon(data = oz_dp_all_viz %>% filter(!order == 7), aes(x = long, y = lat, group = group, fill = all_equity), color = "black", key_glyph = draw_key_hollowed_hex, lwd = 0.05) +
  scale_fill_manual(values = c("blue", "red2", "grey80", "grey30"), 
                    labels = c("Consistent narrowing of disparity", "Consistent widening of disparity", "No major changes", "Scenario dependent")) + 
  labs(fill = "", 
       title = '<b>b) Summer ozone exposure disparity <br>between White and Black populations</b>') +
  geom_polygon(data = oz_dp_all_viz, aes(x = long, y = lat, group = group), color = "black", fill = NA, lwd = 1.5) +
  geom_polygon(data = oz_dp_all_viz, aes(x = long, y = lat, group = group), color = "white", fill = NA, lwd = 1.2) +
  geom_point(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y), color = "white", size = 8) +
  geom_text(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y, label = id)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12), 
        plot.title = element_markdown(hjust = 0.5, halign = 0.1, size = 14),
        panel.background = element_blank(), legend.title = element_markdown(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), legend.box.background = element_blank()) +
  coord_map() 

fig6 <- ggarrange(fig_6a, fig_6b, nrow = 1, common.legend = T, widths = 14, heights = 6, legend = "bottom")
ggsave("../air_health/Figure6.pdf", plot = fig6, width = 14, height = 6)


