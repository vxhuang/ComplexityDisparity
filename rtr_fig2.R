libs <- c("dplyr", "tidyr", "ggplot2", "readxl", "grid", "gridExtra", "ggtext", "scales", "ggnewscale")
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

cpx_co2 <- bind_rows(
  read.csv("../air_health/emis/Ref_energy_CO2.csv", stringsAsFactors = F) %>% mutate(scenario = "REF"),
  read.csv("../air_health/emis/NZ_energy_CO2.csv", stringsAsFactors = F) %>% mutate(scenario = "NZ")
)

cpx_land <- bind_rows(
  read.csv("../air_health/emis/Ref_landuse_CO2.csv", stringsAsFactors = F) %>% mutate(scenario = "REF"),
  read.csv("../air_health/emis/NZ_landuse_CO2.csv", stringsAsFactors = F) %>% mutate(scenario = "NZ")
)

cpx_nonco2 <- bind_rows(
  read.csv("../air_health/emis/nonCO2 emissions by subsector_reference.csv", check.names = F, stringsAsFactors = F) %>% mutate(scenario = "REF"),
  read.csv("../air_health/emis/nonCO2 emissions by resource production_reference.csv", check.names = F, stringsAsFactors = F) %>% mutate(scenario = "REF"),
  read.csv("../air_health/emis/(updated) nonCO2 emissions by subsector_netzero.csv", check.names = F, stringsAsFactors = F) %>% mutate(scenario = "NZ"),
  read.csv("../air_health/emis/(updated) nonCO2 emissions by resource production_netzero.csv", check.names = F, stringsAsFactors = F) %>% mutate(scenario = "NZ")
)

cpx_nonco2_co2 <- cpx_nonco2 %>% filter(GHG == "CO2")
cpx_nonco2 <- cpx_nonco2 %>% filter(GHG != "CO2") %>% group_by(scenario, region, GHG) %>%
  mutate(`2050` = if_else(sector == "other industrial energy use", `2020`, `2050`)) %>%
  bind_rows(cpx_nonco2_co2)

gwp <- read_xlsx("../air_health/gwp_conversion.xlsx", sheet = 1)
names(gwp) <- c("GHG", "GWP conversion")

cpx_co2 <- cpx_co2 %>% select(scenario, region, sector, ghg, X2015, X2050) %>%
  gather(key = year, value = emis, -scenario, -region, -sector, -ghg)
cpx_co2 <- cpx_co2 %>% mutate(scenario = paste(scenario, year)) %>%
  filter(scenario %in% c("REF X2015", "REF X2050", "NZ X2050")) %>% select(-year)
cpx_land <- cpx_land %>% select(scenario, region, sector, ghg, X2015, X2050) %>% 
  gather(key = year, value = emis, -scenario, -region, -sector, -ghg) %>% mutate(emis = emis / 3.67)
cpx_land <- cpx_land %>% mutate(scenario = paste(scenario, year)) %>% 
  filter(scenario != "NZ X2015") %>% select(-year) %>% mutate(emis = emis / 3.67)
names(cpx_land)[4] <- "GHG"
cpx_nonco2 <- cpx_nonco2 %>% select(scenario, region, sector, GHG, `2015`, `2050`) %>% 
  gather(key = year, value = emis, -scenario, -region, -sector, -GHG)
cpx_nonco2 <- cpx_nonco2 %>% mutate(scenario = paste0(scenario, " X", year)) %>% 
  filter(scenario != "NZ X2015") %>% select(-year)

BC <- c("BC", "BC_AWB")
CH4 <- c("CH4", "CH4_AGR", "CH4_AWB")
CO <- c("CO", "CO_AWB")
H2 <- c("H2", "H2_AWB")
N2O <- c("N2O", "N2O_AGR", "N2O_AWB")
NH3 <- c("NH3", "NH3_AGR", "NH3_AWB")
NMVOC <- c("NMVOC", "NMVOC_AGR", "NMVOC_AWB")
NOx <- c("NOx", "NOx_AGR", "NOx_AWB")
OC <- c("OC", "OC_AWB")
SO2 <- c("SO2", "SO2_1","SO2_1_AWB", "SO2_2","SO2_2_AWB", "SO2_3","SO2_3_AWB", "SO2_4","SO2_4_AWB")
# CO2 <- c("CO2", "CO2_ELEC", "CO2_ELEC2")

cpx_nonco2 <- cpx_nonco2 %>% 
  mutate(GHG = ifelse(GHG %in% BC, "BC", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% CH4, "CH4", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% CO, "CO", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% H2, "H2", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% N2O, "N2O", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% NH3, "NH3", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% NMVOC, "NMVOC", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% NOx, "NOx", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% OC, "OC", GHG)) %>%
  mutate(GHG = ifelse(GHG %in% SO2, "SO2", GHG)) 

cpx_ghg <- cpx_nonco2 %>% bind_rows(cpx_land) %>% left_join(gwp, by = "GHG") %>% drop_na() %>% 
  mutate(emis = emis * `GWP conversion`) %>% select(-`GWP conversion`) %>% 
  group_by(scenario, region, sector, GHG) %>% summarise(emis = sum(emis), .groups = "drop")
cpx_ghg <- cpx_ghg %>% mutate(GHG = "CO2e")
cpx_nonco2 <- cpx_nonco2 %>% filter(GHG %in% c("SO2", "NOx", "PM2.5", "BC", "OC")) 
cpx_so2 <- cpx_nonco2 %>% filter(GHG == "SO2", scenario %in% c("REF X2015", "REF X2050", "NZ X2050"))
cpx_nox <- cpx_nonco2 %>% filter(GHG == "NOx", scenario %in% c("REF X2015", "REF X2050", "NZ X2050"))
cpx_pm25 <- cpx_nonco2 %>% filter(GHG == "PM2.5", scenario %in% c("REF X2015", "REF X2050", "NZ X2050"))

sector_colors_wpf <- c(
  "agriculture" = "#c0bfc1",
  "power" = "#8e6639",
  "industry" = "#3c5a7f",
  "residential commercial" = "#97bfd7",
  "transportation" = "#f0c546",
  "land use" = "#00a96a",
  "DAC" = "#283143"
)

cpx_nonco2 <- bind_rows(cpx_ghg, cpx_nonco2)
cpx_nonco2 <- cpx_nonco2 %>% left_join(sector_mapping, by = c("sector" = "GCAM_sector")) %>% 
  mutate(major_sector = if_else(is.na(major_sector), "land use", major_sector))
cpx_nonco2 <- cpx_nonco2 %>% mutate(major_sector = if_else(sector == "airCO2", "DAC", major_sector))
cpx_nonco2 <- cpx_nonco2 %>% filter(major_sector != "wildfire")
cpx_nonco2 <- cpx_nonco2 %>% select(-region, -sector) %>% 
  group_by(scenario, GHG, major_sector) %>% summarise(emis = sum(emis), .groups = "drop")
cpx_nonco2 <- cpx_nonco2 %>% spread(key = GHG, value = emis) %>% replace(is.na(.), 0) %>% 
  mutate(PM2.5 = if_else(major_sector %in% c("agriculture", "land use"), (BC + OC * 1.8) * 1.1, PM2.5))
cpx_nonco2 <- cpx_nonco2 %>% gather(key = GHG, value = emis, -scenario, -major_sector)
cpx_nonco2 <- cpx_nonco2 %>% filter(!GHG %in% c("BC", "OC"))
cpx_nonco2 <- cpx_nonco2 %>% mutate(emis = if_else(GHG == "CO2e", emis / 1000, emis))
cpx_nonco2$scenario = factor(cpx_nonco2$scenario, levels = c("REF X2015", "REF X2050", "NZ X2050"))
cpx_nonco2$GHG <- factor(cpx_nonco2$GHG, levels = c("CO2e", "NOx", "SO2", "PM2.5"))
cpx_nonco2$major_sector <- factor(cpx_nonco2$major_sector, levels = 
                                    c("agriculture", "industry", "power", "residential commercial", "transportation", "DAC", "land use"))

cpx_fig2a <- ggplot(data = cpx_nonco2) +
  geom_col(mapping = aes(x = GHG, y = emis, fill = major_sector)) + 
  facet_wrap(~scenario, labeller = labeller(scenario = c("REF X2015" = "2017", "REF X2050" = "Reference 2050", "NZ X2050" = "Net-Zero 2050"))) +
  labs(x = "", y = "CO<sub>2</sub>e (billion metric tons)", fill = NULL,
       title = "<b>a) National total emissions by sector in 2017 and 2050<b>") +
  theme_bw() +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(), axis.line = element_line(colour = "black"),
    legend.position = "right", legend.text = element_text(size = 12), plot.title = element_markdown(size = 16),
    axis.title.x = element_text(size = 12), axis.text.x = element_markdown(size = 12),
    axis.title.y = element_markdown(size = 12), axis.text.y = element_text(size = 12), plot.margin = unit(c(.5, .5, .5, .5), "cm")) +
  scale_x_discrete(labels = c("CO<sub>2</sub>e", "NO<sub>x</sub>", "SO<sub>2</sub>", "PM<sub>2.5</sub>")) +
  scale_y_continuous(limits = c(-5, 10), expand = c(0, 0), 
                     sec.axis = sec_axis(trans = ~.*1, name = "Other pollutants (Tg)")) +
  scale_fill_manual(values = sector_colors_wpf,
                    labels = c("Agriculture", "Industry", "Power", "Residential and Commercial", "Transportation", "Direct Air Capture", "Land-related"))

cpx_co2 <- cpx_co2 %>% select(-sector, -ghg) %>% group_by(scenario, region) %>% summarise(emis = sum(emis), .groups = "drop")
cpx_co2 <- cpx_co2 %>% spread(key = scenario, value = emis) %>% mutate(dCO2_NZ = `NZ X2050` - `REF X2015`, dCO2_REF = `REF X2050` - `REF X2015`)
cpx_co2 <- cpx_co2 %>% mutate(dCO2_REF = pmin(dCO2_REF, 0))

cpx_nox <- cpx_nox %>% select(-sector, -GHG) %>% group_by(scenario, region) %>% summarise(emis = sum(emis), .groups = "drop")
cpx_nox <- cpx_nox %>% spread(key = scenario, value = emis) %>% mutate(dNOx_NZ = `NZ X2050` - `REF X2015`, dNOx_REF = `REF X2050` - `REF X2015`)

cpx_co2_viz <- cpx_co2 %>% left_join(statepop %>% select(abbr, full), by = c("region" = "abbr")) %>% drop_na()
cpx_co2_viz <- spdf_fortified %>% left_join(cpx_co2_viz %>% select(dCO2_NZ, dCO2_REF, full), by = c("google_name" = "full")) 

cpx_nox_viz <- cpx_nox %>% left_join(statepop %>% select(abbr, full), by = c("region" = "abbr")) %>% drop_na()
cpx_nox_viz <- spdf_fortified %>% left_join(cpx_nox_viz %>% select(dNOx_NZ, dNOx_REF, full), by = c("google_name" = "full")) 

cpx_fig2b <-
  ggplot() +
  geom_polygon(data = cpx_co2_viz %>% filter(!order %in% c(3, 4, 7)), aes(x = long, y = lat, group = group, fill = dCO2_REF), color="black", key_glyph = draw_key_upper_hex, lwd = 0.05) +
  scale_fill_gradientn(colors = c("#013220", "#228b22", "white"), limits = c(-950, 0), values = c(0, 650/950, 1)) +
  labs(fill = "CO<sub>2</sub> emissions <br>(million ton)") +
  guides(fill = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_polygon(data = cpx_nox_viz %>% filter(!order %in% c(7, 1, 6)), aes(x = long, y = lat, group = group, fill = dNOx_REF * 1e3), color="black", key_glyph = draw_key_lower_hex, lwd = 0.05) +
  scale_fill_gradientn(colors = c("darkorange4", "darkorange", "white"), limits = c(-600, 0), values = c(0, 400/600, 1), breaks = c(-600, -400, -200, 0)) + 
  labs(fill = "NO<sub>x</sub> emissions <br>(kilo ton)", 
       title = '<b>b) State-level changes in the energy system CO<sub>2</sub> and NO<sub>x</sub> <br>emissions in Reference 2050 compared to 2017</b>') +
  guides(fill = guide_legend(order = 2)) +
  geom_polygon(data = cpx_co2_viz, aes(x = long, y = lat, group = group), color = "black", fill = NA, lwd = 1.5) + 
  geom_polygon(data = cpx_co2_viz, aes(x = long, y = lat, group = group), color = "white", fill = NA, lwd = 1.2) + 
  geom_point(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y), color = "white", size = 8) + 
  geom_text(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y, label = id)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12), 
        plot.title = element_markdown(size = 16, hjust = 0.9, halign = 0.0),
        panel.background = element_blank(), legend.title = element_markdown(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), legend.box.background = element_blank()) +
  coord_map() 

cpx_fig2c <-
  ggplot() +
  geom_polygon(data = cpx_co2_viz %>% filter(!order %in% c(3, 4, 7)), aes(x = long, y = lat, group = group, fill = dCO2_NZ), color="black", key_glyph = draw_key_upper_hex, lwd = 0.05) +
  scale_fill_gradientn(colors = c("#013220", "#228b22", "white"), limits = c(-950, 0), values = c(0, 650/950, 1)) +
  labs(fill = "CO<sub>2</sub> emissions <br>(million ton)") +
  guides(fill = guide_legend(order = 1)) +
  ggnewscale::new_scale_fill() +
  geom_polygon(data = cpx_nox_viz %>% filter(!order %in% c(7, 1, 6)), aes(x = long, y = lat, group = group, fill = dNOx_NZ * 1e3), color="black", key_glyph = draw_key_lower_hex, lwd = 0.05) +
  scale_fill_gradientn(colors = c("darkorange4", "darkorange", "white"), limits = c(-600, 0), values = c(0, 400/600, 1), breaks = c(-600, -400, -200, 0)) + 
  labs(fill = "NO<sub>x</sub> emissions <br>(kilo ton)", 
       title = '<b>c) State-level changes in the energy system CO<sub>2</sub> and NO<sub>x</sub> <br>emissions in Net-Zero 2050 compared to 2017</b>') +
  guides(fill = guide_legend(order = 2)) +
  geom_polygon(data = cpx_co2_viz, aes(x = long, y = lat, group = group), color = "black", fill = NA, lwd = 1.5) + 
  geom_polygon(data = cpx_co2_viz, aes(x = long, y = lat, group = group), color = "white", fill = NA, lwd = 1.2) + 
  geom_point(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y), color = "white", size = 8) + 
  geom_text(data = centers %>% filter(!id %in% c("AK", "HI")), aes(x = x, y = y, label = id)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text = element_text(size = 12), 
        plot.title = element_markdown(size = 16, hjust = 0.9, halign = 0.0),
        panel.background = element_blank(), legend.title = element_markdown(), 
        axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), 
        panel.border = element_blank(), legend.box.background = element_blank()) +
  coord_map() 

cpx_fig2 <- gridExtra::grid.arrange(cpx_fig2a, 
                                    gridExtra::grid.arrange(NULL, cpx_fig2b, nrow = 1, widths = c(-1.5, 11.5), heights = 5), 
                                    gridExtra::grid.arrange(NULL, cpx_fig2c, nrow = 1, widths = c(-1.5, 11.5), heights = 5), 
                                    nrow = 3)
ggsave("../air_health/Figure2.pdf", plot = cpx_fig2, width = 10, height = 15)

