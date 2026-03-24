##### This script creates the figures in the manuscript and supplement ##### 

rm(list = ls())
gc()

###### SETUP ##################################################################
library(tidyverse)
library(sf)
library(patchwork)
library(units)

## Define the dates of wave 1 and wave 2 peaks 
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

###### READ IN DATA ###########################################################

##### Hexgrid shape file ######################################################
hexgrid <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
         filter(
         # Taking out the isolated hex at Keywest
         as.integer(hexid) != 6644) %>%
         # INLA requires the id to only be 1:N, where N is the total
         # number of observations; because of this we need to rename the 
         # hexids to be continuous. 
         mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                               as.numeric(hexid) - 1),
                hexid = as.character(hexid))

## Certifying the correct number of unique hex; 7516
length(unique(na.omit(hexgrid$hexid)))

###### Additional Geometry files and information ##############################
## County FIPS codes used below 
# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

### US States shape file 
us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()|> 
  st_transform(crs = 5070)

#####  Population data  ########################################################
####### Previously read in the old non-intersection polygons data 
hexpop <- vroom::vroom("Data/data-products/geo-hexes/pop/hexgrid_intersection_pop.csv") %>%
          select(hexid, population, fips) %>%
          mutate(hexid = as.character(hexid)) %>%
          left_join(hexgrid) %>% 
          st_as_sf()

#####  Infection data  ########################################################
## Allocated across the hexgrid by date 
## We use the CSV because it is faster 
## This file is massive and cannot be stored on GitHub; it is in the Zotero repository
hexgrid_preomicron <- vroom::vroom("Data/data-products/geo-hexes/hexid-observations_preomicron_intersection_hexgrid1100km.csv") %>% 
                      mutate(date = as.Date(date)) |>
                      filter(
                             ## Taking out the isolated hex at Keywest
                             as.integer(hexid) != 6644) %>%
                      # INLA requires the id to only be 1:N, where N is the total
                      # number of observations; because of this we need to rename the 
                      # hexids to be continuous. 
                      mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                                            as.numeric(hexid) - 1),
                             hexid = as.character(hexid)) %>% 
                      filter(population > 0)

## Cumulative infections across for each date (For figure 2A)
infections_daily_cum <- hexgrid_preomicron %>%
  group_by(date) |> 
  summarise(sum_infectionsPC = sum(infectionsPC, na.rm = T)) |> 
  st_drop_geometry()

## Cumulative infections across all dates for each hex
hexgrid_preomicron_cum <- hexgrid_preomicron %>%
  group_by(hexid) |> 
  summarise(cum_infections = sum(infections, na.rm = T)) |> 
  right_join(hexpop |>  
               mutate(hexid = as.character(hexid)) |> 
               select(hexid, population, geometry, fips))  |> 
  mutate(cum_infectionsPC = cum_infections/population) |> 
  sf::st_as_sf() |> 
  st_transform(crs = 5070)|>
  dplyr::mutate(logpopulation = log10(population),
                cum_incidence = exp(log10(cum_infections) - logpopulation),
                log_incidence = log10(cum_incidence+1))

##### Model results ###########################################################
CAR_df_preomicron_w1 <- readRDS("Data/data-products/car_list_wave1_adaptStart.rds") |> 
                        bind_rows() |> mutate(wave = "1st Wave")
CAR_df_preomicron_w2 <- readRDS("Data/data-products/car_list_wave2_adaptStart.rds") |> 
                        bind_rows() |> mutate(wave = "2nd Wave")

CAR_df_preomicron <- rbind(CAR_df_preomicron_w1, CAR_df_preomicron_w2)

CAR_df_preomicron <- CAR_df_preomicron |> 
                      left_join(hexgrid) |> 
                      st_as_sf()

##### WAVE FRONT SPEED AND AREAL EXPANSION RATE ###############################
distanceToFront.df_first <- sf::st_read("Data/data-products/wavefronts/firstDistanceToFront.shp") |> 
  rename(distToFront = dstTFrn)   %>% st_transform(crs=5070)

distanceToFront.df_second <- sf::st_read("Data/data-products/wavefronts/secondDistanceToFront.shp")|> 
  rename(distToFront = dstTFrn) %>% st_transform(crs=5070)

wave1Stats <- vroom::vroom("Data/data-products/wavefronts/wave1Characteristics.csv") |> 
  mutate(wave = "1st")

wave2Stats <- vroom::vroom("Data/data-products/wavefronts/wave2Characteristics.csv") |> 
  mutate(wave = "2nd")

waveStatsJoined <- rbind(wave1Stats, wave2Stats) %>%
  mutate(wave = ifelse(wave == "1st", 
                       "1st wave", 
                       "2nd wave"))

colnames(waveStatsJoined)[1] <- "Days before peak"
colnames(waveStatsJoined)[3] <- "Recruitment rate (km2/day)"
colnames(waveStatsJoined)[5] <- "Mean speed (km/day)"

##### Wave boundaries #########################################################
firstBound <- st_read(here("Data/data-products/wavefronts/FirstWaveBound.shp"))
secondBound <- st_read(here("Data/data-products/wavefronts/SecondWaveBound.shp"))

###############################################################################
##### FIGURE 1 ################################################################
###############################################################################

##### Setup data for New England States panel
new_england_states <- us_states |> 
  dplyr::filter(GEOID %in% c("09","23","25","33","44","50"))

new_england_counties <- tigris::counties(state = c("09","23","25","33","44","50"))|> 
  st_transform(crs = 5070)

ct_counties <- tigris::counties(state = 09)|> 
  st_transform(crs = 5070)

palette <- "Purple-Yellow"

##### Plot the population across hexgrid ######################################    

## Figure 1A: Population across total United States
us_hex_plt <- ggplot() + 
              geom_sf(hexpop |> 
                        filter(!is.na(population)) |> 
                        st_transform(crs = 5070),
                      mapping=aes(fill = log10(population+1))) +
              geom_sf(us_states,
                      mapping=aes(),
                      color = "black",
                      fill = "transparent")+
              colorspace::scale_fill_continuous_sequential(name = "Population\n(log scale)",
                                                           palette = palette,
                                                           breaks = seq(1,7,1),
                                                           labels = scales::label_math(),
                                                           limits = c(1,7),
                                                           na.value = "grey60"
              )+
              theme_minimal()+
              theme(legend.position = "bottom", 
                    legend.title.position = "top",
                    legend.title = element_text(hjust = 0.5),
                    legend.key.width = grid::unit(1, "in"),
                    axis.text = element_text(size = 6)); us_hex_plt

## Save Figure 1A
ggsave(plot = us_hex_plt,
       filename = "Figures/extra_figures/fig1a_new.png",
       width = 16,
       height = 9,
       dpi = 100)


### CT, ME, MA, NH,, RI, VT
new_england_hexpop <- hexpop  %>% 
  mutate(STATEFP = str_sub(fips, 1, 2)) %>%
  filter(STATEFP %in% c("09","23","25","33","44","50"))

### CT, ME, MA, NH,, RI, VT
new_england_states <- us_states|> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

## Figure 1B: Population across New England states
new_england_hex_plt <- ggplot()+
                       geom_sf(new_england_hexpop |>
                                filter(!is.na(population), 
                                       STATEFP == "33") |>
                                st_transform(crs = 5070),
                              mapping=aes(fill= log10(population+1))) +
                       geom_sf(new_england_states,
                              mapping=aes(),
                              color = "black",
                              fill = "transparent")+
                       colorspace::scale_fill_continuous_sequential(name = "Population\n(log scale)",
                                                                   palette = palette,
                                                                   breaks = seq(1,7,1),
                                                                   labels = scales::label_math(),
                                                                   limits = c(1,7),
                                                                   na.value = "grey60"
                      )+
                      theme_minimal()+
                      theme(legend.position = "none", 
                            legend.title.position = "top",
                            legend.title = element_text(hjust = 0.5),
                            legend.key.width = grid::unit(1, "in"),
                            axis.text = element_text(size = 6)); new_england_hex_plt

## Save Figure 1B
ggsave(plot = new_england_hex_plt,
       filename = "Figures/extra_figures/fig1b_new.2png",
       width = 16, 
       height = 9, 
       dpi = 100)

## Figure 1B: Population across Connecticut
ct_hexpop <- hexpop  %>% 
  mutate(STATEFP = str_sub(fips, 1, 2)) %>%
  filter(STATEFP %in% c("09"))

ct_hex_plt <- ggplot()+
  geom_sf(ct_hexpop |> 
            filter(!is.na(population))|> 
            st_transform(crs = 5070),
          mapping=aes(fill = log10(population+1)))+ 
  geom_sf(ct_counties,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population\n(log scale)",
                                               palette = palette,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               na.value = "grey60"
  )+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(1, "in"),
        axis.text = element_text(size = 6)); ct_hex_plt

## Save Figure 1C
ggsave(filename = "Figures/extra_figures/fig1c_new.png",
       plot = ct_hex_plt,
       width = 16,
       height = 9, 
       dpi = 100)


## Patchwork Population
hexpop_zoom <- (us_hex_plt | (new_england_hex_plt / ct_hex_plt))+
  plot_annotation(tag_levels = 'A')+
  plot_layout(widths = c(4,1,1),
              heights = c(4,1,1), 
              guides = "keep")
hexpop_zoom

ggsave(plot = hexpop_zoom, 
       filename = "Figures/extra_figures/fig1_upper_new.png",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hexpop_zoom, 
       filename = "Figures/extra_figures/fig1_upper_new.pdf",
       width = 16,
       height = 9,
       dpi = 100)

## hexes to county columns
hexes_to_county <- vroom::vroom("Data/data-sources/hexid-fips-map.csv") |> 
                   mutate(hexid = as.character(hexid))

hexgrid_preomicron_cum <- hexgrid_preomicron_cum |> 
  left_join(hexes_to_county |> select(hexid, fips) |> mutate(hexid = as.character(hexid))) |> 
  mutate(STATEFP = as.character(substr(fips, 1, 2)))

color_option <- "inferno"

##### United States Infections ################################################
us_hex_infections <- ggplot() +
                      geom_sf(hexgrid_preomicron_cum |>
                                filter(population > 0)|> ## uncomment if you wanna check the flatten figures
                                st_transform(crs = 5070),
                              mapping=aes(fill = log10(cum_infections+1))) +
                      geom_sf(hexgrid_preomicron_cum |>
                                filter(population == 0)|> ## uncomment if you wanna check the flatten figures
                                st_transform(crs = 5070),
                              mapping = aes(),
                              fill = "grey50") +
                      geom_sf(us_states,
                              mapping=aes(),
                              color = "black",
                              fill = "transparent")+
                      scale_fill_viridis_c(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                           option = "inferno",
                                           direction = -1,
                                           na.value = "grey80",
                                           # rev = T,
                                           breaks = seq(0,7,1),
                                           labels = scales::label_math(),
                                           limits = c(0,7)
                      )+
                      theme_minimal()+
                      theme(legend.position = "bottom", 
                            legend.title.position = "top",
                            legend.title = element_text(hjust = 0.5),
                            legend.key.width = grid::unit(3, "cm"),
                            axis.text = element_text(size = 6)); us_hex_infections
## Save figure 1D 
ggsave(filename = "Figures/extra_figures/fig1d.png", 
       plot = us_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

##### New England Infections ##################################################
# Filter the infections based on state FIPS codes 
new_england_grid_infections <- hexgrid_preomicron_cum %>%
  mutate(STATEFP = str_sub(fips, 1, 2)) %>%
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_infections <- ggplot() + 
                              geom_sf(new_england_grid_infections|> 
                                        filter(population > 0)|> ## uncomment if you wanna check the flatten figures
                                        st_transform(crs = 5070), 
                                      mapping=aes(fill = log10(cum_infections+1))) +
                              geom_sf(new_england_grid_infections|> 
                                        filter(population == 0)|> ## uncomment if you wanna check the flatten figures
                                        st_transform(crs = 5070), 
                                      mapping=aes(),
                                      fill = "grey50") +
                              geom_sf(new_england_states,
                                      mapping=aes(),
                                      color = "black",
                                      fill = "transparent")+
                              scale_fill_viridis_c(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                                   option = "inferno",
                                                   direction = -1,
                                                   na.value = "grey80",
                                                   # rev = T,
                                                   breaks = seq(0,7,1),
                                                   labels = scales::label_math(),
                                                   limits = c(0,7)
                              )+
                              theme_minimal()+
                              theme(legend.position = "none", 
                                    legend.title.position = "top",
                                    legend.title = element_text(hjust = 0.5),
                                    legend.key.width = grid::unit(3, "cm"),
                                    axis.text = element_text(size = 6)); new_england_hex_infections

###### Connecticut infections #################################################

ct_grid_infection <- hexgrid_preomicron_cum %>%
  mutate(STATEFP = str_sub(fips, 1, 2)) %>%
  filter(STATEFP == "09")

ct_hexes <- hexgrid |> 
            mutate(hexid = as.character(hexid)) |> 
            left_join(hexes_to_county |> select(hexid, fips) |> mutate(hexid = as.character(hexid))) |> 
            mutate(STATEFP = str_sub(fips, 1, 2)) |> 
            filter(STATEFP == "09")

ct_hex_infections <-  ggplot() + 
  geom_sf(ct_grid_infection|> 
            filter(population > 0)|> ## uncomment if you wanna check the flatten figures
            st_transform(crs = 5070), 
          mapping=aes(fill = log10(cum_infections+1))) +
  geom_sf(ct_grid_infection|> 
            filter(population == 0)|> ## uncomment if you wanna check the flatten figures
            st_transform(crs = 5070), 
          mapping=aes(),
          fill = "grey50")+
  geom_sf(data = ct_counties,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_c(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                       option = "inferno",
                       direction = -1,
                       na.value = "grey70",
                       # rev = T,
                       breaks = seq(0,7,1),
                       labels = scales::label_math(),
                       limits = c(0,7)
  )+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6)); ct_hex_infections

hex_infections <- (us_hex_infections | (new_england_hex_infections / ct_hex_infections))+
  plot_annotation(tag_levels = 'A')+
  plot_layout(widths = c(4,1,1),
              heights = c(4,1,1),
              guides = 'keep'); hex_infections

ggsave(plot = hex_infections,
       filename = "Figures/extra_figures/fig1_lower_new.pdf",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hex_infections,
       filename = "Figures/extra_figures/fig1_lower_new.png",
       width = 16,
       height = 9,
       dpi = 100)

###### Connect all the panels with patchwork ##################################
fig1 <- (hexpop_zoom / hex_infections)+
  plot_annotation(tag_level = 'A')+
  plot_layout(widths = c(3,1))&
  theme(plot.tag = element_text(size = 18),
        axis.text = element_text(size = 4)); fig1

##### Save Figure 1 as a PNG and as a TIFF ####################################
ggsave(plot = fig1,
       file = "Figures/fig1.png",
       width = 9,
       height = 16,
       dpi = 300
)

ggsave(plot = fig1,
       file = "Figures/fig1.tiff",
       width = 9,
       height = 16,
       dpi = 300
)


###############################################################################
###### FIGURE 2 ###############################################################
###############################################################################
##### Infections smoothed across the hexgrid post-BYM2 model compared with 
##### the simple time trend of infections. 

##### Panel A: Time trend of infections daily 
fig2a <- ggplot()+
        geom_line(data = infections_daily_cum,
                  aes(x = date, 
                      y = sum_infectionsPC))+
        theme_minimal()+
        scale_x_date(name = "Date",
                     date_breaks = "4 months",
                     date_labels = "%b %y'",
                     limits = c(as.Date("2020-02-01"), 
                                as.Date("2021-11-01")))+
        scale_y_continuous(name = "Estimated infection per 100,000 persons/day",
                           labels = scales::label_comma())+
        ## Alpha wave marks
        annotate("rect",
                 xmin = alpha_peak - 70,
                 xmax = alpha_peak + 7,
                 ymin = 0, ymax = Inf,
                 fill = "grey50",alpha = 0.2)+
        annotate("text",
                 x = c(alpha_peak-63,
                       alpha_peak-45,
                       alpha_peak-24,
                       alpha_peak),
                 y = rep(2e6, 4),
                 label = LETTERS[2:5],
                 size = 8)+
        annotate("segment",
                 y = c(0.45e6,0.65e6,1.125e6,1.65e6),
                 yend = rep(2e6,4),
                 x = c(alpha_peak-63,
                       alpha_peak-45,
                       alpha_peak-24,
                       alpha_peak),
                 xend = c(alpha_peak-63,
                          alpha_peak-45,
                          alpha_peak-24,
                          alpha_peak),
                 color = "grey50",
                 linetype = "dashed")+
        ## Delta wave marks
        annotate("rect",
                 xmin = delta_peak - 70,
                 xmax = delta_peak + 7,
                 ymin = 0, ymax = Inf,
                 fill = "grey50",alpha = 0.2) +
        annotate("text",
                 x = c(delta_peak-63,
                       delta_peak-45,
                       delta_peak-24,
                       delta_peak),
                 y = rep(0, 4),
                 label = LETTERS[6:9],
                 size = 8)+
        annotate("segment",
                 y = rep(1e4,4),
                 yend = c(0.23e6,0.48e6,1.25e6,1.65e6),
                 x = c(delta_peak-63,
                       delta_peak-45,
                       delta_peak-24,
                       delta_peak),
                 xend = c(delta_peak-63,
                          delta_peak-45,
                          delta_peak-24,
                          delta_peak),
                 color = "grey50",
                 linetype = "dashed"); fig2a

##### Save Figure 2A 
ggsave(filename = "Figures/extra_figures/fig2a_new.png",
       plot = fig2a,
       width = 16,
       height =9, 
       dpi = 100)

################################################################################
##### Panels B-I: Infections across the United States throughout each peak ####
## Shows infections at 63, 42, 21, and 0 days prior to each wave peak. 

## Setup some plotting options 
breaks_plt <- c(0,seq(150,350, 20))
labels_plt <- c("150< ",seq(150,330, 20), ' >350')
limits_plt <- c(0,350)
color_option <- "magma"
na_color <- "grey80"

##### 1st Wave snapshots ######################################################
## Figure 2B: Wave 1, 63 days prior to peak
fig2.b <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == alpha_peak-63) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'B', subtitle = (alpha_peak-63)); fig2.b

## Save Figure 2B
ggsave(plot = fig2.b,
       filename = "Figures/extra_figures/fig2_b_new.png",
       width = 16,
       height = 9, 
       dpi = 200)

## Figure 2c: Wave 1, 42 days prior to peak
fig2.c <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == alpha_peak-42) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'C', subtitle = (alpha_peak-42)); fig2.c

## Save Figure 2C
ggsave(plot = fig2.c,
       filename = "Figures/extra_figures/fig2_c.png",
       width = 16,
       height = 9, 
       dpi = 200)

## Figure 2d: Wave 1, 21 days prior to peak
fig2.d <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == alpha_peak-21) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'D', subtitle = (alpha_peak-21)); fig2.d

## Save Figure 2D
ggsave(plot = fig2.d,
       filename = "Figures/extra_figures/fig2_d_new.png",
       width = 16,
       height = 9, 
       dpi = 200)

## Figure 2e: Wave 1 at peak
fig2.e <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == alpha_peak) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'E', subtitle = (alpha_peak)); fig2.e

## Save Figure 2E
ggsave(plot = fig2.e,
       filename = "Figures/extra_figures/fig2_e.png",
       width = 16,
       height = 9, 
       dpi = 200)

##### 2nd Wave snapshots ######################################################
## Figure 2F: Wave 2, 63 days prior to peak
fig2.f <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == delta_peak-63) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'F', subtitle = (delta_peak-63)); fig2.f

## Save Figure 2F
ggsave(plot = fig2.f,
       filename = "Figures/extra_figures/fig2_f.png",
       width = 16,
       height = 9, 
       dpi = 200)

## Figure 2G: Wave 2, 42 days prior to peak
fig2.g <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == delta_peak-42) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       # name = "Estimated Infections/1000/week",
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        # name = "Estimated Infections/1000/week",
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'G', subtitle = (delta_peak-42)); fig2.g

## Save Figure 2G 
ggsave(plot = fig2.g,
       filename = "Figures/extra_figures/fig2_g.png",
       width = 16,
       height = 9, 
       dpi = 200)

## Figure 2H: Wave 2, 21 days prior to peak
fig2.h <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == delta_peak-21) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'H', subtitle = (delta_peak-21)); fig2.h

## Save Figure 2H
ggsave(plot = fig2.h,
       filename = "Figures/extra_figures/fig2_h.png",
       width = 16,
       height = 9, 
       dpi = 200)

## Figure 2I: Wave 2 at peak
fig2.i <- ggplot()+
  geom_sf(data = CAR_df_preomicron |> 
            filter(date == delta_peak) |> 
            st_transform(crs=5070),
          aes(fill = mean, 
              color = mean))+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'I', subtitle = (delta_peak)); fig2.i

## Save Figure 2I 
ggsave(plot = fig2.i,
       filename = "Figures/extra_figures/fig2_i.png",
       width = 16,
       height = 9, 
       dpi = 200)

### Collect all panels and plot Figure 2 together 
fig2theme <- theme(legend.position = "right",
                  legend.direction = "vertical",
                  legend.title.position = "left",
                  legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                  legend.key.height = grid::unit(1.6, "cm"),
                  legend.key.width = grid::unit(1, "cm"))

fig2 <- (((fig2.b + 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                    color = "none") + fig2theme | 
             fig2.c + 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                    color = "none") +
             fig2theme | 
             fig2.d + 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                    color = "none") +
             fig2theme | 
             fig2.e+ 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                    color = "none") +
             fig2theme)) / 
           (fig2a+ggtitle('A')) / 
           ((fig2.f+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                      color = "none") +
               fig2theme | 
               fig2.g+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                      color = "none") +
               fig2theme | 
               fig2.h+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                      color = "none") +
               fig2theme | 
               fig2.i+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week"),
                      color = "none") +
               fig2theme))
)+
  plot_layout(guides = "collect"); fig2

##### Save Figure 2 as a PNG and as a TIFF ####################################
ggsave(plot = fig2,
       filename = "Figures/fig2.png",
       width = 16, 
       height = 9,
       dpi = 300)

ggsave(plot = fig2,
       filename = "Figures/fig2.pdf",
       width = 16, 
       height = 9,
       dpi = 300)

###############################################################################
##### FIGURE 3 ################################################################
###############################################################################
## Daily areal expansion rate and wavefront speed compared between the two waves

##### Figure 3A: Areal expansion rate compared between wave 1 and wave 2 ######
fig3a <- ggplot()+
          geom_col(data = waveStatsJoined,
                   aes(x = `Days before peak`, 
                       y = `Recruitment rate (km2/day)`, 
                       fill = wave),
                   alpha = 0.75,
                   position = position_dodge())+
          geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
          geom_vline(xintercept = seq(7,63,7), lty = "dotted", color = "grey50")+
          theme_minimal()+
          colorspace::scale_fill_discrete_divergingx(name = "")+
          scale_y_continuous(labels = scales::label_comma()) + 
          labs(x = "Days before national curve peak", 
               y = "Daily mean areal expansion rate \n [km2/day]")+
          scale_x_reverse(breaks = seq(7,63,7))+
          colorspace::scale_fill_discrete_divergingx(name = "")+
          theme(legend.position = c(0.10,0.90),
                legend.text = element_text(size = 12),
                legend.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 12)) + 
          ggtitle("Areal expansion rate"); fig3a

### Save figure 3A
ggsave(plot = fig3a, filename = here("figures/fig3a.tif"), 
       width = 16, height = 9, dpi = 300)

##### Figure 3B: Wave front speed compared between wave 1 and wave 2
fig3b <- ggplot()+
          geom_col(data = waveStatsJoined,
                   aes(x = `Days before peak`, 
                       y = `Mean speed (km/day)`, 
                       fill = wave),
                   alpha = 0.75,
                   position = position_dodge())+
          geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
          geom_vline(xintercept = seq(7,63,7), lty = "dotted", color = "grey50")+
          theme_minimal()+
          labs(x = "Days before national curve peak", 
               y = "Daily mean wavefront speed \n [km/day]")+
          colorspace::scale_fill_discrete_divergingx(name = "")+
          scale_x_reverse(breaks = seq(7,63,7))+
          theme(legend.position = c(0.10,0.90),
                legend.text = element_text(size = 12),
                legend.title = element_text(hjust = 0.5),
                axis.text = element_text(size = 12)) + 
          ggtitle("Wavefront speed"); fig3b

### Save figure 3B
ggsave(plot = fig3b, filename = here("figures/fig3b.tif"), 
       width = 16, height = 9, dpi = 300)

## Save the two panels together as Figure 3
ggsave( plot = (fig3a+fig3b) + plot_layout(nrow=2, guides = "collect") & 
        theme(legend.position = 'bottom',
              legend.direction = 'horizontal'), 
        filename = here("figures/fig3.tif"),
        width = 8.5, height = 11, dpi = 300) 

###############################################################################
##### FIGURE 4 ################################################################
###############################################################################

##### FIRST WAVE ##############################################################

## Figure 4A: Wave 1 at 63 prior to peak
date_displayed <- alpha_peak-42

fig4a <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=firstBound |>
            filter(wave_dt == date_displayed),
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_first|>
            filter(date == date_displayed),
          mapping=aes(geometry= geometry,
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_first %>% filter(distToFront == 0)|>
            filter(date ==date_displayed),
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C",
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500),
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey30"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4a 

## Figure 4B: Wave 1 at 42 prior to peak
date_displayed <- "2020-10-22"

fig4b <- ggplot() + 
          geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
          geom_sf(data=firstBound |> 
                    filter(wave_dt == date_displayed), 
                  mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
          geom_sf(data=distanceToFront.df_first|> 
                    filter(date == date_displayed), 
                  mapping=aes(geometry= geometry, 
                              color=distToFront/1000), size=1.5) +
          geom_sf(data=distanceToFront.df_first %>% filter(distToFront ==0)|> 
                    filter(date == date_displayed), 
                  mapping=aes(geometry= geometry), color = "black", size=1.5) +
        geom_sf(data = us_states,
                color = "black",
                fill = "transparent")+
          scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                                # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                                breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                                
                                name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
          scale_fill_manual(values = c("grey80", "grey50"))+
          guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
          theme_void()+
          labs(title = date_displayed); fig4b

## Figure 4C: Wave 1 at 21 prior to peak
date_displayed <- "2020-11-05"

fig4c<- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=firstBound |> 
            filter(wave_dt ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_first|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_first %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4c

## Figure 4D: Wave 1 at peak
date_displayed <- "2020-11-17"

fig4d <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=firstBound |> 
            filter(wave_dt ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_first|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_first %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4d

## Second wave panels
## Figure 4F: Wave 2 at 63 prior to peak
date_displayed <- delta_peak-42
fig4e <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=secondBound |> 
            filter(wave_dt ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_second|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_second %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4e

## Figure 4F: Wave 2 at 42 prior to peak
date_displayed <- "2021-08-07"
fig4f <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=secondBound |> 
            filter(wave_dt ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_second|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_second %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4f

## Figure 4G: Wave 2 at 21 prior to peak
date_displayed <- "2021-08-21"
fig4g <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=secondBound |> 
            filter(wave_dt ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_second|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_second %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4g

## Figure 4H: Wave 2 at peak
date_displayed <- "2021-09-02"
fig4h <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=secondBound |> 
            filter(wave_dt ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_second|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_second %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        # breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        breaks=c(0, 10, 25, 50, 100, 200, 400, 800),
                        
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed); fig4h


fig4 <- (((fig4a / fig4b / fig4c / fig4d)) |
           # (fig4a) /
           ((fig4e / fig4f / fig4g / fig4h)))+
  plot_layout(guides = "collect"); fig4

ggsave(plot = fig4,
       filename = "figures/fig4_no_center.png",
       width = 9,
       height = 14,
       dpi = 300)

ggsave(plot = fig4,
       filename = "figures/fig4_no_center.pdf",
       width = 9,
       height = 14,
       dpi = 300)

##