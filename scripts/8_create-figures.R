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
hexpop <- st_read("Data/data-products/geo-hexes/pop/hexgrid_1100_km_meta_pop.shp")

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
  summarise(sum_infectionsPC = sum(infectionsPC, na.rm = T), 
            average_infectionsPC = sum(infectionsPC, na.rm = T)/nrow(hexgrid)*1e5) |> 
  st_drop_geometry()

## Cumulative infections across all dates for each hex
hexgrid_preomicron_cum <- hexgrid_preomicron %>%
  group_by(hexid) |> 
  summarise(cum_infections = sum(infections, na.rm = T)) |> 
  right_join(hexpop |>  
               mutate(hexid = as.character(hexid)) |> 
               select(hexid, population))  |> 
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
  select(!"...1") %>%
  mutate(wave = "1st")

wave2Stats <- vroom::vroom("Data/data-products/wavefronts/wave2Characteristics.csv") |> 
  select(!"...1") %>%
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
new_england_hexpop <- st_filter(hexpop, new_england_states)

new_england_states <- us_states|> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

## Figure 1B: Population across New England states
new_england_hex_plt <- ggplot()+
                       geom_sf(new_england_hexpop |> 
                                 filter(!is.na(population)) |> 
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
       filename = "Figures/extra_figures/fig1b_new.png",
       width = 16, 
       height = 9, 
       dpi = 100)

## Figure 1B: Population across Connecticut
ct_hexpop <- st_filter(hexpop, ct_counties)

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
ggsave(filename = "Figures/extra_figures/fig1d_new.png", 
       plot = us_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

##### New England Infections ##################################################
new_england_grid_infections <- st_filter(hexgrid_preomicron_cum, new_england_states)

new_england_hexes <- st_filter(hexgrid, new_england_states)

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

## Save figure 1#
ggsave(filename = "Figures/extra_figures/fig1e_new.png", 
       plot = new_england_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

###### Connecticut infections #################################################

ct_grid_infection <- st_filter(hexgrid_preomicron_cum, ct_counties)

ct_hexes <- st_filter(hexgrid, ct_counties)

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
        axis.text = element_text(size = 6))
ct_hex_infections

ggsave(filename = "Figures/extra_figures/fig1f_new.png", 
       plot = ct_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

hex_infections <- (us_hex_infections | (new_england_hex_infections / ct_hex_infections))+
  plot_annotation(tag_levels = 'A')+
  plot_layout(widths = c(4,1,1),
              heights = c(4,1,1),
              guides = 'keep')
hex_infections
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
annotationValues <- infections_daily_cum %>% 
  filter(date %in% c(alpha_peak-c(63,42,21,0),
                     delta_peak-c(63,42,21,0))) %>% 
  select(average_infectionsPC) %>% unlist() %>% as.vector()

fig2a <- 
  ggplot()+
  geom_line(data = infections_daily_cum,
            aes(x = date, 
                y = average_infectionsPC))+
  theme_minimal()+
  scale_x_date(name = "Date",
               date_breaks = "4 months",
               date_labels = "%b %y'",
               limits = c(as.Date("2020-02-01"), 
                          as.Date("2021-11-01")))+
  scale_y_continuous(name = "Average daily infections per 100,000 persons",
                     labels = scales::label_comma())+
  ## Alpha wave marks
  annotate("rect",
           xmin = alpha_peak - 70,
           xmax = alpha_peak + 7,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(alpha_peak-63,
                 alpha_peak-42,
                 alpha_peak-21,
                 alpha_peak),
           y = rep(250, 4),
           label = LETTERS[2:5],
           size = 7)+
  annotate("segment",
           y = annotationValues[1:4],
           yend = rep(245,4),
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
                 delta_peak-42,
                 delta_peak-21,
                 delta_peak),
           y = rep(0, 4),
           label = LETTERS[6:9],
           size = 7)+
  annotate("segment",
           y = annotationValues[5:8],
           yend = rep(5,4),
           x = c(delta_peak-63,
                 delta_peak-42,
                 delta_peak-21,
                 delta_peak),
           xend = c(delta_peak-63,
                    delta_peak-42,
                    delta_peak-21,
                    delta_peak),
           color = "grey50",
           linetype = "dashed"); fig2a

##### Save Figure 2A 
ggsave(filename = "figures/extra_figures/fig2a.png",
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
       filename = "Figures/extra_figures/fig2_b.png",
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
       filename = "Figures/extra_figures/fig2_d.png",
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
       filename = "Figures/fig2.tiff",
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
ggsave(plot = fig3a, filename = here("figures/extra_figures/fig3a.png"), 
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
ggsave(plot = fig3b, filename = here("figures/extra_figures/fig3b.png"), 
       width = 16, height = 9, dpi = 300)

## Save the two panels together as Figure 3
ggsave( plot = (fig3a+fig3b) + plot_layout(nrow=2, guides = "collect") & 
        theme(legend.position = 'bottom',
              legend.direction = 'horizontal'), 
        filename = here("figures/fig3.tiff"),
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

###############################################################################
###### Sensitivity Analysis ###################################################
###############################################################################
color_option <- "inferno"
figS2 <- ggplot(data = CAR_df_preomicron |> 
                  filter(date %in% c(delta_peak, alpha_peak,
                                     delta_peak-21, alpha_peak - 21,
                                     delta_peak-42, alpha_peak - 42,
                                     delta_peak-63, alpha_peak - 63)) |> 
                  st_transform(crs=26915),
                aes(fill = mean, 
                    color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k",
                       direction = -1
  )+
  scale_color_viridis_c(option = color_option,
                        name = "Estimated Infections/1000/week",
                        direction = -1
  )+
  theme_void()+
  guides(color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  facet_wrap(.~date, nrow = 2); figS2

ggsave(plot = figS2,
       filename = "Figures/extra_figures/figS2.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = figS2,
       filename = "Figures/extra_figures/figS2.pdf",
       width = 16, 
       height = 9,
       dpi = 300)

## Breakdowns of each peaks
breaks_plt <- c(0,seq(127,300, 20))
labels_plt <- c("127< ",seq(127,280, 20), ' >300')
limits_plt <- c(0,350)
color_option <- "magma"
na_color <- "grey70"

figS3 <- ggplot(data = CAR_df_preomicron |> 
                  filter(date %in% c(delta_peak, alpha_peak,
                                     delta_peak-21, alpha_peak - 21,
                                     delta_peak-42, alpha_peak - 42,
                                     delta_peak-63, alpha_peak - 63)) |> 
                  st_transform(crs=26915),
                aes(fill = mean, 
                    color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_b(option = color_option,
                       name = "Estimated Infections/100k",
                       direction = -1,
                       na.value = na_color,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
                        name = "Estimated Infections/1000/week",
                        direction = -1,
                        na.value = na_color,
                        breaks = breaks_plt,
                        labels = labels_plt,
                        limits = limits_plt,
  )+
  theme_void()+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  facet_wrap(.~date, nrow = 2); figS3

ggsave(plot = figS3,
       filename = "Figures/extra_figures/figS3.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = figS3,
       filename = "Figures/extra_figures/figS3.pdf",
       width = 16, 
       height = 9,
       dpi = 300)


## Sensitivity Analys on threshold values for the risk surface
## Fig2A - Layered depiction on transforming estimated infections on counties polygon on hexgrid

figS2a <- ggplot(data = CAR_df_preomicron,
                 aes(x = mean))+
  geom_histogram(bins = 500)+
  theme_minimal()+
  # xlim(c(0,300))+
  labs(x = expression("Random effects " *theta ~ "(Ai)"),
       y = "Frequency")+
  scale_y_continuous(labels = scales::label_comma())+
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10))
figS2a

ecdf_mean <- ecdf(CAR_df_preomicron$mean)
ecdf_upper <- ecdf(CAR_df_preomicron$`0.975quant`)
ecdf_lower <- ecdf(CAR_df_preomicron$`0.025quant`)
# ecdf_median <- ecdf(CAR_df_preomicron$`0.5quant`)

figS2b <- ggplot(data = CAR_df_preomicron,
                 aes(x = mean, y = ecdf_mean(mean)))+
  geom_line()+
  geom_ribbon(aes(x = mean,
                  ymin = ecdf_lower(`0.025quant`),
                  ymax = ecdf_upper(`0.975quant`)))+
  geom_vline(xintercept = 189, linetype = "dashed")+
  theme_minimal()+
  labs(x = expression("Random effects " *theta ~ "(Ai)"),
       y = "Percentile")+
  xlim(c(0, NA))
figS2b

figS2b <- ggplot(CAR_df_preomicron) +
  # geom_histogram(aes(mean), bins = 500)+
  stat_ecdf(aes(mean, color = "median"), geom = "line", pad = FALSE)+
  stat_ecdf(aes(`0.025quant`, color = "lower"), geom = "step", pad = FALSE)+
  stat_ecdf(aes(`0.975quant`, color = "upper"), geom = "step", pad = FALSE)+
  geom_vline(xintercept = 189, linetype = "dashed")+
  theme_minimal()+
  labs(x = expression("Random effects " *theta ~ "(Ai)"),
       y = "Percentile",
       color = "")+
  scale_y_continuous(labels = scales::label_number())+
  scale_x_continuous(breaks = scales::breaks_pretty(n = 10))+
  scale_color_manual(values = c("grey30", "black", "grey50"))+
  xlim(c(0, NA))
figS2b

ggsave(filename = "Figures/extra_figures/figS1b.png",
       plot = figS2b,
       width = 16,
       height = 9,
       dpi = 300)


figS2 <- (figS2a | figS2b)
figS2

ggsave(filename = "Figures/extra_figures/figS1.png",
       plot = figS2,
       width = 16, 
       height = 9, 
       dpi = 100)

## Sensitivity Analys on threshold values for the risk surface
## Fig2A - Layered depiction on transforming estimated infections on counties polygon on hexgrid

## Threshold for filtering given the distribution, any value of trend that it 
## is above the 3rd quartile of the trend distribution
# threshold_mean <- quantile(CAR_df_preomicron$mean, probs = 0.90, na.rm = TRUE)

## Set to 85 and 300 to produce a and b panels for figure S2
# threshold_mean <- quantile(CAR_df_preomicron$mean, probs = 0.50, na.rm = TRUE)
# threshold_mean <- quantile(CAR_df_preomicron$mean, probs = 0.99, na.rm = TRUE)


## Threshold for filtering given the distribution, any value of trend that it is above the 3rd Quarter of the trend distribution
threshold_mean <- quantile(CAR_df_preomicron$mean, prob=0.75, na.rm=TRUE)

##### Wave 1 ##################################################################

CAR_lag_alpha <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(from = alpha_peak-63,
                            to = alpha_peak,
                            length.out = 63))|> 
  mutate(contour_surface = if_else(mean >= threshold_mean, date - (alpha_peak-63), NA))|> 
  st_transform(crs = 5070)

contour_alpha <- CAR_lag_alpha|> 
  filter(!is.na(contour_surface)) |> 
  group_by(contour_surface, date) |> 
  summarise(geometry = st_union(geometry),
            area = format(round(units::set_units(st_area(st_union(geometry)), 
                                                 km^2),0), 
                          big.mark = ",")) |> 
  arrange(date)

##### Wave 2 ##################################################################

CAR_lag_delta <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(from = delta_peak-63,
                            to = delta_peak,
                            length.out = 63))|> 
  mutate(contour_surface = if_else(mean >= threshold_mean, date - (delta_peak-63), NA))|> 
  st_transform(crs = 5070)

contour_delta <- CAR_lag_delta|> 
  filter(!is.na(contour_surface)) |> 
  group_by(contour_surface, date) |> 
  summarise(geometry = st_union(geometry),
            area = format(round(units::set_units(st_area(st_union(geometry)), 
                                                 km^2),0), 
                          big.mark = ",")) |> 
  arrange(date)

## Speed distribution
CAR_area_alpha <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(alpha_peak, (alpha_peak-63), length.out = 63))|> 
  filter(mean >= threshold_mean) |> 
  mutate(days = (alpha_peak - date))|> 
  st_transform(crs = 5070) |> 
  group_by(days) |> 
  summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
  mutate(wave = "1st wave")

## Speed and Velocity calculation
## Alpha
CAR_area_alpha$speed <- units::set_units(c(0, 
                                           -diff(as.numeric(CAR_area_alpha$area))), km^2/day)

## Velocity
CAR_velocity_alpha <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(alpha_peak, (alpha_peak-63), length.out = 63))|> 
  filter(mean >= threshold_mean) |> 
  mutate(days = (alpha_peak - date))|> 
  st_transform(crs = 5070) |> 
  group_by(days) |> 
  summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  mutate(wave = "1st wave")

CAR_area_alpha$velocity <- units::set_units(c(0, 
                                              -diff(as.numeric(CAR_velocity_alpha$area))), km^2/day)

CAR_velocity_alpha$velocity <- units::set_units(c(0, 
                                                  -diff(as.numeric(CAR_velocity_alpha$area))), km^2/day)

## Delta
CAR_area_delta <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(delta_peak, (delta_peak-63), length.out = 63))|> 
  filter(mean >= threshold_mean) |> 
  mutate(days = (delta_peak - date))|> 
  st_transform(crs = 5070) |> 
  group_by(days) |> 
  summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
  mutate(wave = "2nd wave")

## Speed/Velocity calculation
CAR_area_delta$speed <- units::set_units(c(0, 
                                           -diff(as.numeric(CAR_area_delta$area))), km^2/day)

## Velocity
CAR_velocity_delta <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(delta_peak, (delta_peak-63), length.out = 63))|> 
  filter(mean >= threshold_mean) |> 
  mutate(days = (delta_peak - date))|> 
  st_transform(crs = 5070) |> 
  group_by(days) |> 
  summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  mutate(wave = "2nd wave")

CAR_area_delta$velocity <- units::set_units(c(0, 
                                              -diff(as.numeric(CAR_velocity_delta$area))), km^2/day)

CAR_velocity_delta$velocity <- units::set_units(c(0, 
                                                  -diff(as.numeric(CAR_velocity_delta$area))), km^2/day)
## Speed distribution
CAR_joined <- rbind(CAR_area_alpha, CAR_area_delta)

fig4c <- ggplot()+
  geom_col(data = CAR_joined,
           aes(x = days, y = speed, fill = wave),
           alpha = 0.75,
           position = position_dodge())+
  geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
  geom_vline(xintercept = seq(7,63,7), lty = "dotted", color = "grey50")+
  theme_minimal()+
  labs(x = "Days before national curve peak", 
       y = "Rate of areal expansion")+
  scale_x_reverse(breaks = seq(7,63,7))+
  units::scale_y_units(labels = scales::label_comma(),
                       breaks = scales::breaks_extended(n = 10))+
  colorspace::scale_fill_discrete_divergingx(name = "")+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12)) + 
  ggtitle("Rate of areal expansion")
fig4c

ggsave(filename = here("figures/fig3.png"),
       plot = fig4c,
       width = 16,
       height = 9,
       dpi = 200)

ggsave(filename = here("figures/fig3.pdf"),
       plot = fig4c,
       width = 16,
       height = 9,
       dpi = 200)

## Making figure S2
## After running the line 1063 to 1347 with threshold_mean set to 127 and 233, we
figS2a <- fig4c
figS2b <- fig4c

figS2 <- (figS2a | figS2b)
figS2

ggsave(filename = "Figures/figS2.png",
       plot = figS2,
       width = 16, 
       height = 9, 
       dpi = 300)

ggsave(filename = "Figures/figS2.pdf",
       plot = figS2,
       width = 16, 
       height = 9, 
       dpi = 300)

# ## Final Layered figure
# fig4_layered <- ((fig4a_layered / fig4b_layered)| fig4c)+
#   # plot_layout(guides = "keep")&
#   theme(legend.position = "bottom")
# fig4_layered

# ggsave(filename = "Figures/extra_figures/fig4_layered.png",
#        plot = fig4_layered,
#        width = 16, 
#        height = 9, 
#        dpi = 100)

# ggsave(filename = "Figures/extra_figures/fig4_layered.pdf",
#        plot = fig4_layered,
#        width = 16, 
#        height = 9, 
#        dpi = 300)