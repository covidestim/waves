rm(list = ls())
gc()

library(tidyverse)
library(sf)
# library(MoMAColors)

##### Hexgrid |
hexes <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
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
length(unique(na.omit(hexes$hexid)))
##############################################################################|
## Pre-Omicron infections allocated to hexgrid
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
  filter(population > 0)# |> 
#   mutate(infectionsPC = case_when(population == 0 ~ 0,
#                                   population !=0 & infectionsPC == 0 ~ NA,
#                                   TRUE~infectionsPC))

## Figure2
# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

## Population hexes
## Hexgrid Population
hexpop <- vroom::vroom("Data/data-products/geo-hexes/pop/hexgrid_1100_km_meta_pop.csv") |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  st_as_sf()

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()|> 
  st_transform(crs = 5070)

new_england_states <- us_states |> 
  dplyr::filter(GEOID %in% c("09","23","25","33","44","50"))

new_england_counties <- tigris::counties(state = c("09","23","25","33","44","50"))|> 
  st_transform(crs = 5070)

ct_counties <- tigris::counties(state = 09)|> 
  st_transform(crs = 5070)

## Hexgrid plots
# high <- "deeppink1"
# low <- "thistle1"
palette <- "Purple-Yellow"

us_hex_plt <- ggplot() + 
  # geom_sf(data = hexes|>
  #           filter(as.integer(hexid) < 7662) |>
  #           st_transform(crs = 'ESRI:102009'),
  #         fill = 'transparent')+
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
  # scale_colour_manual(values=NA) +              
  # guides(colour=guide_legend("No data", override.aes=list(colour="grey60")))+
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(1, "in"),
        axis.text = element_text(size = 6))
us_hex_plt

ggsave(plot = us_hex_plt,
       filename = "Figures/extra_figures/fig1a_new.png",
       width = 16,
       height = 9,
       dpi = 100)

## hexes to county columns
hexes_to_county <- vroom::vroom("Data/data-sources/hexid-fips-map.csv") |> mutate(hexid = as.character(hexid))

hexpop <- hexpop |> 
  left_join(hexes_to_county |> select(hexid, fips)) |> 
  mutate(STATEFP = str_sub(fips, 1, 2))

new_england_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_states <- us_states|> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_plt <- ggplot()+
  geom_sf(new_england_hexpop |> 
            filter(!is.na(population)) |> 
            st_transform(crs = 5070),
          mapping=aes(fill= log10(population+1)))+ 
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
  # scale_colour_manual(values=NA) +              
  # guides(colour=guide_legend("No data", override.aes=list(colour="grey60")))+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(1, "in"),
        axis.text = element_text(size = 6))
new_england_hex_plt

ggsave(plot = new_england_hex_plt,
       filename = "Figures/extra_figures/fig1b_new.png",
       width = 16, 
       height = 9, 
       dpi = 100)

ct_hexpop <- hexpop |> 
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
  # scale_colour_manual(values=NA) +              
  # guides(colour=guide_legend("No data", override.aes=list(colour="grey60")))+
  theme_minimal()+
  theme(legend.position = "none", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(1, "in"),
        axis.text = element_text(size = 6))
ct_hex_plt

ggsave(filename = "Figures/extra_figures/fig1c_new.png",
       plot = ct_hex_plt,
       width = 16,
       height = 9, 
       dpi = 100)

library(patchwork)

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

## Pre-Omicron
# hexgrid_preomicron <- vroom::vroom("Data/data-products/hexid-observations_preomicron_meta30m.csv") |>
#   mutate(hexid = as.character(hexid),
#          date = as.Date(date)) |>
#   select(-geometry) |>
#   mutate(infectionsPC = (infections/population)*1e5) |>
#   # filter(infectionsPC >= 1) |>
#   left_join(hexgrid_pop |> select(hexid, geometry) |> mutate(hexid = as.character()), by = "hexid") |>
#   sf::st_as_sf()

## Hexgrid Population
hexpop <- vroom::vroom("Data/data-products/geo-hexes/pop/hexgrid_1100_km_meta_pop.csv") |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  st_as_sf()

## Only keep hexes with a less than 10 cumulative infections per capita
hexgrid_preomicron_cum <- vroom::vroom("Data/data-products/geo-hexes/hexid-observations_preomicron_intersection_hexgrid1100km.csv") %>% 
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
  filter(population > 0) |> 
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

## hexes to county columns
hexes_to_county <- vroom::vroom("Data/data-sources/hexid-fips-map.csv") |> mutate(hexid = as.character(hexid))

hexgrid_preomicron_cum <- hexgrid_preomicron_cum |> 
  left_join(hexes_to_county |> select(hexid, fips) |> mutate(hexid = as.character(hexid))) |> 
  mutate(STATEFP = as.character(substr(fips, 1, 2)))

# breaks_plt <- seq(1,1001, 100)
# labels_plt <- c("1>", seq(100,900, 100), '1,000+')
# limits_plt <- c(0,1000)
color_option <- "inferno"

### Pre-Omicron
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
        axis.text = element_text(size = 6))
us_hex_infections

ggsave(filename = "Figures/extra_figures/fig1d.png", 
       plot = us_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

## New England zooming
new_england_grid_infections <- hexgrid_preomicron_cum |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hexes <- hexes |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes_to_county |> select(hexid, fips) |> mutate(hexid = as.character(hexid))) |> 
  mutate(STATEFP = str_sub(fips, 1, 2)) |> 
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
        axis.text = element_text(size = 6))
new_england_hex_infections

ct_grid_infection <- hexgrid_preomicron_cum |> 
  filter(STATEFP == "09")

ct_hexes <- hexes |> 
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
        axis.text = element_text(size = 6))
ct_hex_infections

library(patchwork)
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

## Patchwork all together
fig1 <- (hexpop_zoom / hex_infections)+
  plot_annotation(tag_level = 'A')+
  plot_layout(widths = c(3,1))&
  theme(plot.tag = element_text(size = 18),
        axis.text = element_text(size = 4))
fig1

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

## hexgrids
dataset <- "adaptiveStartVals"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("Data/data-products/car_",
                                         dataset, 
                                         ".csv"))

## Fig.2 Spatial hexes, population and infections

fig2a_data <- vroom::vroom("Data/data-products/tsa_preomicron.csv") |> 
  group_by(date) |> 
  summarise(cum_infections = sum(infectionsPC, na.rm = T)) |> 
  arrange(desc(date))

# # Function that finds the closest date in a vector of dates.
# find_closest_date <- function(date, date_vector)
# {
#   date_vector <- unique(date_vector)
#   diffs <- abs(date - date_vector)
#   
#   # Two dates in date_vector can have the same distance to date, by adding
#   # `[1]` we pick whichever comes first in date_vector.
#   date_vector[diffs == min(diffs)][1]
# }

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

fig2a <- ggplot()+
  geom_line(data = fig2a_data,
            aes(x = date, 
                y = cum_infections))+
  theme_minimal()+
  scale_x_date(name = "Date",
               date_breaks = "4 months",
               date_labels = "%b %y'",
               limits = c(as.Date("2020-02-01"), 
                          as.Date("2021-11-01")))+
  scale_y_continuous(name = "Estimated infection/day",
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
           fill = "grey50",alpha = 0.2)+
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
           linetype = "dashed")
fig2a

ggsave(filename = "Figures/extra_figures/fig2a_new.png",
       plot = fig2a,
       width = 16,
       height =9, 
       dpi = 100)

## Figure 2, TSA
## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,350, 20))
labels_plt <- c("150< ",seq(150,330, 20), ' >350')
limits_plt <- c(0,350)
color_option <- "magma"
na_color <- "grey80"

# color_option <- "magma"
# na_color <- "grey70"
# ## Breakdowns of each peaks
# breaks_plt <- c(0,seq(190,550, ))
# labels_plt <- c("25< ",seq(25,250, 25), ' >250')
# limits_plt <- c(0,300)

## Make the plots with the gif codes

# CAR_df_preomicron <- vroom::vroom("Data/data-products/tsa_meta30m_run_preomicron_daily.csv")

CAR_df_preomicron_w1 <- readRDS("Data/data-products/car_list_wave1_adaptStart.rds") |> bind_rows() |> mutate(wave = "1st Wave")
CAR_df_preomicron_w2 <- readRDS("Data/data-products/car_list_wave2_adaptStart.rds") |> bind_rows() |> mutate(wave = "2nd Wave")

CAR_df_preomicron <- rbind(CAR_df_preomicron_w1, CAR_df_preomicron_w2)

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

CAR_df_preomicron <- CAR_df_preomicron |> 
  left_join(hexgrid) |> 
  st_as_sf()

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

## 1st Wave snapshots
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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'B', subtitle = (alpha_peak-63))
fig2.b

ggsave(plot = fig2.b,
       filename = "Figures/extra_figures/fig2_b_new.png",
       width = 16,
       height = 9, 
       dpi = 200)

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'C', subtitle = (alpha_peak-42))
fig2.c

ggsave(plot = fig2.c,
       filename = "Figures/extra_figures/fig2_c.png",
       width = 16,
       height = 9, 
       dpi = 200)

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'D', subtitle = (alpha_peak-21))
fig2.d

ggsave(plot = fig2.d,
       filename = "Figures/extra_figures/fig2_d_new.png",
       width = 16,
       height = 9, 
       dpi = 200)

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'E', subtitle = (alpha_peak))
fig2.e

ggsave(plot = fig2.e,
       filename = "Figures/extra_figures/fig2_e.png",
       width = 16,
       height = 9, 
       dpi = 200)

## 2nd Wave snapshots

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'F', subtitle = (delta_peak-63))
fig2.f

ggsave(plot = fig2.f,
       filename = "Figures/extra_figures/fig2_f.png",
       width = 16,
       height = 9, 
       dpi = 200)

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'G', subtitle = (delta_peak-63))
fig2.g

ggsave(plot = fig2.g,
       filename = "Figures/extra_figures/fig2_g.png",
       width = 16,
       height = 9, 
       dpi = 200)

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'H', subtitle = (delta_peak-63))
fig2.h

ggsave(plot = fig2.h,
       filename = "Figures/extra_figures/fig2_h.png",
       width = 16,
       height = 9, 
       dpi = 200)

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
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = 'I', subtitle = (delta_peak))
fig2.i

ggsave(plot = fig2.i,
       filename = "Figures/extra_figures/fig2_i.png",
       width = 16,
       height = 9, 
       dpi = 200)

library(patchwork)
fig2 <- (((fig2.b+ 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                      title.position = "left",
                                      title.hjust = 0.5),
                    color = "none") +
             theme(legend.position = "right",
                   legend.direction = "vertical",
                   legend.title.position = "left",
                   legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                   legend.key.height = grid::unit(1.6, "cm"),
                   legend.key.width = grid::unit(1, "cm")) | 
             fig2.c+ 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                      title.position = "left",
                                      title.hjust = 0.5),
                    color = "none") +
             theme(legend.position = "right",
                   legend.direction = "vertical",
                   legend.title.position = "left",
                   legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                   legend.key.height = grid::unit(1.6, "cm"),
                   legend.key.width = grid::unit(1, "cm")) | 
             fig2.d+ 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                      title.position = "left",
                                      title.hjust = 0.5),
                    color = "none") +
             theme(legend.position = "right",
                   legend.direction = "vertical",
                   legend.title.position = "left",
                   legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                   legend.key.height = grid::unit(1.6, "cm"),
                   legend.key.width = grid::unit(1, "cm")) | 
             fig2.e+ 
             guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                      title.position = "left",
                                      title.hjust = 0.5),
                    color = "none") +
             theme(legend.position = "right",
                   legend.direction = "vertical",
                   legend.title.position = "left",
                   legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                   legend.key.height = grid::unit(1.6, "cm"),
                   legend.key.width = grid::unit(1, "cm")))) / 
           (fig2a+ggtitle('A')) / 
           ((fig2.f+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                        title.position = "left",
                                        title.hjust = 0.5),
                      color = "none") +
               theme(legend.position = "right",
                     legend.direction = "vertical",
                     legend.title.position = "left",
                     legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                     legend.key.height = grid::unit(1.6, "cm"),
                     legend.key.width = grid::unit(1, "cm")) | 
               fig2.g+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                        title.position = "left",
                                        title.hjust = 0.5),
                      color = "none") +
               theme(legend.position = "right",
                     legend.direction = "vertical",
                     legend.title.position = "left",
                     legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                     legend.key.height = grid::unit(1.6, "cm"),
                     legend.key.width = grid::unit(1, "cm")) | 
               fig2.h+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                        title.position = "left",
                                        title.hjust = 0.5),
                      color = "none") +
               theme(legend.position = "right",
                     legend.direction = "vertical",
                     legend.title.position = "left",
                     legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                     legend.key.height = grid::unit(1.6, "cm"),
                     legend.key.width = grid::unit(1, "cm")) | 
               fig2.i+ 
               guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                                        title.position = "left",
                                        title.hjust = 0.5),
                      color = "none") +
               theme(legend.position = "right",
                     legend.direction = "vertical",
                     legend.title.position = "left",
                     legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
                     legend.key.height = grid::unit(1.6, "cm"),
                     legend.key.width = grid::unit(1, "cm"))))
)+
  plot_layout(guides = "collect")
fig2

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

## Sensitivity Analysis
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
                       direction = -1,
                       # na.value = na_color,
                       #  breaks = breaks_plt,
                       # labels = scales::label_math(),
                       # limits = limits_plt,
  )+
  scale_color_viridis_c(option = color_option,
                        name = "Estimated Infections/1000/week",
                        direction = -1,
                        # na.value = na_color,
                        # breaks = breaks_plt,
                        # labels = labels_plt,
                        # limits = limits_plt,
  )+
  theme_void()+
  guides(color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  facet_wrap(.~date, nrow = 2)
figS2

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
  facet_wrap(.~date, nrow = 2)
figS3

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

## Figure S4 correlation between trend of 'alpha', 'delta'
library(units)

## hexgrids
# dataset <- "meta30m_run_preomicron_daily"

# ## Pre-Omicron
# CAR_df_preomicron <- vroom::vroom(paste0("Data/data-products/tsa_",
#                                          dataset, 
#                                          ".csv"))
## peak date
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

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

library(patchwork)

figS2 <- (figS2a | figS2b)
figS2

ggsave(filename = "Figures/extra_figures/figS1.png",
       plot = figS2,
       width = 16, 
       height = 9, 
       dpi = 100)

## Threshold for filtering given the distribution, any value of trend that it is above the 3rd Quarter of the trend distribution
threshold_mean <- quantile(CAR_df_preomicron$mean, probs = 0.90, na.rm = TRUE)

## Set to 85 and 300 to produce a and b panels for figure S2
threshold_mean <- quantile(CAR_df_preomicron$mean, probs = 0.50, na.rm = TRUE)
threshold_mean <- quantile(CAR_df_preomicron$mean, probs = 0.99, na.rm = TRUE)

## hexgrids
dataset <- "adaptiveStartVals"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("Data/data-products/car_",
                                         dataset, 
                                         ".csv"))

## Threshold for filtering given the distribution, any value of trend that it is above the 3rd Quarter of the trend distribution
threshold_mean <- quantile(CAR_df_preomicron$mean, prob=0.75, na.rm=TRUE)

## peak date
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

## Hexgrid
hexes1 <- sf::st_read("Data/data-products/geo-hexes/hexgrid_1100_km_meta_pop.shp")

## Pre-Omicron
CAR_lag_alpha <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
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

## Delta
CAR_lag_delta <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
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
  left_join(hexes) |> 
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
  left_join(hexes) |> 
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
  left_join(hexes) |> 
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
  left_join(hexes) |> 
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

hexgrid2 <- sf::st_read("Data/data-products/hexgrid.geojson")
hexgrid <- sf::st_read("Data/data-products/geo-hexes/hexgrid_1100_km_meta_pop.shp")

## Heatmaps for the velocity
distanceToFront.df_first <- sf::st_read("Data/data-products/boundaryPlots/firstWaveModel/firstDistanceToFront.shp") |> 
  rename(distToFront = dstTFrn)

distanceToFront.df_second <- sf::st_read("Data/data-products/boundaryPlots/secondWaveModel/secondDistanceToFront.shp")|> 
  rename(distToFront = dstTFrn)

wave1Stats <- vroom::vroom("Data/data-products/wave1Characteristics.csv") |> 
  mutate(wave = "1st")

wave2Stats <- vroom::vroom("Data/data-products/wave2Characteristics.csv") |> 
  mutate(wave = "2nd")

waveStatsJoined <- rbind(wave1Stats, wave2Stats) %>%
                   mutate(wave = ifelse(wave == "1st", 
                                        "1st wave", 
                                        "2nd wave"))
colnames(waveStatsJoined)[1] <- "Days before peak"
colnames(waveStatsJoined)[3] <- "Recruitment rate (km2/day)"
colnames(waveStatsJoined)[5] <- "Mean speed (km/day)"

fig4a <- ggplot()+
  geom_col(data = waveStatsJoined,
           aes(x = `Days before peak`, y = `Recruitment rate (km2/day)`, fill = wave),
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
  ggtitle("Areal expansion rate")

fig4a

ggsave(plot = fig4a, filename = here("figures/fig3a.tif"), width = 16, height = 9, dpi = 300)

fig4b <- ggplot()+
  geom_col(data = waveStatsJoined,
           aes(x = `Days before peak`, y = `Mean speed (km/day)`, fill = wave),
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
  ggtitle("Wavefront speed")
fig4b

ggsave(plot = fig4b, filename = here("figures/fig3b.tif"), width = 16, height = 9, dpi = 300)

ggsave( plot = (fig4a+fig4b) + plot_layout(nrow=2, guides = "collect") & 
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal'), 
  filename = here("figures/fig3.tif"),
  width = 8.5, height = 11, dpi = 300) 

## Wavefront speed figure
## first wave panels
firstBound <- st_read("Data/data-products/firstWaveBound.shp")
secondBound <- st_read("~/Downloads/SecondWaveBound.shp")

date_displayed <- alpha_peak-63

plt1 <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=firstBound |> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) + 
  # geom_sf(data=distanceToFront.df_first|>
  #           filter(date ==date_displayed),
  #         mapping=aes(geometry= geometry,
  #                     color=distToFront/1000), size=1.5) +
  # geom_sf(data=distanceToFront.df_first %>% filter(distToFront == 0)|>
  #           filter(date ==date_displayed),
  #         mapping=aes(geometry= geometry), color = "black", size=1.5) +
  # scale_color_viridis_b(limits=c(0, 1500), option = "C", 
  #                       breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
  #                       name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  # scale_fill_manual(values = c("grey80", "grey30"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt1

date_displayed <- alpha_peak-42
plt2 <- ggplot() + 
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
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt2

date_displayed <- alpha_peak-21
plt3 <- ggplot() + 
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
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt3

date_displayed <- alpha_peak
plt4 <- ggplot() + 
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
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt4

## Second wave panels

date_displayed <- delta_peak-63
plt5 <- ggplot() + 
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
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt5

date_displayed <- delta_peak-42
plt6 <- ggplot() + 
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
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt6

date_displayed <- delta_peak-21
plt7 <- ggplot() + 
  geom_sf(data=hexgrid, mapping=aes(geometry= geometry, fill="Hex in a wave")) +
  geom_sf(data=secondBound |> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, fill="Hex not in a wave")) +
  geom_sf(data=distanceToFront.df_second|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry, 
                      color=distToFront/1000), size=1.5) +
  geom_sf(data=distanceToFront.df_second %>% filter(distToFront ==0)|> 
            filter(date ==date_displayed), 
          mapping=aes(geometry= geometry), color = "black", size=1.5) +
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt7


date_displayed <- delta_peak
plt8 <- ggplot() + 
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
  scale_color_viridis_b(limits=c(0, 1500), option = "C", 
                        breaks=c(0, 50, 100, 200, 500, 1000, 1500), 
                        name = "Distance to nearest point\non boundary at t+1\n(in km/day)") +
  scale_fill_manual(values = c("grey80", "grey50"))+
  guides(fill = guide_legend(override.aes = list(size = 5, shape = 17)))+
  theme_void()+
  labs(title = date_displayed)
plt8


library(patchwork)
fig4 <- (((plt1 / plt2 / plt3 / plt4)) |
           # (fig4a) /
           ((plt5 / plt6 / plt7 / plt8)))+
  plot_layout(guides = "collect")
fig4

ggsave(plot = fig4,
       filename = "Figures/fig4_no_center.png",
       width = 9,
       height = 14,
       dpi = 300)

ggsave(plot = fig4,
       filename = "Figures/fig4_no_center.pdf",
       width = 9,
       height = 14,
       dpi = 300)

##