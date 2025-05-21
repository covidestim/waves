rm(list = ls())
gc()

library(tidyverse)
library(sf)
# library(MoMAColors)

## Reading hex grid
hexes <- st_read("data-products/geo-hexes/hexes.shp") 

hexes_to_county <- vroom::vroom("data-products/geo-hexes/hexid-fips-map.csv")

## Figure2
# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

## New hexgrid with Meta 30m population
hexpop <- st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662) |> ## Filtering out Puerto Rico hexes
  st_transform(crs = 26915) |>
  rename(population = metapop_30m)

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()|> 
  st_transform(crs = 26915)

# ## Highways
# highways <- tigris::primary_roads(year = 2024) |> 
#   filter(RTTYP == "I",
#          !grepl("H", FULLNAME)) |> 
#   st_transform(crs = 26915)

new_england_states <- us_states |> 
  dplyr::filter(GEOID %in% c("09","23","25","33","44","50"))

new_england_counties <- tigris::counties(state = c("09","23","25","33","44","50"))|> 
  st_transform(crs = 26915)

ct_counties <- tigris::counties(state = 09)|> 
  st_transform(crs = 26915)

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
            st_transform(crs = 'ESRI:102009'),
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
       filename = "img/extra_figures/fig1a_new.png",
       width = 16,
       height = 9,
       dpi = 100)

## hexes to county columns
hexes_to_county <- vroom::vroom("data-products/geo-hexes/hexid-fips-map.csv")

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
            st_transform(crs = 'ESRI:102009'),
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
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(1, "in"),
        axis.text = element_text(size = 6))
new_england_hex_plt

ggsave(plot = new_england_hex_plt,
       filename = "img/extra_figures/fig1b_new.png",
       width = 16, 
       height = 9, 
       dpi = 100)

ct_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09"))

ct_hex_plt <- ggplot()+
  geom_sf(ct_hexpop |> 
            filter(!is.na(population)) |>
            st_transform(crs = 'ESRI:102009'),
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
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(1, "in"),
        axis.text = element_text(size = 6))
ct_hex_plt

ggsave(filename = "img/extra_figures/fig1c_new.png",
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
              guides = "collect")&
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 6))
hexpop_zoom

ggsave(plot = hexpop_zoom, 
       filename = "img/extra_figures/fig1_upper_new.png",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hexpop_zoom, 
       filename = "img/extra_figures/fig1_upper_new.pdf",
       width = 16,
       height = 9,
       dpi = 100)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

hexes <- hexes |>
  filter(as.integer(hexid) < 7662) |>
  left_join(hexes_to_county |>
              select(hexid, fips) |>
              mutate(hexid = as.character(hexid))) |>
  mutate(STATEFP = str_sub(fips, 1, 2)) |>
  st_transform(crs = 'ESRI:102009')

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron_meta30m.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

## New hexgrid with Meta 30m population, loading again hexpop to have the original 7660 hexes
hexpop <- st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662) |> ## Filtering out Puerto Rico hexes
  st_transform(crs = 26915) |>
  rename(population = metapop_30m)

hexgrid_preomicron_cum <- hexgrid_preomicron |>
  st_drop_geometry() |> 
  mutate(hexid = as.character(hexid)) |>
  group_by(hexid) |> 
  summarise(cum_infections = sum(infections)) |> 
  left_join(hexpop|> 
              st_drop_geometry() |>
              mutate(hexid = as.character(hexid)) |> 
              select(hexid, population)) |> 
  mutate(cum_infectionsPC = (cum_infections/population)*1e5) |>
  # filter(cum_infectionsPC >= 1)|>
  right_join(hexes) |> 
  sf::st_as_sf() |> 
  st_transform(crs = 'ESRI:102009')

# breaks_plt <- seq(1,1001, 100)
# labels_plt <- c("1>", seq(100,900, 100), '1,000+')
# limits_plt <- c(0,1000)
color_option <- "Inferno"

### Pre-Omicron
us_hex_infections <- ggplot() + 
  geom_sf(hexgrid_preomicron_cum |> 
            # filter(!is.na(cum_infections))|> 
            st_transform(crs = 'ESRI:102009'), 
          mapping=aes(fill = log10(cum_infections+1))) +
  geom_sf(us_states,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                               na.value = "grey60",
                                               rev = T,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = color_option)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))
us_hex_infections

ggsave(filename = "img/extra_figures/fig2e.png", 
       plot = us_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

## New England zooming
new_england_grid_infections <- hexgrid_preomicron_cum |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hexes <- hexes |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_infections <- ggplot() + 
  geom_sf(new_england_grid_infections, 
          mapping=aes(fill = log10(cum_infections+1))) +
  geom_sf(new_england_states,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                               na.value = "grey60",
                                               rev = T,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = color_option)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))
new_england_hex_infections

ct_grid_infection <- hexgrid_preomicron_cum |> 
  filter(STATEFP == "09")

ct_hexes <- hexes |> 
  filter(STATEFP == "09")

ct_hex_infections <-  ggplot() + 
  geom_sf(data = ct_grid_infection |> 
            filter(!is.na(cum_infections)), 
          mapping=aes(fill = log10(cum_infections+1))) +
  geom_sf(data = ct_counties,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                               na.value = "grey60",
                                               rev = T,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = color_option)+
  theme_minimal()+
  theme(legend.position = "bottom", 
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
              guides = 'collect')&
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 6))
hex_infections

ggsave(plot = hex_infections,
       filename = "img/extra_figures/fig1_lower_new.pdf",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hex_infections,
       filename = "img/extra_figures/fig1_lower_new.png",
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
       file = "img/fig1_new.pdf",
       width = 9,
       height = 16,
       dpi = 300
)

## hexgrids
dataset <- "meta30m_run_preomicron_daily"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv"))

## Pre-Omicron
CAR_df_preomicron <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  select(hexid, date, population, infections, infectionsPC, mean, sd, geometry) |>
  st_as_sf() |> 
  st_transform(crs = 'ESRI:102009')

## Fig.2 Spatial hexes, population and infections

fig2a_data <- CAR_df_preomicron |> 
  st_drop_geometry() |> 
  mutate(date = as.Date(date)) |> 
  reframe(infections = sum(infections, na.rm = T),
          infectionsPC = sum(infectionsPC, na.rm = T),
          .by = "date") |> 
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

alpha_peak_week <- as.Date("2020-11-28")
delta_peak_week <- as.Date("2021-09-04")

fig2a <- ggplot()+
  geom_line(data = fig2a_data,
            aes(x = date, 
                y = infectionsPC))+
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
           y = rep(4e6, 4),
           label = LETTERS[2:5],
           size = 12)+
  annotate("segment", 
           y = c(1.85e6,2.1e6,2.75e6,3.2e6),
           yend = rep(4e6,4),
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
           xmin = delta_peak_week - 70,
           xmax = delta_peak_week + 7,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(delta_peak-63,
                 delta_peak-45, 
                 delta_peak-24, 
                 delta_peak),
           y = rep(0, 4),
           label = LETTERS[6:9],
           size = 12)+
  annotate("segment", 
           y = rep(1e4,4),
           yend = c(1.5e6,2e6,2.75e6,3.25e6),
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

ggsave(filename = "img/extra_figures/fig2a_new.png",
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
na_color <- "grey70"

fig2.a <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak_week-63)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (alpha_peak_week-63))
fig2.a

ggsave(plot = fig2.a,
       filename = "img/extra_figures/fig2_a_new.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.b <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak_week-42)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (alpha_peak_week-42))
fig2.b

ggsave(plot = fig2.b,
       filename = "img/extra_figures/fig2_b.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.c <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak_week-21)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (alpha_peak_week-21))
fig2.c

ggsave(plot = fig2.c,
       filename = "img/extra_figures/fig2_c_new.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.d <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak_week)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (alpha_peak_week))
fig2.d

ggsave(plot = fig2.d,
       filename = "img/extra_figures/fig2_d.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.e <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak_week-63)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (delta_peak_week-63))
fig2.e

ggsave(plot = fig2.e,
       filename = "img/extra_figures/fig2_e.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.f <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak_week-35)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (delta_peak_week-42))
fig2.f

ggsave(plot = fig2.f,
       filename = "img/extra_figures/fig2_f.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.g <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak_week-21)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
  labs(title = (delta_peak_week-21))
fig2.g

ggsave(plot = fig2.g,
       filename = "img/extra_figures/fig2_g.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2.h <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak_week)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
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
        legend.title.position = "t",
        legend.key.width = grid::unit(1.6, "cm"))+
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = (delta_peak_week))
fig2.h

ggsave(plot = fig2.h,
       filename = "img/extra_figures/fig2_h.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig2 <- (((fig2.a | fig2.b | fig2.c | fig2.d)) / 
           (fig2a) / 
           ((fig2.e | fig2.f | fig2.g | fig2.h))
         )+
  plot_layout(guides = "collect")&
  # plot_annotation(tag_levels = 'A')&
  guides(fill = guide_bins(title = "Estimated Infections/100k/week",
                           title.position = "left",
                           title.hjust = 0.5),
         color = "none")&
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.title.position = "left",
        legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5),
        legend.key.height = grid::unit(1.6, "cm"),
        legend.key.width = grid::unit(1, "cm"))
fig2

ggsave(plot = fig2,
       filename = "img/fig2_new.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = fig2,
       filename = "img/fig2_new.pdf",
       width = 16, 
       height = 9,
       dpi = 300)

## Sensitivity Analysis
figS2 <- ggplot(data = CAR_df_preomicron |> 
                  filter(date %in% c(delta_peak_week, alpha_peak_week,
                                     delta_peak_week-21, alpha_peak_week - 21,
                                     delta_peak_week-35, alpha_peak_week - 35,
                                     delta_peak_week-63, alpha_peak_week - 63)) |> 
                  st_transform(crs=26915),
                aes(fill = log(mean+1), 
                    color = log(mean+1)))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "black",
          fill = "transparent")+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k",
                       direction = -1,
                       # na.value = na_color,
                       breaks = breaks_plt,
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
       filename = "img/extra_figures/figS2.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = figS2,
       filename = "img/extra_figures/figS2.pdf",
       width = 16, 
       height = 9,
       dpi = 300)

## Breakdowns of each peaks
breaks_plt <- c(0,seq(85,300, 20))
labels_plt <- c("85< ",seq(85,280, 20), ' >300')
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
       filename = "img/extra_figures/figS3.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = figS3,
       filename = "img/extra_figures/figS3.pdf",
       width = 16, 
       height = 9,
       dpi = 300)

## Sensitivity Analys on threshold values for the risk surface
## Fig2A - Layered depiction on transforming estimated infections on counties polygon on hexgrid

## Figure S4 correlation between trend of 'alpha', 'delta'
library(units)

## hexgrids
dataset <- "meta30m_run_preomicron_daily"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv"))
## peak date
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

figS2a <- ggplot(data = CAR_df_preomicron,
                 aes(x = mean))+
  geom_histogram(bins = 100)+
  theme_minimal()+
  xlim(c(0,300))+
  labs(x = expression("BYM2 Random effects " *theta ~ "(Ai)"),
       y = "Frequency")+
  scale_y_continuous(labels = scales::label_comma())
figS2a

ecdf_mean <- ecdf(CAR_df_preomicron$mean)
# ecdf_upper <- ecdf(CAR_df_preomicron$`0.975quant`)
# ecdf_lower <- ecdf(CAR_df_preomicron$`0.025quant`)
# ecdf_median <- ecdf(CAR_df_preomicron$`0.5quant`)

figS2b <- ggplot(data = CAR_df_preomicron,
                 aes(x = mean, y = ecdf_mean(mean)))+
  geom_line()+
  # geom_ribbon(aes(x = mean, 
  #                 ymin = ecdf_lower(`0.025quant`), 
  #                 ymax = ecdf_upper(`0.975quant`)))+
  geom_vline(xintercept = 175, linetype = "dashed")+
  theme_minimal()+
  labs(x = expression("BYM2 Random effects " *theta ~ "(Ai)"),
       y = "Percentile")+
  xlim(c(0, 350))
figS2b

library(patchwork)

figS2 <- (figS2a | figS2b)
figS2

ggsave(filename = "img/extra_figures/figS1.png",
       plot = figS2,
       width = 16, 
       height = 9, 
       dpi = 100)

## Threshold for filtering given the distribution, any value of trend that it is above the 3rd Quarter of the trend distribution
threshold_mean <- 165

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
  st_transform(crs = 26915)

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
  st_transform(crs = 26915)

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
  mutate(days_to_peak = (alpha_peak - date))|> 
  st_transform(crs = 26915) |> 
  group_by(days_to_peak) |> 
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
  st_transform(crs = 26915) |> 
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
  st_transform(crs = 26915) |> 
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
  st_transform(crs = 26915) |> 
  group_by(days) |> 
  summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  mutate(wave = "2nd wave")

CAR_area_delta$velocity <- units::set_units(c(0, 
                                              -diff(as.numeric(CAR_velocity_delta$area))), km^2/day)

CAR_velocity_delta$velocity <- units::set_units(c(0, 
                                                  -diff(as.numeric(CAR_velocity_delta$area))), km^2/day)

## Speed distribution
# fig4c_85 <- fig4c

fig4c <- ggplot()+
  geom_col(data = rbind(CAR_area_alpha, CAR_area_delta) |> 
             filter(days >= 7),
           aes(x = days, y = speed, fill = wave),
           alpha = 0.75,
           position = position_dodge())+
  geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
  geom_vline(xintercept = seq(7,63,7), lty = "dotted", color = "grey50")+
  theme_minimal()+
  labs(x = "Days before national curve peak \n [days]", 
       y = "Speed of expansion")+
  scale_x_reverse(breaks = seq(0,63,7))+
  units::scale_y_units(labels = scales::label_comma(),
                       breaks = scales::breaks_extended(n = 10))+
  colorspace::scale_fill_discrete_divergingx(name = "")+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 12))
fig4c

ggsave(filename = "img/extra_figures/fig4.png",
       plot = fig4c,
       width = 16,
       height = 9,
       dpi = 200)

ggsave(filename = "img/extra_figures/fig4.pdf",
       plot = fig4c,
       width = 16,
       height = 9,
       dpi = 200)

figS3 <- (fig4c_85 | fig4c_200)+
  plot_layout(guides = "collect")+
  plot_annotation(tag_levels = 'A')&
  theme(legend.position = "bottom")
figS3

ggsave(filename = "img/extra_figures/figS3.png",
       plot = figS3,
       width = 16,
       height = 9,
       dpi = 100)

## Final Layered figure
fig4_layered <- ((fig4a_layered / fig4b_layered)| fig4c)+
  # plot_layout(guides = "keep")&
  theme(legend.position = "bottom")
fig4_layered

ggsave(filename = "img/extra_figures/fig4_layered.png",
       plot = fig4_layered,
       width = 16, 
       height = 9, 
       dpi = 100)

ggsave(filename = "img/extra_figures/fig4_layered.pdf",
       plot = fig4_layered,
       width = 16, 
       height = 9, 
       dpi = 300)
