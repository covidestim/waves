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

cbgpop <- sf::st_read("data-products/geo-hexes/cbg_population.shp")

hexpop <- sf::st_read("data-products/geo-hexes/hexid-population.shp")

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()

# ## Highways
# highways <- tigris::primary_roads(year = 2024) |> 
#   filter(RTTYP == "I",
#          !grepl("H", FULLNAME)) |> 
#   st_transform(crs = 26915)

new_england_states <- us_states |> 
  dplyr::filter(GEOID %in% c("09","23","25","33","44","50"))

new_england_counties <- tigris::counties(state = c("09","23","25","33","44","50"))

ct_counties <- tigris::counties(state = 09)

# cbgpop <- sf::st_transform(cbgpop, crs = 26915)
us_states <- sf::st_transform(us_states, crs = 26915)

## Hexgrid plots
# high <- "deeppink1"
# low <- "thistle1"
palette <- "Purple-Yellow"

us_hex_plt <- ggplot() + 
  geom_sf(hexpop |> 
            filter(!is.na(population)), 
          mapping=aes(fill = log10(population+1),
                      color = "")) +
  geom_sf(us_states,
          mapping=aes(),
          color = "deeppink4",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population (log10 scale)",
                                               na.value = "transparent",
                                               # rev = F,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = palette)+
  scale_colour_manual(values=NA) +              
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.key.width = grid::unit(3, "cm"),
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6))+
  guides(colour="none")
us_hex_plt

ggsave(plot = us_hex_plt,
       filename = "img/extra_figures/fig2a.png",
       width = 16,
       height = 9,
       dpi = 100)

hexes_to_county <- vroom::vroom("data-products/geo-hexes/hexid-fips-map.csv")

hexpop <- hexpop |> 
  left_join(hexes_to_county |> select(hexid, fips)) |> 
  mutate(STATEFP = str_sub(fips, 1, 2))

new_england_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_plt <- ggplot()+
  geom_sf(new_england_hexpop |> 
            filter(!is.na(population)),
          mapping=aes(fill= log10(population+1),
                      color = ""))+ 
  geom_sf(new_england_states,
          mapping=aes(),
          color = "deeppink4",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population (log10 scale)",
                                               na.value = "transparent",
                                               # rev = F,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = palette)+
  scale_colour_manual(values=NA) +              
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.key.width = grid::unit(3, "cm"),
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6))+
  guides(colour="none")
new_england_hex_plt

ggsave(plot = new_england_hex_plt,
       filename = "img/extra_figures/fig2b.png",
       width = 16, 
       height = 9, 
       dpi = 100)

ct_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09"))

ct_hex_plt <- ggplot()+
  geom_sf(ct_hexpop |> 
            filter(!is.na(population)),
          mapping=aes(fill = log10(population+1),
                      color = ""))+ 
  geom_sf(ct_counties,
          mapping=aes(),
          color = "deeppink4",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Population (log10 scale)",
                                               na.value = "transparent",
                                               # rev = F,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = palette)+
  scale_colour_manual(values=NA) +              
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.key.width = grid::unit(3, "cm"),
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        axis.text = element_text(size = 6))+
  guides(colour="none")
ct_hex_plt

ggsave(filename = "img/extra_figures/fig2c.png",
       plot = ct_hex_plt,
       width = 16,
       height = 9, 
       dpi = 100)

library(patchwork)
# p1 <- (new_england_hex_plt / ct_hex_plt)+
#   plot_layout(widths = c(1,1),
#               guides = "collect",
#               heights = c(1,1))&
#   theme(legend.position = "none")
# p1
# 
# ggsave(filename = "img/extra_figures/fig2c.png",
#        plot = p1, 
#        width = 16, 
#        height = 9,
#        dpi = 100)
# 
# ggsave(filename = "img/extra_figures/fig2c.pdf",
#        plot = p1, 
#        width = 16, 
#        height = 9,
#        dpi = 100)

hexpop_zoom <- (us_hex_plt | (new_england_hex_plt / ct_hex_plt))+
  plot_layout(widths = c(4,1,1),
              heights = c(4,1,1))+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 6))
hexpop_zoom

ggsave(plot = hexpop_zoom, 
       filename = "img/extra_figures/fig2b.png",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hexpop_zoom, 
       filename = "img/extra_figures/fig2b.pdf",
       width = 16,
       height = 9,
       dpi = 100)

## Infections on Hexes
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron.csv")

hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

hexes <- hexes |>
  filter(as.integer(hexid) < 7662) |>
  left_join(hexes_to_county |>
              select(hexid, fips) |>
              mutate(hexid = as.character(hexid))) |>
  mutate(STATEFP = str_sub(fips, 1, 2)) |>
  st_transform(crs = 'ESRI:102009')

hexpop <- sf::st_read("data-products/geo-hexes/hexid-population.shp")

hexgrid_preomicron_cum <- hexgrid_preomicron |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  # ungroup() |> 
  # select(-geometry, -population, -infectionsPC) |>
  group_by(hexid) |> 
  summarise(cum_infections = sum(infections, na.rm = T)) |> 
  left_join(hexpop|> 
              # st_drop_geometry() |> 
              mutate(hexid = as.character(hexid))) |> 
  mutate(cum_infectionsPC = (cum_infections/population)*1e5) |>
  filter(cum_infectionsPC >= 1) |>
  right_join(hexes |> st_drop_geometry()) |>
  sf::st_as_sf() |> 
  st_transform(crs = 'ESRI:102009')

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

# breaks_plt <- seq(1,1001, 100)
# labels_plt <- c("1>", seq(100,900, 100), '1,000+')
# limits_plt <- c(0,1000)
color_option <- "Inferno"

### Pre-Omicron
us_hex_infections <- ggplot() + 
  geom_sf(data = hexes|>
            filter(as.integer(hexid) < 7662) |>
            st_transform(crs = 'ESRI:102009'),
          fill = 'transparent',
          aes(color = ""))+
  geom_sf(hexgrid_preomicron_cum |> 
            filter(!is.na(cum_infections))|> 
            st_transform(crs = 'ESRI:102009'), 
          mapping=aes(fill = log10(cum_infections+1),
                      color = "")) +
  geom_sf(us_states,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  # geom_sf(highways,
  #         mapping = aes(),
  #         color = "thistle3")+
  colorspace::scale_fill_continuous_sequential(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                               na.value = "transparent",
                                               rev = F,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = color_option)+
  scale_color_manual(values = NA)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))+
  guides(colour="none")
us_hex_infections

ggsave(filename = "img/extra_figures/fig2e.png", 
       plot = us_hex_infections,
       width = 16,
       height = 9, 
       dpi = 100)

new_england_grid_infections <- hexgrid_preomicron_cum |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hexes <- hexes |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_infections <- ggplot() + 
  geom_sf(data = new_england_hexes,
          fill = 'transparent',
          aes(color = ""))+
  geom_sf(new_england_grid_infections |> 
            filter(!is.na(cum_infections)), 
          mapping=aes(fill = log10(cum_infections+1), 
                      color = "")) +
  geom_sf(new_england_states,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                               na.value = "transparent",
                                               rev = F,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = color_option)+
  scale_color_manual(values = NA)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))+
  guides(colour="none")
new_england_hex_infections

ct_grid_infection <- hexgrid_preomicron_cum |> 
  filter(STATEFP == "09")

ct_hexes <- hexes |> 
  filter(STATEFP == "09")

ct_hex_infections <-  ggplot() + 
  geom_sf(data = ct_hexes,
          fill = "transparent",
          aes(color = ""))+
  geom_sf(data = ct_grid_infection |> 
            filter(!is.na(cum_infections)), 
          mapping=aes(fill = log10(cum_infections+1), 
                      color = "")) +
  geom_sf(data = ct_counties,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
  colorspace::scale_fill_continuous_sequential(name = "Cumulative Infections (log10 scale) \n (March 2020 - December 2021)",
                                               na.value = "transparent",
                                               rev = F,
                                               breaks = seq(1,7,1),
                                               labels = scales::label_math(),
                                               limits = c(1,7),
                                               palette = color_option)+
  scale_color_manual(values = NA)+
  theme_minimal()+
  theme(legend.position = "bottom", 
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5),
        legend.key.width = grid::unit(3, "cm"),
        axis.text = element_text(size = 6))+
  guides(colour="none")
ct_hex_infections

library(patchwork)
hex_infections <- (us_hex_infections | (new_england_hex_infections / ct_hex_infections))+
  plot_layout(widths = c(4,1,1),
              heights = c(4,1,1))+
  plot_layout(guides = 'collect')&
  theme(legend.position = "bottom", 
        axis.text = element_text(size = 6))
hex_infections

ggsave(plot = hex_infections,
       filename = "img/extra_figures/fig2c.pdf",
       width = 16,
       height = 9,
       dpi = 100)

ggsave(plot = hex_infections,
       filename = "img/extra_figures/fig2c.png",
       width = 16,
       height = 9,
       dpi = 100)

## Patchwork all together
fig2 <- (hexpop_zoom / hex_infections)+
  # plot_annotation(tag_levels = 'A')+
  plot_layout(widths = c(3,1))&
  theme(plot.tag = element_text(size = 18),
        axis.text = element_text(size = 4))
fig2

ggsave(plot = fig2,
       file = "img/fig2.pdf",
       width = 9,
       height = 16,
       dpi = 100
)

## hexgrids
dataset <- "preomicron"

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
  st_transform(crs = 26915)

# ## Breakdowns of each peaks
# breaks_plt <- seq(0,1500, 150)
# labels_plt <- c(seq(0,900, 150), "1,050", "1,200", "1,350",'1,500+')
# limits_plt <- c(0,1700)
# color_option <- "magma"
# na_color <- "grey70"
# 
# ## Alpha wave snapshots
# fig2.c <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (alpha_peak - 63)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   labs(title = (alpha_peak-63))
# fig2.c
# 
# ggsave(plot = fig2.c,
#        filename = "img/extra_figures/fig2c.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2.d <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (alpha_peak - 42)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (alpha_peak-42))
# fig2.d
# 
# ggsave(plot = fig2.d,
#        filename = "img/extra_figures/fig2d.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2.e <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (alpha_peak - 21)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (alpha_peak-21))
# fig2.e
# 
# ggsave(plot = fig2.e,
#        filename = "img/extra_figures/fig2e.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2.f <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (alpha_peak)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (alpha_peak))
# fig2.f
# 
# ggsave(plot = fig2.f,
#        filename = "img/extra_figures/fig2f.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# ## Delta wave snapshots
# fig2.g <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (delta_peak - 63)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (delta_peak-63))
# fig2.g
# 
# ggsave(plot = fig2.g,
#        filename = "img/extra_figures/fig2g.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2.h <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (delta_peak - 42)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (delta_peak-42))
# fig2.h
# 
# ggsave(plot = fig2.h,
#        filename = "img/extra_figures/fig2h.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2.i <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (delta_peak - 21)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC,
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (delta_peak-21))
# fig2.i
# 
# ggsave(plot = fig2.i,
#        filename = "img/extra_figures/fig2i.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# fig2.j <- ggplot(data = CAR_df_preomicron |> 
#                    filter(date == (delta_peak)) |> 
#                    st_transform(crs=26915),
#                  aes(fill = infectionsPC, 
#                      color = infectionsPC))+
#   geom_sf()+
#   geom_sf(data = us_states,
#           color = "deeppink4",
#           fill = "transparent")+
#   scale_fill_viridis_b(option = color_option,
#                        # name = "Estimated Infections/1000/week",
#                        direction = -1,
#                        na.value = na_color,
#                        breaks = breaks_plt,
#                        labels = labels_plt,
#                        limits = limits_plt,
#   )+
#   scale_color_viridis_b(option = color_option,
#                         # name = "Estimated Infections/1000/week",
#                         direction = -1,
#                         na.value = na_color,
#                         breaks = breaks_plt,
#                         labels = labels_plt,
#                         limits = limits_plt,
#   )+
#   theme_void()+
#   guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
#                            title.position = "top",
#                            title.hjust = 0.5),
#          color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   labs(title = (delta_peak))
# fig2.j
# 
# ggsave(plot = fig2.j,
#        filename = "img/extra_figures/fig2j.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 100)
# 
# ## Fig.2 patchwork
# library(patchwork)
# 
# fig2 <- ((fig2.c / fig2.d / fig2.e / fig2.f) | (fig2.a.flip) | (fig2.g / fig2.h / fig2.i / fig2.j))+
#   plot_layout(guides = "collect")&
#   theme(legend.position = "bottom")
# fig2
# 
# ggsave(plot = fig2,
#        filename = "img/extra_figures/fig2_vertical.pdf",
#        width = 16,
#        height = 9, 
#        dpi = 300)

## Figure 3, TSA
## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,300, 20))
labels_plt <- c("150< ",seq(150,280, 20), ' >300')
limits_plt <- c(0,350)
color_option <- "magma"
na_color <- "grey70"

fig3.a <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak-63)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak-63))
fig3.a

ggsave(plot = fig3.a,
       filename = "img/extra_figures/fig3_a.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.b <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak-42)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak-42))
fig3.b

ggsave(plot = fig3.b,
       filename = "img/extra_figures/fig3_b.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.c <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak-21)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak-21))
fig3.c

ggsave(plot = fig3.c,
       filename = "img/extra_figures/fig3_c.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.d <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak))
fig3.d

ggsave(plot = fig3.d,
       filename = "img/extra_figures/fig3_d.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.e <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak-63)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak-63))
fig3.e

ggsave(plot = fig3.e,
       filename = "img/extra_figures/fig3_e.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.f <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak-42)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak-42))
fig3.f

ggsave(plot = fig3.f,
       filename = "img/extra_figures/fig3_f.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.g <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak-21)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak-21))
fig3.g

ggsave(plot = fig3.g,
       filename = "img/extra_figures/fig3_g.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.h <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak)) |> 
                   st_transform(crs=26915),
                 aes(fill = mean, 
                     color = mean))+
  geom_sf()+
  geom_sf(data = us_states,
          color = "deeppink4",
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
  guides(fill = guide_bins(title = "Trend Surface of infections estimated/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak))
fig3.h

ggsave(plot = fig3.h,
       filename = "img/extra_figures/fig3_h.png",
       width = 16,
       height = 9, 
       dpi = 200)

fig3.vertical <- ((fig3.a / fig3.b / fig3.c / fig3.d) | (fig2.a.flip) | (fig3.e / fig3.f / fig3.g / fig3.h))+
  plot_layout(guides = "collect")&
  theme(legend.position = "bottom")
fig3.vertical

ggsave(plot = fig3.vertical,
       filename = "img/extra_figures/fig3_vertical.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = fig3.vertical,
       filename = "img/extra_figures/fig3_vertical.pdf",
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
          color = "deeppink4",
          fill = "transparent")+
  scale_fill_viridis_c(option = color_option,
                       name = "Estimated Infections/100k",
                       direction = -1,
                       # na.value = na_color,
                       # breaks = breaks_plt,
                       # labels = labels_plt,
                       # limits = limits_plt,
  )+
  scale_color_viridis_b(option = color_option,
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
breaks_plt <- c(0,seq(85,300, 50))
labels_plt <- c("85< ",seq(85,280, 50), ' >300')
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
          color = "deeppink4",
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
  guides(color = "none")+
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
## Aux functions
rotate_data <- function(data,
                        shear_cos_x = 2,
                        shear_sin_x = 1.2,
                        shear_cos_y = 0,
                        shear_sin_y = 1,
                        x_add = 0, y_add = 0) {
  
  shear_matrix <- function(){ matrix(c(shear_cos_x,
                                       shear_sin_x,
                                       shear_cos_y,
                                       shear_sin_y),
                                     2,
                                     2) }
  
  rotate_matrix <- function(x){
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }
  data %>%
    dplyr::mutate(
      geometry = .$geometry * shear_matrix() +
        c(x_add, y_add)
    )
}

rotate_data_geom <- function(data, x_add = 0, y_add = 0) {
  shear_matrix <- function(){ matrix(c(2, 1.2, 0, 1), 2, 2) }
  
  rotate_matrix <- function(x) {
    matrix(c(cos(x), sin(x), -sin(x), cos(x)), 2, 2)
  }
  data %>%
    dplyr::mutate(
      geom = .$geom * shear_matrix() * rotate_matrix(pi/20) + c(x_add, y_add)
    )
}

# # annotate parameters
x = 0
y = 0
# color = 'gray40'
shear_cos_x = pi/2
shear_sin_x = -1
shear_cos_y = 0
shear_sin_y = pi/2

## Figure S4 correlation between trend of 'alpha', 'delta'
library(units)

## hexgrids
dataset <- "preomicron"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv"))
## peak date
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

figS2a <- ggplot(data = CAR_df_preomicron,
                 aes(x = mean))+
  geom_histogram(bins = 30)+
  theme_minimal()+
  xlim(c(0,300))+
  labs(x = expression("BYM2 Random effects " *theta ~ "(Ai)"),
       y = "Frequency")+
  scale_y_continuous(labels = scales::label_comma())
figS2a

ecdf_mean <- ecdf(CAR_df_preomicron$mean)

figS2b <- ggplot(data = CAR_df_preomicron,
                aes(x = mean, y = ecdf_mean(mean)))+
  geom_line()+
  theme_minimal()+
  labs(x = expression("BYM2 Random effects " *theta ~ "(Ai)"),
       y = "Percentile")+
  # scale_y_continuous(labels = scales::label_percent())+
  xlim(c(0, 300))
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
  filter(date %in% c(alpha_peak, (alpha_peak-7), (alpha_peak-14), (alpha_peak-28), (alpha_peak-56)))|> 
  mutate(contour_surface = factor(case_when(mean >= threshold_mean & date == (alpha_peak-56) ~ 0,
                                            mean >= threshold_mean & date == (alpha_peak-28) ~ 1,
                                            mean >= threshold_mean & date == (alpha_peak-14) ~ 2,
                                            mean >= threshold_mean & date == (alpha_peak-7) ~ 3,
                                            mean >= threshold_mean & date == (alpha_peak) ~ 4),
                                  labels = c("56 days","28 days", "14 days", "7 days", "peak"),
                                  ordered = T))|> 
  st_transform(crs = 26915)

contour_alpha <- CAR_lag_alpha|> 
  filter(!is.na(contour_surface),
         contour_surface != "7 days") |> 
  group_by(contour_surface, date) |> 
  summarise(geometry = st_union(geometry),
            area = format(round(units::set_units(st_area(st_union(geometry)), 
                                          km^2),0), 
                          big.mark = ",")) |> 
  arrange(date)

## Creating the US border for the lower 48 states
us_border <- us_states |> 
  st_union() |> 
  st_as_sf() |> 
  rename(geometry = x)

## Delta
CAR_lag_delta <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% c(delta_peak, (delta_peak-7), (delta_peak-14), (delta_peak-28), (delta_peak-56)))|> 
  mutate(contour_surface = factor(case_when(mean >= threshold_mean & date == (delta_peak-56) ~ 0,
                                            mean >= threshold_mean & date == (delta_peak-28) ~ 1,
                                            mean >= threshold_mean & date == (delta_peak-14) ~ 2,
                                            mean >= threshold_mean & date == (delta_peak-7) ~ 3,
                                            mean >= threshold_mean & date == (delta_peak) ~ 4),
                                  labels = c("56 days", "28 days", "7 days", "14 days", "peak"),
                                  ordered = T))|> 
  st_transform(crs = 26915)

## Delta wave layers
contour_delta <- CAR_lag_delta|> 
  filter(!is.na(contour_surface),
         contour_surface != "7 days") |> 
  group_by(contour_surface, date) |> 
  summarise(geometry = st_union(geometry),
            area = format(round(untis::set_units(st_area(st_union(geometry)), km^2), 0), 
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
  st_transform(crs = 26915) |> 
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
