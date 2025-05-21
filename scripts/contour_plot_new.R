rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(units)

hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)

hexes_centroids <- as.data.frame(st_coordinates(st_cast(st_centroid(hexes), "MULTIPOINT"))) |> 
  rename(hexid = L1) |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  mutate(geometry = st_centroid(geometry)) |> 
  st_as_sf()

## US States border lines
us_states <- tigris::states(cb = T, resolution = "500k") |> 
  st_transform(crs = 26915) |> 
  filter(!NAME %in% c("Alaska",
                      "Hawaii",
                      "Puerto Rico", 
                      "Guam", 
                      "Commonwealth of the Northern Mariana Islands",
                      "American Samoa",
                      "United States Virgin Islands"))

## hexgrids
dataset <- "meta30m_run_preomicron_daily"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv"))

## Hexes
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)

## US States
## US States border lines
us_states <- tigris::states(cb = T, resolution = "500k") |> 
  st_transform(crs = 26915) |> 
  filter(!NAME %in% c("Alaska",
                      "Hawaii",
                      "Puerto Rico", 
                      "Guam", 
                      "Commonwealth of the Northern Mariana Islands",
                      "American Samoa",
                      "United States Virgin Islands"))

## peak date
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

alpha_peak_week <- as.Date("2020-11-28")
delta_peak_week <- as.Date("2021-09-04")

## Threshold for filtering given the distribution, any value of trend that it is above the 3rd Quarter of the trend distribution
threshold_mean <- 165

## Pre-Omicron

## Alpha
CAR_lag_alpha <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(to = alpha_peak,
                            from = (alpha_peak-63),
                            length.out = 63))|>
  st_transform(crs = 26915)

contour_alpha <- CAR_lag_alpha|> 
  mutate(days = as.numeric(case_when(mean >= threshold_mean ~ (alpha_peak - date)))) |>
  filter(!is.na(days)) |> 
  group_by(days, date) |> 
  summarise(geometry = st_union(geometry),
            infections_pc = sum(mean, na.rm = T),
            area = format(round(set_units(st_area(st_union(geometry)), 
                                          km^2),0)))|> 
  mutate(area_format = format(area, 
                              big.mark = ","),
         area_numeric = drop_units(round(set_units(st_area(st_union(geometry)), 
                                                   km^2),0))) |> 
  mutate(wave = "1st Wave")

## Speed calculation
contour_alpha$diffusion <- units::set_units(c(0, 
                                              -diff(contour_alpha$area_numeric)), 'km^2/day')

contour_alpha$diffusion_numeric <- drop_units(contour_alpha$diffusion)

contour_alpha$speed_numeric <- sqrt(abs(contour_alpha$diffusion_numeric))
contour_alpha$speed <- units::set_units(contour_alpha$speed_numeric, 'km/day')

## delta
CAR_lag_delta <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(to = delta_peak,
                            from = (delta_peak-63),
                            length.out = 63))|>
  st_transform(crs = 26915)

contour_delta <- CAR_lag_delta|> 
  mutate(days = as.numeric(case_when(mean >= threshold_mean ~ (delta_peak - date)))) |>
  filter(!is.na(days)) |> 
  group_by(days, date) |> 
  summarise(geometry = st_union(geometry),
            infections_pc = sum(mean, na.rm = T),
            area = format(round(set_units(st_area(st_union(geometry)), 
                                          km^2),0)))|> 
  mutate(area_format = format(area, 
                              big.mark = ","),
         area_numeric = drop_units(round(set_units(st_area(st_union(geometry)), 
                                                   km^2),0))) |> 
  mutate(wave = "2nd Wave")

## Speed calculation
contour_delta$diffusion <- units::set_units(c(0, 
                                              -diff(contour_delta$area_numeric)), 'km^2/day')

contour_delta$diffusion_numeric <- drop_units(contour_delta$diffusion)

contour_delta$speed_numeric <- sqrt(abs(contour_delta$diffusion_numeric))
contour_delta$speed <- units::set_units(contour_delta$speed_numeric, 'km/day')

## Speed distribution
fig3a <- ggplot()+
  geom_col(data = rbind(contour_alpha, contour_delta),
           aes(x = days, y = speed, fill = wave),
           alpha = 0.65,
           position = position_dodge())+
  geom_vline(xintercept = c(63, 56, 49, 42, 35, 28, 21, 14, 7), 
             lty = "dotted", 
             color = "grey50")+
  theme_minimal()+
  labs(x = "Days before peak", 
       y = "Speed of expansion")+
  scale_x_reverse(breaks = seq(7,63,7),
                  labels = seq(7,63,7),
                  limits = c(63,7))+
  # scale_y_continuous(name = "Speed of expansion \n [sq. km/day]",
  #                    labels = scales::label_comma(),
  #                    breaks = scales::breaks_extended(n=10),
  #                    # sec.axis = sec_axis(~./scaleFactor, name="Speed of centroid \n [km/day]", 
  #                    #                     breaks = scales::breaks_extended(n=5),
  #                    #                     labels = scales::label_comma())
  #                    )+
  units::scale_y_units(labels = scales::label_comma(),
                       breaks = scales::breaks_extended(n=7))+
  colorspace::scale_fill_discrete_divergingx()+
  colorspace::scale_color_discrete_divergingx()+
  theme(legend.position = "none",
        legend.background = element_rect(),
        legend.title = element_blank())+
  facet_wrap(.~wave, ncol = 1,strip.position = "right")
fig3a

ggsave(filename = "img/extra_figures/fig3a_new.png",
       plot = fig3a,
       width = 16, 
       height = 9, 
       dpi = 300)

# library(metR)
# ggplot(contour_alpha, 
#        aes(lon, lat, z = air.z)) +
#   geom_contour_fill() +
#   geom_contour2(color = "black") +
#   geom_text_contour(stroke = 0.2) +
#   scale_fill_divergent() 

fig3b <- ggplot()+
  ## peak layers
  geom_sf(data = contour_alpha,
          aes(fill = speed_numeric),
          alpha = 0.70)+
  # geom_sf(data = us_states,
  #         fill = "transparent",
  #         color = "deeppink4")+
  scico::scale_fill_scico(breaks = seq(0,1100, 50),
                          palette = "lipari",
                          # direction = -1,
                          limits = c(0,1100),
                          labels = scales::label_comma(),
                          name = "Speed of expansion [km/day]")+
  # khroma::scale_fill_buda(breaks = seq(0,1100, 50),
  #                                  reverse = T,
  #                                  limits = c(0,1100),
  #                                  labels = scales::label_comma(),
  #                                  name = "Speed of expansion [km/day]")+
  # khroma::scale_color_batlow(breaks = seq(0,1100, 50),
  #                                   # reverse = T,
  #                                   limits = c(0,1100),
  #                                   labels = scales::label_comma(),
  #                            name = "Speed of expansion [km/day]")+
  theme_void()+
  theme(legend.position = "right",
        legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5), 
        legend.title.position = "left",
        # legend.text = element_text(),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, "cm"))+
  guides(fill = guide_legend(reverse = F, ncol = 1),
         color = guide_legend(reverse = F, ncol = 1))+
  labs(title = "1st Wave")
fig3b

ggsave(filename = "img/extra_figures/fig3b_new.png",
       plot = fig3b,
       width = 16, 
       height = 9, 
       dpi = 300)

fig3c <- ggplot()+
  ## peak layers
  geom_sf(data = contour_delta,
          aes(fill = speed_numeric),
          alpha = 0.70)+
  # geom_sf(data = us_states,
  #         fill = "transparent",
  #         color = "deeppink4")+
  scico::scale_fill_scico(breaks = seq(0,1100, 50),
                          palette = "lipari",
                          # direction = -1,
                          limits = c(0,1100),
                          labels = scales::label_comma(),
                          name = "Speed of expansion [km/day]")+
  # khroma::scale_fill_batlowK(breaks = seq(0,1100, 50),
  #                                  # reverse = T,
  #                                  limits = c(0,1100),
  #                                  labels = scales::label_comma(),
  #                           name = "Speed of expansion [km/day]")+
  # khroma::scale_color_batlow(breaks = seq(0,1100, 50),
  #                                   # reverse = T,
  #                                   limits = c(0,1100),
  #                                   labels = scales::label_comma(),
  #                            name = "Speed of expansion [km/day]")+
  theme_void()+
  theme(legend.position = "right",
        legend.title = element_text(angle = 90, vjust = 0.5, hjust = 0.5), 
        legend.title.position = "left",
        # legend.text = element_text(),
        legend.direction = "vertical",
        legend.key.height = unit(0.5, "cm"))+
  guides(fill = guide_legend(reverse = F, ncol = 1),
         color = guide_legend(reverse = F, ncol = 1))+
  labs(title = "2nd Wave")
fig3c

ggsave(filename = "img/extra_figures/fig3c_new.png",
       plot = fig3c,
       width = 16, 
       height = 9, 
       dpi = 300)

library(patchwork)

fig4 <- (fig3b / fig3c)+
  plot_layout(guides = "collect")
fig4

ggsave(filename = "img/extra_figures/fig4_new.png",
       plot = fig4,
       width = 9, 
       height = 16,
       dpi = 200)

ggsave(filename = "img/extra_figures/fig4_new.pdf",
       plot = fig4,
       width = 9, 
       height = 16,
       dpi = 200)

# ## Speed distribution
# CAR_area_alpha <- CAR_df_preomicron |> 
#   mutate(hexid = as.character(hexid)) |> 
#   left_join(hexes) |> 
#   dplyr::select(hexid, date, mean, sd, geometry) |>
#   st_as_sf() |> 
#   filter(date %in% seq.Date(alpha_peak, (alpha_peak-63), length.out = 63))|> 
#   filter(mean >= threshold_mean) |> 
#   mutate(days = (alpha_peak - date))|> 
#   st_transform(crs = 26915) |> 
#   group_by(days) |> 
#   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
#   mutate(wave = "1st wave")
# 
# ## Speed and Velocity calculation
# ## Alpha
# CAR_area_alpha$speed <- units::set_units(c(0, 
#                                            -diff(as.numeric(CAR_area_alpha$area))), km^2/day)
# 
# ## Velocity
# CAR_velocity_alpha <- CAR_df_preomicron |> 
#   mutate(hexid = as.character(hexid)) |> 
#   left_join(hexes) |> 
#   dplyr::select(hexid, date, mean, sd, geometry) |>
#   st_as_sf() |> 
#   filter(date %in% seq.Date(alpha_peak, (alpha_peak-63), length.out = 63))|> 
#   filter(mean >= threshold_mean) |> 
#   mutate(days = (alpha_peak - date))|> 
#   st_transform(crs = 26915) |> 
#   group_by(days) |> 
#   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
#   mutate(geometry = st_centroid(geometry)) |> 
#   mutate(wave = "1st wave")
# 
# CAR_area_alpha$velocity <- units::set_units(c(0, 
#                                               -diff(as.numeric(CAR_velocity_alpha$area))), km^2/day)
# 
# CAR_velocity_alpha$velocity <- units::set_units(c(0, 
#                                                   -diff(as.numeric(CAR_velocity_alpha$area))), km^2/day)
# 
# ## Delta
# CAR_area_delta <- CAR_df_preomicron |> 
#   mutate(hexid = as.character(hexid)) |> 
#   left_join(hexes) |> 
#   dplyr::select(hexid, date, mean, sd, geometry) |>
#   st_as_sf() |> 
#   filter(date %in% seq.Date(delta_peak, (delta_peak-63), length.out = 63))|> 
#   filter(mean >= threshold_mean) |> 
#   mutate(days = (delta_peak - date))|> 
#   st_transform(crs = 26915) |> 
#   group_by(days) |> 
#   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
#   mutate(wave = "2nd wave")
# 
# ## Speed/Velocity calculation
# CAR_area_delta$speed <- units::set_units(c(0, 
#                                            -diff(as.numeric(CAR_area_delta$area))), km^2/day)
# 
# ## Velocity
# CAR_velocity_delta <- CAR_df_preomicron |> 
#   mutate(hexid = as.character(hexid)) |> 
#   left_join(hexes) |> 
#   dplyr::select(hexid, date, mean, sd, geometry) |>
#   st_as_sf() |> 
#   filter(date %in% seq.Date(delta_peak, (delta_peak-63), length.out = 63))|> 
#   filter(mean >= threshold_mean) |> 
#   mutate(days = (delta_peak - date))|> 
#   st_transform(crs = 26915) |> 
#   group_by(days) |> 
#   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
#   mutate(geometry = st_centroid(geometry)) |> 
#   mutate(wave = "2nd wave")
# 
# CAR_area_delta$velocity <- units::set_units(c(0, 
#                                               -diff(as.numeric(CAR_velocity_delta$area))), km^2/day)
# 
# CAR_velocity_delta$velocity <- units::set_units(c(0, 
#                                                   -diff(as.numeric(CAR_velocity_delta$area))), km^2/day)

## Scaling factor for the secondary axis
# scaleFactor <- max(rbind(contour_alpha, contour_delta)$speed_numeric, na.rm = T) / max(rbind(centroid_alpha, centroid_delta)$centroid_speed_numeric, na.rm = T)

library(patchwork)

## Final Layered figure
fig3 <- (fig3c | (fig3a / fig3b))
fig3

ggsave(filename = "img/extra_figures/fig4_layered.png",
       plot = fig3,
       width = 16, 
       height = 9, 
       dpi = 100)

ggsave(filename = "img/extra_figures/fig4_layered.pdf",
       plot = fig3,
       width = 16, 
       height = 9, 
       dpi = 300)

# ## Creating the US border for the lower 48 states
# us_border <- us_states |> 
#   st_union() |> 
#   st_as_sf() |> 
#   rename(geometry = x)

# ## Table Summarizing
# library(gtsummary)

# ## Joined dataframe
# joined_table <- rbind(contour_alpha |>
#                         filter(days %in% seq(0,63, 21)) |> 
#                         st_drop_geometry() |> 
#                         select(date, -days, infections_pc, area_numeric, speed_numeric, wave),
#                       contour_delta|>
#                         filter(days %in% seq(0,63, 21)) |> 
#                         st_drop_geometry() |> 
#                         select(date, infections_pc, area_numeric, speed_numeric, wave)) |> 
#   ungroup() |> 
#   # select(-days) |> 
#   group_by(date, wave)
# 
# tbl_wave1 <- joined_table |> 
#   filter(wave == "1st Wave") |>
#   ## Nasty trick to organize days from 63 days prior to peak day
#   mutate(days = -days) |> 
#   tbl_summary(by = days,
#               include = c(infections_pc, area_numeric, speed_numeric),
#               label = list(infections_pc ~ "Infections per capita \n (infections/100k)",
#                            area_numeric ~ "Area \n (km^2)",
#                            speed_numeric ~ "Speed \n (km^2/day)"),
#               type = list(c(infections_pc, area_numeric, speed_numeric) ~ 'continuous'),
#               statistic = list(c(infections_pc, area_numeric, speed_numeric) ~ "{mean}"),
#               digits = all_continuous() ~ 0) |> 
#   modify_header(label ~ "**Wave 1**",
#                 update = list(stat_1 ~ "**2020-09-17**",
#                               stat_2 ~ "**2020-10-08**",
#                               stat_3 ~ "**2020-10-29**",
#                               stat_4 ~ "**2020-11-19**")) |> 
#   modify_spanning_header(all_stat_cols() ~ "**Date**") |> 
#   modify_footnote(everything() ~ NA)
# tbl_wave1
# 
# tbl_wave2 <- joined_table |> 
#   filter(wave == "2nd Wave") |>
#   ## Nasty trick to organize days from 63 days prior to peak day
#   mutate(days = -days) |> 
#   tbl_summary(by = days,
#               include = c(infections_pc, area_numeric, speed_numeric),
#               label = list(infections_pc ~ "Infections per capita \n (infections/100k)",
#                            area_numeric ~ "Area \n (km^2)",
#                            speed_numeric ~ "Speed \n (km^2/day)"),
#               type = list(c(infections_pc, area_numeric, speed_numeric) ~ 'continuous'),
#               statistic = list(c(infections_pc, area_numeric, speed_numeric) ~ "{mean}"),
#               digits = all_continuous() ~ 0) |> 
#   modify_header(label ~ "**Wave 2**",
#                 update = list(stat_1 ~ "**2021-07-07**",
#                               stat_2 ~ "**2021-07-28**",
#                               stat_3 ~ "**2021-08-18**",
#                               stat_4 ~ "**2021-09-08**")) |> 
#   modify_spanning_header(all_stat_cols() ~ "**Date**") |> 
#   modify_footnote(everything() ~ NA)
# tbl_wave2
# 
# tbl_stack <- tbl_stack(list(tbl_wave1, tbl_wave2), 
#                        quiet = T, 
#                        group_header = c("Wave 1", "Wave 2"))
# tbl_stack
# 
# library(gt)
# joined_table |> 
#   arrange(desc(days)) |> 
#   gt(groupname_col = "wave",
#      rowname_col = "date") |> 
#   tab_header(
#     title = md("**Table.1 - Waves Characteristics**")
#   ) |> 
#   fmt_number(
#     columns = c(days, infections_pc, area_numeric, speed_numeric),
#     decimals = 0
#   ) |> 
#   cols_label(
#     days = md("Days prior to peak"),
#     infections_pc = md("Infections per capita<br>[infections/100k]"),
#     area_numeric = md("Area<br>[km<sup>2</sup>]"),
#     speed_numeric = md("Speed<br>[km<sup>2</sup>/day]")
#   ) |> 
#   summary_rows(columns = -c(days,speed_numeric),
#                fns = list(label = "Total", fn = "sum"),
#                fmt = ~ fmt_number(.x, decimals = 0))

## Centroid movement and speed
centroid_alpha <- contour_alpha |> 
  group_by(days) |> 
  summarise(path = units::set_units(st_area(st_union(geometry)), km^2))|> 
  mutate(geometry = st_centroid(geometry)) |> 
  mutate(lat = st_coordinates(geometry)[,"X"],
         latend = lag(lat),
         lon = st_coordinates(geometry)[,"Y"],
         lonend = lag(lon)) |>
  st_transform(crs = 26915) |> 
  mutate(wave = "1st Wave")

centroid_alpha$distance <- units::set_units(c(0, st_distance(centroid_alpha[-1,],
                                                             centroid_alpha[-nrow(centroid_alpha),],
                                                             by_element=TRUE)), km)

centroid_alpha$centroid_speed <- units::set_units(c(0, 
                                                    abs(-diff(round(centroid_alpha$distance, 
                                                                    0)))), km/day)

centroid_alpha$centroid_speed_numeric <- drop_units(centroid_alpha$centroid_speed)

## Centroid movement and speed
centroid_delta <- contour_delta |> 
  group_by(days) |> 
  summarise(path = units::set_units(st_area(st_union(geometry)), km^2))|> 
  mutate(geometry = st_centroid(geometry)) |> 
  mutate(lat = st_coordinates(geometry)[,"X"],
         latend = lag(lat),
         lon = st_coordinates(geometry)[,"Y"],
         lonend = lag(lon)) |>
  st_transform(crs = 26915) |> 
  mutate(wave = "2nd Wave")

centroid_delta$distance <- units::set_units(c(0, st_distance(centroid_delta[-1,],
                                                             centroid_delta[-nrow(centroid_delta),],
                                                             by_element=TRUE)), km)

centroid_delta$centroid_speed <- units::set_units(c(0, 
                                                    abs(-diff(round(centroid_delta$distance, 
                                                                    0)))), km/day)

centroid_delta$centroid_speed_numeric <- drop_units(centroid_delta$centroid_speed)

breaks_alpha <- seq.Date(from = (alpha_peak-63),
                         to = alpha_peak,
                         by = "3 days")

labels_alpha <- c(as.character(alpha_peak-63), rep(" ", 2),
                  as.character(alpha_peak-56), rep(" ", 2),
                  as.character(alpha_peak-45), rep(" ", 2),
                  as.character(alpha_peak-36), rep(" ", 2),
                  as.character(alpha_peak-27), rep(" ", 2),
                  as.character(alpha_peak-18), rep(" ", 2),
                  as.character(alpha_peak-9), rep(" ", 2),
                  "1st Wave peak: \n 2020-11-19")

breaks_delta <- seq.Date(from = (delta_peak-63),
                         to = delta_peak,
                         by = "3 days")

labels_delta <- c(as.character(delta_peak-63), rep(" ", 2),
                  as.character(delta_peak-56), rep(" ", 2),
                  as.character(delta_peak-45), rep(" ", 2),
                  as.character(delta_peak-36), rep(" ", 2),
                  as.character(delta_peak-27), rep(" ", 2),
                  as.character(delta_peak-18), rep(" ", 2),
                  as.character(delta_peak-9), rep(" ", 2),
                  "2nd Wave peak: \n 2021-09-08")
