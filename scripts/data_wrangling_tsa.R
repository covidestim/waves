rm(list = ls())
gc()

library(tidyverse)
library(sf)

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

## Plot US map and highways
# ggplot()+
#   geom_sf(data = us_states,
#           color = "thistle4")+
#   geom_sf(data = highways,
#           color = "deeppink1")+
#   theme_map()

dataset <- "preomicron"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv"))

# dataset <- "omicronera"
# 
# ## Omicron-era
# CAR_df_omicronera <- vroom::vroom(paste0("data-products/tsa_",
#                                          dataset,
#                                          ".csv"))

## Building GeoJSON datasets, complete ones

## Pre-Omicron
CAR_df_preomicron <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes_centroids) |> 
  dplyr::select(hexid, date, population, infections, infectionsPC, mean, sd, X, Y, geometry) |> 
  st_as_sf()

# ## Omicron-era
# CAR_df_omicronera <- CAR_df_omicronera |> 
#   mutate(hexid = as.character(hexid),
#          infectionsPC = infectionsPC*1e5) |> 
#   left_join(hexes_centroids) |> 
#   dplyr::select(hexid, date, population, infections, infectionsPC, mean, sd, X, Y, geometry) |> 
#   st_as_sf()

## Saving as GeoJSON
# ## Pre-Omicron
# sf::st_write(obj = CAR_df_preomicron,
#              dsn = "data-products/geo-hexes/hex_tsa_preomicron.geojson", 
#              delete_dsn = T)
# 
# ## Omicron-era
# sf::st_write(obj = CAR_df_omicronera,
#              dsn = "data-products/geo-hexes/hex_tsa_omicronera.geojson", 
#              delete_dsn = T)

## Specific waves GeoJSON
## Joining
# CAR_df <- rbind(CAR_df_preomicron, CAR_df_omicronera)

# sf::st_write(obj = CAR_df,
#              dsn = "data-products/geo-hexes/hex_tsa_all.geojson",
#              delete_dsn = T)

## Pre-Omicron
sum_preomicron <- CAR_df_preomicron |> 
  st_drop_geometry() |> 
  group_by(date) |> 
  summarise(infectionsPC = sum(infectionsPC, na.rm = T))

## Ten most dates with infectionsPC
head(sum_preomicron[order(rank(-sum_preomicron$infectionsPC)),],15)

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

# ## Omicron-era
# sum_omicronera <- CAR_df_omicronera |> 
#   st_drop_geometry() |> 
#   group_by(date) |> 
#   summarise(infectionsPC = sum(infectionsPC, na.rm = T))
# 
# ## Ten most dates with infectionsPC
# head(sum_omicronera[order(rank(-sum_omicronera$infectionsPC)),],10)
# 
# omicronba1_peak <- as.Date("2022-01-20")
# omicronba5_peak <- as.Date("2022-07-21")

## Alpha
CAR_df_alpha <- CAR_df_preomicron |> 
  filter(date %in% seq.Date(from = alpha_peak - 63, 
                            to = alpha_peak + 63, length.out = 124))

# sf::st_write(obj = CAR_df_alpha,
#              dsn = "data-products/geo-hexes/hex_tsa_alpha_wave.geojson",
#              delete_dsn = T)

## Delta
CAR_df_delta <- CAR_df_preomicron |> 
  filter(date %in% seq.Date(from = delta_peak - 63, 
                            to = delta_peak + 63, length.out = 124))

# sf::st_write(obj = CAR_df_delta,
#              dsn = "data-products/geo-hexes/hex_tsa_delta_wave.geojson",
#              delete_dsn = T)

## Prototyping to the new
sum_preomicron <- CAR_df_preomicron |> 
  # st_drop_geometry() |> 
  group_by(date) |> 
  summarise(infectionsPC = sum(infectionsPC, na.rm = T))

fig2.a <- ggplot()+
  geom_line(data = sum_preomicron,
            aes(x = date, 
                y = infectionsPC))+
  theme_minimal()+
  scale_x_date(name = "Date",
               date_breaks = "2 months",
               # date_minor_breaks = "2 months",
               date_labels = "%b %y'")+
  ## Adding start and ending dates
  # annotate("segment",
  #          x = c(min(sum_preomicron$date), max(sum_preomicron$date)),
  #          xend = c(min(sum_preomicron$date), max(sum_preomicron$date)),
  #          y = rep(0,2),
  #          yend = rep(2e6,2),
  #          lty = "dashed",
  #          color = "grey80")+
  scale_y_continuous(name = "Estimated infection/day",
                     labels = scales::label_comma())+
  # fig1a
  ## Alpha wave marks
  annotate("rect",
           xmin = alpha_peak - 70,
           xmax = alpha_peak + 70,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(alpha_peak-63,
                 alpha_peak-45, 
                 alpha_peak-24, 
                 alpha_peak),
           y = rep(2e6, 4),
           label = LETTERS[2:5],
           size = 5)+
  annotate("segment", 
           yend = rep(2e6,4),
           y = c(4.5e5,7.5e5,1.25e6,1.65e6),
           x = c(alpha_peak-63,
                 alpha_peak-42, 
                 alpha_peak-21, 
                 alpha_peak),
           xend = c(alpha_peak-63,
                    alpha_peak-42, 
                    alpha_peak-21, 
                    alpha_peak),
           color = "grey50",
           linetype = "dashed")+
  ## Delta wave marks
  annotate("rect",
           xmin = delta_peak - 70,
           xmax = delta_peak + 70,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(delta_peak-63,
                 delta_peak-45, 
                 delta_peak-24, 
                 delta_peak),
           y = rep(0, 4),
           label = LETTERS[6:9],
           size = 5)+
  annotate("segment", 
           y = c(2.7e5, 6.5e5, 1.3e6, 1.67e6),
           yend = rep(0,4),
           x = c(delta_peak-63,
                 delta_peak-45, 
                 delta_peak-24, 
                 delta_peak),
           xend = c(delta_peak-63,
                    delta_peak-45, 
                    delta_peak-24, 
                    delta_peak),
           color = "grey50",
           linetype = "dashed")+
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_blank())
fig2.a

ggsave(plot = fig2.a,
       filename = "img/extra_figures/fig2a_wide.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.b <- ggplot()+
  geom_line(data = sum_preomicron,
            aes(x = date, 
                y = infectionsPC))+
  theme_minimal()+
  scale_x_date(name = "Date",
               date_breaks = "4 months",
               date_labels = "%b %y'")+
  scale_y_continuous(name = "Estimated infection/day",
                     labels = scales::label_comma())+
  ## Delta wave marks
  annotate("rect",
           xmin = delta_peak - 70,
           xmax = delta_peak + 70,
           ymin = 0, ymax = Inf,
           fill = "grey50",alpha = 0.2)+
  annotate("text",
           x = c(delta_peak-63,
                 delta_peak-45, 
                 delta_peak-24, 
                 delta_peak),
           y = rep(2e6, 4),
           label = LETTERS[6:9],
           size = 5)+
  annotate("segment", 
           y = rep(0,4),
           yend = rep(2e6,4),
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
fig2.b

ggsave(plot = fig2.b,
       filename = "img/extra_figures/fig2b.pdf",
       width = 16,
       height = 9, 
       dpi = 100)


# Hacky workaround for demo purposes
scale_x_date2 <- function(..., rescaler) {
  x <- scale_x_date(...)
  x$rescaler <- rescaler
  x
}

# The default `to` argument of the `rescale()` function is `c(0, 1)`.
# Here, we reverse that.
invert_scale <- function(x, to = c(1, 0), from = range(x)) {
  scales::rescale(x, to, from)
}

fig2.a.flip <- fig2.a+
  coord_flip()+ 
  scale_x_date2(rescaler = invert_scale, 
                name = "",
                date_breaks = "4 months",
                date_labels = "%b %y'")
fig2.a.flip

ggsave(plot = fig2.a.flip,
       filename = "img/extra_figures/fig2a_flipped.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.b.flip <- fig2.b+
  coord_flip()+ 
  scale_x_date2(rescaler = invert_scale, 
                name = "Date",
                date_breaks = "4 months",
                date_labels = "%b %y'")
fig2.b.flip

ggsave(plot = fig2.b.flip,
       filename = "img/extra_figures/fig2b_flipped.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

## hexgrids
dataset <- "preomicron"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv"))

# dataset <- "omicronera"
# 
# ## Omicron-era
# CAR_df_omicronera <- vroom::vroom(paste0("data-products/tsa_",
#                                          dataset,
#                                          ".csv"))

## Building GeoJSON datasets, complete ones

## Pre-Omicron
CAR_df_preomicron <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  select(hexid, date, population, infections, infectionsPC, mean, sd, geometry) |>
  st_as_sf() |> 
  st_transform(crs = 26915)

## Breakdowns of each peaks
breaks_plt <- seq(0,1500, 150)
labels_plt <- c(seq(0,900, 150), "1,050", "1,200", "1,350",'1,500+')
limits_plt <- c(0,1700)
color_option <- "magma"
na_color <- "grey70"

## Alpha wave snapshots
fig2.c <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak - 63)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  labs(title = (alpha_peak-63))
fig2.c

ggsave(plot = fig2.c,
       filename = "img/extra_figures/fig2c.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.d <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak - 42)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak-42))
fig2.d

ggsave(plot = fig2.d,
       filename = "img/extra_figures/fig2d.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.e <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak - 21)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak-21))
fig2.e

ggsave(plot = fig2.e,
       filename = "img/extra_figures/fig2e.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.f <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (alpha_peak)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (alpha_peak))
fig2.f

ggsave(plot = fig2.f,
       filename = "img/extra_figures/fig2f.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

## Delta wave snapshots
fig2.g <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak - 63)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak-63))
fig2.g

ggsave(plot = fig2.g,
       filename = "img/extra_figures/fig2g.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.h <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak - 42)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak-42))
fig2.h

ggsave(plot = fig2.h,
       filename = "img/extra_figures/fig2h.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.i <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak - 21)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC,
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak-21))
fig2.i

ggsave(plot = fig2.i,
       filename = "img/extra_figures/fig2i.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

fig2.j <- ggplot(data = CAR_df_preomicron |> 
                   filter(date == (delta_peak)) |> 
                   st_transform(crs=26915),
                 aes(fill = infectionsPC, 
                     color = infectionsPC))+
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
  guides(fill = guide_bins(title = "Estimated infections per capita/100k/day",
                           title.position = "top",
                           title.hjust = 0.5),
         color = "none")+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  labs(title = (delta_peak))
fig2.j

ggsave(plot = fig2.j,
       filename = "img/extra_figures/fig2j.pdf",
       width = 16,
       height = 9, 
       dpi = 100)

## Fig.2 patchwork
library(patchwork)

fig2 <- ((fig2.c / fig2.d / fig2.e / fig2.f) | (fig2.a.flip) | (fig2.g / fig2.h / fig2.i / fig2.j))+
  plot_layout(guides = "collect")&
  theme(legend.position = "bottom")
fig2

ggsave(plot = fig2,
       filename = "img/extra_figures/fig2_vertical.pdf",
       width = 16,
       height = 9, 
       dpi = 300)

fig2.wide <- ((fig2.c | fig2.d | fig2.e | fig2.f) / (fig2.a) / (fig2.g | fig2.h | fig2.i | fig2.j))+
  plot_layout(guides = "collect")&
  guides(fill = guide_bins(title.position = "left",
                           title = "Estimated infections per capita/100k/day",
                           title.hjust = 0.5))&
  theme(legend.position = "right",
        legend.title = element_text(angle = 90),
        legend.direction = "vertical", 
        legend.key.height = grid::unit(1, "cm"))
fig2.wide

ggsave(plot = fig2.wide,
       filename = "img/extra_figures/fig2_wide.pdf",
       width = 16,
       height = 9, 
       dpi = 300)


## Supplementary figure for detailed maps
figS2 <- ((fig2.c | fig2.d | fig2.e | fig2.f) / (fig2.g | fig2.h | fig2.i | fig2.j))+
  plot_layout(guides = "collect")&
  guides(fill = guide_bins(title.position = "left",
                           title = "Estimated infections per capita/100k/day",
                           title.hjust = 0.5))&
  theme(legend.position = "right",
        legend.title = element_text(angle = 90),
        legend.direction = "vertical", 
        legend.key.height = grid::unit(1, "cm"))
figS2

ggsave(plot = figS2,
       filename = "img/extra_figures/figS2.pdf",
       width = 16,
       height = 9, 
       dpi = 300)


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


fig3.wide <- ((fig3.a | fig3.b | fig3.c | fig3.d) / (fig2.a) /
                (fig3.e | fig3.f | fig3.g | fig3.h))+
  plot_layout(guides = "collect")&
  guides(fill = guide_bins(title.position = "left",
                           title = "Estimated infections per capita/100k/day",
                           title.hjust = 0.5))&
  theme(legend.position = "right",
        legend.title = element_text(angle = 90),
        legend.direction = "vertical", 
        legend.key.height = grid::unit(1, "cm"))
fig3.wide

ggsave(plot = fig3.wide,
       filename = "img/extra_figures/fig3_wide.png",
       width = 16, 
       height = 9,
       dpi = 100)

ggsave(plot = fig3.wide,
       filename = "img/extra_figures/fig3_wide.pdf",
       width = 16, 
       height = 9,
       dpi = 300)


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

## Supplementary figure for detailed maps
figS3 <- ((fig3.a | fig3.b | fig3.c | fig3.d) / (fig3.e | fig3.f | fig3.g | fig3.h))+
  plot_layout(guides = "collect")&
  guides(fill = guide_bins(title.position = "left",
                           title = "Trend surface of infections estimated/100k/day",
                           title.hjust = 0.5))&
  theme(legend.position = "right",
        legend.title = element_text(angle = 90),
        legend.direction = "vertical", 
        legend.key.height = grid::unit(1, "cm"))
figS3

ggsave(plot = figS3,
       filename = "img/extra_figures/figS3.png",
       width = 16,
       height = 9, 
       dpi = 300)

ggsave(plot = figS3,
       filename = "img/extra_figures/figS3.pdf",
       width = 16,
       height = 9, 
       dpi = 300)

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
delta_peak <- as.Date("2021-09-08")

## Threshold for filtering given the distribution, any value of trend that it is above the 3rd Quarter of the trend distribution
threshold_mean <- 165

## Pre-Omicron
CAR_lag_alpha <- CAR_df_preomicron |> 
  mutate(hexid = as.character(hexid)) |> 
  left_join(hexes) |> 
  dplyr::select(hexid, date, mean, sd, geometry) |>
  st_as_sf() |> 
  filter(date %in% seq.Date(to = alpha_peak,
                            from = (alpha_peak-63),
                            length.out = 63))|>
  # mutate(contour_surface = factor(case_when(mean >= threshold_mean & 
  #                                             date == (alpha_peak-63) ~ 1,
  #                                           mean >= threshold_mean & 
  #                                             date == (alpha_peak-56) ~ 2,
  #                                           mean >= threshold_mean & 
  #                                             date == (alpha_peak-49) ~ 3,
  #                                           mean >= threshold_mean & 
  #                                             date == (alpha_peak-42) ~ 4,
  #                                           mean >= threshold_mean & 
  #                                             date == (alpha_peak-35) ~ 5,
  #                                           mean >= threshold_mean & 
#                                             date == (alpha_peak-28) ~ 6,
#                                           mean >= threshold_mean & 
#                                             date == (alpha_peak-21) ~ 7,
#                                           mean >= threshold_mean & 
#                                             date == (alpha_peak-14) ~ 8,
#                                           mean >= threshold_mean & 
#                                             date == (alpha_peak-7) ~ 9,
#                                           mean >= threshold_mean & 
#                                             date == (alpha_peak) ~ 10),
#                                 labels = c(str_c(rev(seq(7,63, 7)), 
#                                                  " days"),
#                                            "peak"),
#                                 ordered = T))|> 
st_transform(crs = 26915)

contour_alpha <- CAR_lag_alpha|> 
  # filter(!is.na(contour_surface)) |> 
  mutate(days = as.numeric(case_when(mean >= threshold_mean ~ (alpha_peak - date)))) |>
  group_by(days, date) |> 
  summarise(geometry = st_union(geometry),
            area = format(round(units::set_units(st_area(st_union(geometry)), 
                                          km^2),0)))

contour_alpha <- contour_alpha |> 
  mutate(area_format = format(area, 
                              big.mark = ","),
         area_numeric = units::drop_units(round(units::set_units(st_area(st_union(geometry)), 
                                                   km^2),0)))


palette_name <- "Inferno"
labels <- seq.Date(from = (alpha_peak - 63),
                   to = alpha_peak,
                   length.out = 9)

ggplot()+
  ## peak layers
  geom_sf(data = contour_alpha |> 
            filter(!is.na(days)),
          aes(fill = date, color = date),
          # color = "black",
          alpha = 0.15)+
  colorspace::scale_fill_binned_sequential(palette = palette_name,
                                           rev = F,
                                           trans = "date",
                                           # breaks = breaks,
                                           labels = scales::label_date(format = "%b %Y"),
                                           guide = guide_colorbar(reverse = TRUE))+
  colorspace::scale_color_binned_sequential(palette = palette_name,
                                           rev = F,
                                           trans = "date",
                                           # breaks = breaks,
                                           labels = scales::label_date(format = "%b %Y"),
                                           guide = guide_colorbar(reverse = TRUE))+
  theme_void()+
  theme(legend.position = "right",
        legend.title = element_text(hjust = 0.5, angle = 90), 
        legend.title.position = "left",
        legend.direction = "vertical",
        legend.key.height = unit(1, "in"))

## Creating the US border for the lower 48 states
us_border <- us_states |> 
  st_union() |> 
  st_as_sf() |> 
  rename(geometry = x)

palette_name <- "Inferno"
# 
# fig4a_layered <- ggplot()+
#   ## US States border
#   geom_sf(data = us_states|> 
#             rotate_data(shear_cos_x = shear_cos_x,
#                         shear_sin_x = shear_sin_x,
#                         shear_cos_y = shear_cos_y,
#                         shear_sin_y = shear_sin_y,
#                         x_add = x - 1.5e6,
#                         y_add = y - 1.5e6),
#           fill = "transparent",
#           color = "thistle4")+
#   ## peak layer
#   geom_sf(data = contour_alpha |> 
#             filter(contour_surface == "peak") |> 
#             rotate_data(shear_cos_x = shear_cos_x,
#                         shear_sin_x = shear_sin_x,
#                         shear_cos_y = shear_cos_y,
#                         shear_sin_y = shear_sin_y,
#                         x_add = x - 1.5e6,
#                         y_add = y - 1.5e6),
#           aes(fill = area, color = area),
#           alpha = 0.15)+
#   ## peak area label
#   geom_sf_label(data = contour_alpha |> 
#                   filter(contour_surface == "peak")|>
#                   rotate_data(shear_cos_x = shear_cos_x,
#                               shear_sin_x = shear_sin_x,
#                               shear_cos_y = shear_cos_y,
#                               shear_sin_y = shear_sin_y,
#                               x_add = x - 1.5e6,
#                               y_add = y - 1.5e6),
#                 aes(label = date, fill = area), 
#                 size = 3,
#                 show.legend = F,
#                 position = position_nudge(x = 0,
#                                           y = -2.5e6))+
#   # ## Border at peak
#   # geom_sf(data = us_border |> 
#   #           cbind(area = contour_alpha |> 
#   #                   filter(contour_surface == "peak")|>
#   #                   pull(var = area)) |> 
#   #           rotate_data(shear_cos_x = shear_cos_x,
#   #                       shear_sin_x = shear_sin_x,
#   #                       shear_cos_y = shear_cos_y,
#   #                       shear_sin_y = shear_sin_y,
#   #                       x_add = x - 1.5e6,
#   #                       y_add = y - 1.5e6),
# #         aes(color = area),
# #         fill = "transparent")+
# ## 14 days layer
# geom_sf(data = contour_alpha |> 
#           filter(contour_surface == "14 days")|> 
#           rotate_data(shear_cos_x = shear_cos_x,
#                       shear_sin_x = shear_sin_x,
#                       shear_cos_y = shear_cos_y,
#                       shear_sin_y = shear_sin_y,
#                       x_add = x - 1.2e6,
#                       y_add = y - 1.2e6),
#         aes(fill = area, color = area),
#         alpha = 0.55)+
#   ## 14 days area label
#   geom_sf_label(data = contour_alpha |> 
#                   filter(contour_surface == "14 days")|>
#                   rotate_data(shear_cos_x = shear_cos_x,
#                               shear_sin_x = shear_sin_x,
#                               shear_cos_y = shear_cos_y,
#                               shear_sin_y = shear_sin_y,
#                               x_add = x - 1.2e6,
#                               y_add = y - 1.2e6),
#                 aes(label = date, fill = area), 
#                 size = 3,
#                 show.legend = F,
#                 position = position_nudge(x = 0,
#                                           y = -2.5e6))+
#   # ## 14 days US border
#   # geom_sf(data = us_border |> 
#   #           cbind(area = contour_alpha |> 
#   #                   filter(contour_surface == "14 days")|>
#   #                   pull(var = area)) |> 
#   #           rotate_data(shear_cos_x = shear_cos_x,
#   #                       shear_sin_x = shear_sin_x,
#   #                       shear_cos_y = shear_cos_y,
#   #                       shear_sin_y = shear_sin_y,
#   #                       x_add = x - 1e6,
#   #                       y_add = y - 1e6),
# #         aes(color = area),
# #         fill = "transparent")+
# ## 28 days layer
# geom_sf(data = contour_alpha |> 
#           filter(contour_surface == "28 days")|> 
#           rotate_data(shear_cos_x = shear_cos_x,
#                       shear_sin_x = shear_sin_x,
#                       shear_cos_y = shear_cos_y,
#                       shear_sin_y = shear_sin_y,
#                       x_add = x - 9e5,
#                       y_add = y - 9e5),
#         aes(fill = area, color = area),
#         alpha = 0.75)+
#   ## 28 days area label
#   geom_sf_label(data = contour_alpha |> 
#                   filter(contour_surface == "28 days")|>
#                   rotate_data(shear_cos_x = shear_cos_x,
#                               shear_sin_x = shear_sin_x,
#                               shear_cos_y = shear_cos_y,
#                               shear_sin_y = shear_sin_y,
#                               x_add = x - 9e5,
#                               y_add = y - 9e5),
#                 aes(label = date, fill = area), 
#                 size = 3,
#                 show.legend = F,
#                 position = position_nudge(x = 0,
#                                           y = -2.5e6))+
#   # ## 28 days US border
#   # geom_sf(data = us_border |> 
#   #           cbind(area = contour_alpha |> 
#   #                   filter(contour_surface == "28 days")|>
#   #                   pull(var = area)) |>
#   #           rotate_data(shear_cos_x = shear_cos_x,
#   #                       shear_sin_x = shear_sin_x,
#   #                       shear_cos_y = shear_cos_y,
#   #                       shear_sin_y = shear_sin_y,
#   #                       x_add = x - 5e5,
#   #                       y_add = y - 5e5),
# #         aes(color = area),
# #         fill = "transparent")+
# ## 42 days layer
# geom_sf(data = contour_alpha |>
#           filter(contour_surface == "42 days")|>
#           rotate_data(shear_cos_x = shear_cos_x,
#                       shear_sin_x = shear_sin_x,
#                       shear_cos_y = shear_cos_y,
#                       shear_sin_y = shear_sin_y,
#                       x_add = x - 6e5,
#                       y_add = y - 6e5),
#         aes(fill = area, color = area),
#         alpha = 0.95)+
#   ## 42 days area label
#   geom_sf_label(data = contour_alpha |> 
#                   filter(contour_surface == "42 days")|>
#                   rotate_data(shear_cos_x = shear_cos_x,
#                               shear_sin_x = shear_sin_x,
#                               shear_cos_y = shear_cos_y,
#                               shear_sin_y = shear_sin_y,
#                               x_add = x - 6e5,
#                               y_add = y - 6e5),
#                 aes(label = date, fill = area), 
#                 size = 3,
#                 show.legend = F,
#                 position = position_nudge(x = 0,
#                                           y = -2.5e6))+
#   # ## 42 days US border
#   # geom_sf(data = us_border |> 
#   #           cbind(area = contour_alpha |> 
#   #                   filter(contour_surface == "42 days")|>
#   #                   pull(var = area)) |>
#   #           rotate_data(shear_cos_x = shear_cos_x,
#   #                       shear_sin_x = shear_sin_x,
#   #                       shear_cos_y = shear_cos_y,
#   #                       shear_sin_y = shear_sin_y,
#   #                       x_add = x,
#   #                       y_add = y),
# #         aes(color = area),
# #         fill = "transparent")+
# colorspace::scale_color_discrete_divergingx(palette = palette_name, 
#                                             na.value = "grey70",
#                                             rev = F,
#                                             name = "")+
#   colorspace::scale_fill_discrete_divergingx(palette = palette_name,
#                                              na.value = "grey70",
#                                              rev = F,
#                                              name = "")+
#   guides(color = "none")+
#   theme_void()+
#   theme(legend.position = "none",
#         legend.title.position = "top",
#         legend.title = element_text(hjust = 0.5))+
#   guides(fill = guide_legend(reverse = T))+
#   labs(title = "1st wave")
# fig4a_layered

contour_alpha <- CAR_lag_alpha |> 
  filter(!is.na(contour_surface)) |> 
  group_by(contour_surface, date) |> 
  summarise(geometry = st_union(geometry),
            area = format(round(units::set_units(st_area(st_union(geometry)), 
                                          km^2),0), 
                          big.mark = ","))

contour_alpha <- contour_alpha |> 
  mutate(date = factor(date))

palete_values <- colorspace::sequential_hcl(10, "Purple-Yellow")

fig4a <- ggplot()+
  ## peak layers
  geom_sf(data = contour_alpha |> 
            filter(contour_surface == "peak"),
          aes(fill = area, color = area),
          alpha = 0.25)+
  ## 7 days layer
  geom_sf(data = contour_alpha |>
            filter(contour_surface == "7 days"),
          aes(fill = area, color = area),
          alpha = 0.25)+
  # ## 14 days layer
  geom_sf(data = contour_alpha |>
            filter(contour_surface == "14 days"),
          aes(fill = area, color = area),
          alpha = 0.25)+
  # ## 21 days layer
  geom_sf(data = contour_alpha |>
            filter(contour_surface == "21 days"),
          aes(fill = area, color = area),
          alpha = 0.25)+
  # ## 28 days layer
  # geom_sf(data = contour_alpha |>
  #           filter(contour_surface == "28 days"),
  #         aes(fill = area, color = area),
  #         alpha = 0.25)+
  # ## 35 days layer
  geom_sf(data = contour_alpha |>
            filter(contour_surface == "35 days"),
          aes(fill = area, color = area),
          alpha = 0.25)+
  # ## 42 days layer
  # geom_sf(data = contour_alpha |>
  #           filter(contour_surface == "42 days"),
  #         aes(fill = area, color = area),
  #         alpha = 0.25)+
  # ## 49 days layer
  geom_sf(data = contour_alpha |>
            filter(contour_surface == "49 days"),
          aes(fill = area, color = area),
          alpha = 0.25)+
  # ## 56 days layer
  # geom_sf(data = contour_alpha |>
  #           filter(contour_surface == "56 days"),
  #         aes(fill = area, color = area),
  #         alpha = 0.25)+
  # ## 63 days layer
  # geom_sf(data = contour_alpha |>
  #           filter(contour_surface == "63 days"),
  #         aes(fill = area, color = area),
  #         alpha = 0.25)+
  scale_color_manual(values = palete_values)+
  scale_fill_manual(values = palete_values)+
  guides(color = "none")+
  theme_void()+
  theme(legend.position = "bottom",
        # legend.title.position = "right",
        legend.title = element_blank())+
  # guides(fill = guide_legend(reverse = T, 
  #                            nrow = 1, 
  #                            title = "", 
  #                            # position = "bottom",
  #                            # override.aes = T,
  #                            order = 1),
  #        color = guide_legend(reverse = T, 
  #                             nrow = 1,
  #                             title = "",
  #                             # position = "bottom",
  #                             # override.aes = T,
#                             order = 2))+
labs(title = "1st wave")
fig4a

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
            area = format(round(set_units(st_area(st_union(geometry)), km^2), 0), 
                          big.mark = ",")) |> 
  arrange(date)


fig4b_layered <- ggplot()+
  ## peak layer
  geom_sf(data = contour_delta |> 
            filter(contour_surface == "peak") |> 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x - 1.5e6,
                        y_add = y - 1.5e6),
          aes(fill = area, color = area),
          alpha = 0.15)+
  ## peak area label
  geom_sf_label(data = contour_delta |> 
                  filter(contour_surface == "peak")|>
                  rotate_data(shear_cos_x = shear_cos_x,
                              shear_sin_x = shear_sin_x,
                              shear_cos_y = shear_cos_y,
                              shear_sin_y = shear_sin_y,
                              x_add = x - 1.5e6,
                              y_add = y - 1.5e6),
                aes(label = date, fill = area), 
                size = 3,
                show.legend = F,
                position = position_nudge(x = 0,
                                          y = -2.5e6))+
  ## Border at peak
  geom_sf(data = us_border |> 
            cbind(area = contour_delta |> 
                    filter(contour_surface == "peak")|>
                    pull(var = area)) |> 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x - 1.5e6,
                        y_add = y - 1.5e6),
          aes(color = area),
          fill = "transparent")+
  ## 14 days layer
  geom_sf(data = contour_delta |> 
            filter(contour_surface == "14 days")|> 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x - 1e6,
                        y_add = y - 1e6),
          aes(fill = area, color = area),
          alpha = 0.55)+
  ## 14 days area label
  geom_sf_label(data = contour_delta |> 
                  filter(contour_surface == "14 days")|>
                  rotate_data(shear_cos_x = shear_cos_x,
                              shear_sin_x = shear_sin_x,
                              shear_cos_y = shear_cos_y,
                              shear_sin_y = shear_sin_y,
                              x_add = x - 1e6,
                              y_add = y - 1e6),
                aes(label = date, fill = area), 
                size = 3,
                show.legend = F,
                position = position_nudge(x = 0,
                                          y = -2.5e6))+
  ## 14 days US border
  geom_sf(data = us_border |> 
            cbind(area = contour_delta |> 
                    filter(contour_surface == "14 days")|>
                    pull(var = area)) |> 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x - 1e6,
                        y_add = y - 1e6),
          aes(color = area),
          fill = "transparent")+
  ## 28 days layer
  geom_sf(data = contour_delta |> 
            filter(contour_surface == "28 days")|> 
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x - 5e5,
                        y_add = y - 5e5),
          aes(fill = area, color = area),
          alpha = 0.75)+
  ## 28 days area label
  geom_sf_label(data = contour_delta |> 
                  filter(contour_surface == "28 days")|>
                  rotate_data(shear_cos_x = shear_cos_x,
                              shear_sin_x = shear_sin_x,
                              shear_cos_y = shear_cos_y,
                              shear_sin_y = shear_sin_y,
                              x_add = x - 5e5,
                              y_add = y - 5e5),
                aes(label = date, fill = area), 
                size = 3,
                show.legend = F,
                position = position_nudge(x = 0,
                                          y = -2.5e6))+
  ## 28 days US border
  geom_sf(data = us_border |> 
            cbind(area = contour_delta |> 
                    filter(contour_surface == "28 days")|>
                    pull(var = area)) |>
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x - 5e5,
                        y_add = y - 5e5),
          aes(color = area),
          fill = "transparent")+
  ## 56 days layer
  geom_sf(data = contour_delta |>
            filter(contour_surface == "56 days")|>
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x,
                        y_add = y),
          aes(fill = area, color = area),
          alpha = 0.95)+
  ## 56 days area label
  geom_sf_label(data = contour_delta |> 
                  filter(contour_surface == "56 days")|>
                  rotate_data(shear_cos_x = shear_cos_x,
                              shear_sin_x = shear_sin_x,
                              shear_cos_y = shear_cos_y,
                              shear_sin_y = shear_sin_y,
                              x_add = x,
                              y_add = y),
                aes(label = date, fill = area), 
                size = 3,
                show.legend = F,
                position = position_nudge(x = 0,
                                          y = -2.5e6))+
  ## 56 days US border
  geom_sf(data = us_border |> 
            cbind(area = contour_delta |> 
                    filter(contour_surface == "56 days")|>
                    pull(var = area)) |>
            rotate_data(shear_cos_x = shear_cos_x,
                        shear_sin_x = shear_sin_x,
                        shear_cos_y = shear_cos_y,
                        shear_sin_y = shear_sin_y,
                        x_add = x,
                        y_add = y),
          aes(color = area),
          fill = "transparent")+
  colorspace::scale_color_discrete_divergingx(palette = palette_name, 
                                              na.value = "grey70",
                                              rev = F,
                                              name = "")+
  colorspace::scale_fill_discrete_divergingx(palette = palette_name,
                                             na.value = "grey70",
                                             rev = F,
                                             name = "")+
  guides(color = "none")+
  theme_void()+
  theme(legend.position = "none",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5))+
  guides(fill = guide_legend(reverse = T))+
  labs(title = "2nd wave")
fig4b_layered

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
fig4c <- ggplot()+
  geom_col(data = rbind(CAR_area_alpha, CAR_area_delta) |>
             filter(days >= 7, days <= 56),
           aes(x = days, y = speed, fill = wave),
           alpha = 0.75,
           position = position_dodge())+
  geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
  geom_vline(xintercept = c(56,28,14,7), lty = "dotted", color = "grey50")+
  theme_minimal()+
  labs(x = "Days before national curve peak \n [days]", 
       y = "Speed of invasion")+
  scale_x_reverse(breaks = seq(7,63,7))+
  units::scale_y_units(labels = scales::label_comma())+
  colorspace::scale_fill_discrete_divergingx()+
  colorspace::scale_color_discrete_divergingx()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.title = element_text(hjust = 0.5))
fig4c

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

# ## Centroid movement
# figS5 <- ggplot()+
#   ## Alpha
#   geom_segment(data = CAR_velocity_alpha |> 
#                  filter(days %in% seq(0,63, 7)) |> 
#                  mutate(lat = st_coordinates(geometry)[,"X"],
#                         latend = lag(lat),
#                         lon = st_coordinates(geometry)[,"Y"],
#                         lonend = lag(lon)) |> 
#                  st_transform(crs = 26915),
#                arrow = arrow(length = unit(0.1, "cm")),
#                # show.legend = F,
#                aes(color = wave,
#                    x = lat, xend = latend,
#                    y = lon, yend = lonend))+
#   ## Delta
#   geom_segment(data = CAR_velocity_delta |> 
#                  filter(days %in% seq(0,63, 7)) |> 
#                  mutate(lat = st_coordinates(geometry)[,"X"],
#                         latend = lag(lat),
#                         lon = st_coordinates(geometry)[,"Y"],
#                         lonend = lag(lon)),
#                arrow = arrow(length = unit(0.1, "cm")),
#                # show.legend = F,
#                aes(color = wave,
#                    x = lat, xend = latend,
#                    y = lon, yend = lonend))+
#   ## Omicron BA.1
#   geom_segment(data = CAR_velocity_omicronba1 |> 
#                  filter(days %in% seq(0,63, 7)) |> 
#                  mutate(lat = st_coordinates(geometry)[,"X"],
#                         latend = lag(lat),
#                         lon = st_coordinates(geometry)[,"Y"],
#                         lonend = lag(lon)),
#                arrow = arrow(length = unit(0.1, "cm")),
#                # show.legend = F,
#                aes(color = wave,
#                    x = lat, xend = latend,
#                    y = lon, yend = lonend))+
#   ## Omicron BA.5
#   geom_segment(data = CAR_velocity_omicronba5 |> 
#                  filter(days %in% seq(0,63, 7)) |> 
#                  mutate(lat = st_coordinates(geometry)[,"X"],
#                         latend = lag(lat),
#                         lon = st_coordinates(geometry)[,"Y"],
#                         lonend = lag(lon)),
#                arrow = arrow(length = unit(0.1, "cm")),
#                # show.legend = F,
#                aes(color = wave,
#                    x = lat, xend = latend,
#                    y = lon, yend = lonend))+
#   ## US map
#   geom_sf(data = us_states,
#           color = "deeppink3",
#           fill = "transparent")+
#   ## scale for colors
#   colorspace::scale_color_discrete_divergingx()+
#   theme_minimal()+
#   theme(axis.title = element_blank(),
#         legend.position = "bottom",
#         legend.title = element_blank())
# figS5
# 
# ggsave(plot = figS5,
#        filename = "img/extra_figures/figS5.png",
#        width = 16,
#        height = 9,
#        dpi = 200)
# 
# ## Contour plot
# fig4a <- ggplot()+
#   geom_sf(data = CAR_lag_alpha |> filter(is.na(contour_surface)),
#           color = NA)+
#   geom_sf(data = CAR_lag_alpha |> filter(contour_surface == "peak"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_alpha |> filter(contour_surface == "7 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_alpha |> filter(contour_surface == "14 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_alpha |> filter(contour_surface == "28 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_alpha |> filter(contour_surface == "56 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_segment(data = CAR_velocity_alpha |> 
#                  filter(days %in% seq(0,63, 7)) |> 
#                  mutate(lat = st_coordinates(geometry)[,"X"],
#                         latend = lag(lat),
#                         lon = st_coordinates(geometry)[,"Y"],
#                         lonend = lag(lon)) |> 
#                  st_transform(crs = 26915),
#                arrow = arrow(length = unit(0.1, "cm")),
#                show.legend = F,
#                aes(x = lat, xend = latend,
#                    y = lon, yend = lonend))+
#   geom_sf(data = us_states, color = "grey70", fill = "transparent")+
#   theme_minimal()+
#   colorspace::scale_color_discrete_sequential(palette = "Sunset", 
#                                               na.value = "grey70",
#                                               rev = F,
#                                               name = "Days before national curve peak")+
#   colorspace::scale_fill_discrete_sequential(palette = "Sunset",
#                                              na.value = "grey70",
#                                              rev = F,
#                                              name = "Days before national curve peak")+
#   guides(color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.title = element_text(hjust = 0.5),
#         axis.title = element_blank())+
#   guides(fill = guide_legend(reverse = T))+
#   labs(title = "Alpha wave")
# fig4a
# 
# ## Delta
# fig4b <- ggplot()+
#   geom_sf(data = CAR_lag_delta |> filter(is.na(contour_surface)),
#           color = NA)+
#   geom_sf(data = CAR_lag_delta |> filter(contour_surface == "peak"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_delta |> filter(contour_surface == "7 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_delta |> filter(contour_surface == "14 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_delta |> filter(contour_surface == "28 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_sf(data = CAR_lag_delta |> filter(contour_surface == "56 days"),
#           aes(fill = contour_surface, color = contour_surface))+
#   geom_segment(data = CAR_velocity_delta |> 
#                  filter(days %in% seq(0,63, 7)) |> 
#                  mutate(lat = st_coordinates(geometry)[,"X"],
#                         latend = lag(lat),
#                         lon = st_coordinates(geometry)[,"Y"],
#                         lonend = lag(lon)) |> 
#                  st_transform(crs = 26915),
#                arrow = arrow(length = unit(0.1, "cm")),
#                show.legend = F,
#                aes(x = lat, xend = latend,
#                    y = lon, yend = lonend))+
#   geom_sf(data = us_states, color = "grey70", fill = "transparent")+
#   theme_minimal()+
#   colorspace::scale_color_discrete_sequential(palette = "Sunset", 
#                                               na.value = "grey70",
#                                               rev = F,
#                                               name = "Days before national curve peak")+
#   colorspace::scale_fill_discrete_sequential(palette = "Sunset",
#                                              na.value = "grey70",
#                                              rev = F,
#                                              name = "Days before national curve peak")+
#   guides(color = "none")+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.title = element_text(hjust = 0.5),
#         axis.title = element_blank())+
#   guides(fill = guide_legend(reverse = T))+
#   labs(title = "Delta wave")
# fig4b
# 
# ## Alternative version, Figure.4
# fig4 <- ((fig4a / fig4b)+
#            plot_layout(guides = "collect")&
#            theme(legend.position = "bottom")| fig4c&
#            theme(legend.position = "bottom"))
# fig4
# 
# ggsave(plot = fig4,
#        filename = "img/extra_figures/fig4.png",
#        width = 16,
#        height = 9,
#        dpi = 100)
# 
# ggsave(plot = fig4,
#        filename = "img/extra_figures/fig4.pdf",
#        width = 16,
#        height = 9,
#        dpi = 100)
# 
# # us_area <- set_units(st_area(st_union(hexes)), km^2)
# 
# # ## Threshold for Omicrons
# # threshold_mean <- 1200
# # 
# # dataset <- "omicronera"
# # 
# # ## Omicron-era
# # CAR_df_omicronera <- vroom::vroom(paste0("data-products/tsa_",
# #                                          dataset, 
# #                                          ".csv"))
# # 
# # omicronba1_peak <- as.Date("2022-01-20")
# # omicronba5_peak <- as.Date("2022-07-21")
# # 
# # ## Omicron BA.1
# # CAR_lag_omicronba1 <- CAR_df_omicronera |> 
# #   mutate(hexid = as.character(hexid),
# #          mean = mean*1e5,
# #          sd = sd*1e5) |> 
# #   left_join(hexes) |> 
# #   dplyr::select(hexid, date, mean, sd, geometry) |>
# #   st_as_sf() |> 
# #   filter(date %in% c(omicronba1_peak, (omicronba1_peak-7), (omicronba1_peak-14), (omicronba1_peak-28), (omicronba1_peak-56)))|> 
# #   mutate(contour_surface = factor(case_when(mean >= threshold_mean & date == (omicronba1_peak-56) ~ 0,
# #                                             mean >= threshold_mean & date == (omicronba1_peak-28) ~ 1,
# #                                             mean >= threshold_mean & date == (omicronba1_peak-14) ~ 2,
# #                                             mean >= threshold_mean & date == (omicronba1_peak-7) ~ 3,
# #                                             mean >= threshold_mean & date == (omicronba1_peak) ~ 4),
# #                                   labels = c("56 days","28 days", "14 days", "7 days", "peak"),
# #                                   ordered = T))|> 
# #   st_transform(crs = 26915)
# # 
# # ## CAR area
# # CAR_area_omicronba1 <- CAR_df_omicronera |> 
# #   mutate(hexid = as.character(hexid),
# #          mean = mean*1e5,
# #          sd = sd*1e5) |> 
# #   left_join(hexes) |> 
# #   dplyr::select(hexid, date, mean, sd, geometry) |>
# #   st_as_sf() |> 
# #   filter(date %in% seq.Date(omicronba1_peak, (omicronba1_peak-63), length.out = 63))|> 
# #   filter(mean >= threshold_mean) |> 
# #   mutate(days = (omicronba1_peak - date))|> 
# #   st_transform(crs = 26915) |> 
# #   group_by(days) |> 
# #   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
# #   mutate(wave = "Omicron BA.1")
# # 
# # ## Speed/Velocity calculation
# # CAR_area_omicronba1$speed <- units::set_units(c(0, 
# #                                                 -diff(as.numeric(CAR_area_omicronba1$area))), km^2/day)
# # 
# # ## Velocity
# # CAR_velocity_omicronba1 <- CAR_df_omicronera |> 
# #   mutate(hexid = as.character(hexid),
# #          mean = mean*1e5,
# #          sd = sd*1e5) |> 
# #   left_join(hexes) |> 
# #   dplyr::select(hexid, date, mean, sd, geometry) |>
# #   st_as_sf() |> 
# #   filter(date %in% seq.Date(omicronba1_peak, (omicronba1_peak-63), length.out = 63))|> 
# #   filter(mean >= threshold_mean) |> 
# #   mutate(days = (omicronba1_peak - date))|> 
# #   st_transform(crs = 26915) |> 
# #   group_by(days) |> 
# #   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
# #   mutate(geometry = st_centroid(geometry)) |> 
# #   mutate(wave = "Omicron BA.1")
# # 
# # CAR_area_omicronba1$velocity <- units::set_units(c(0, 
# #                                                    -diff(as.numeric(CAR_velocity_omicronba1$area))), km^2/day)
# # 
# # CAR_velocity_omicronba1$velocity <- units::set_units(c(0, 
# #                                                        -diff(as.numeric(CAR_velocity_omicronba1$area))), km^2/day)
# # 
# # fig4d <- ggplot()+
# #   geom_sf(data = CAR_lag_omicronba1 |> filter(is.na(contour_surface)),
# #           color = NA)+
# #   geom_sf(data = CAR_lag_omicronba1 |> filter(contour_surface == "peak"),
# #           aes(fill = contour_surface, color = contour_surface))+
# #   geom_sf(data = CAR_lag_omicronba1 |> filter(contour_surface == "7 days"),
# #           aes(fill = contour_surface, color = contour_surface))+
# #   geom_sf(data = CAR_lag_omicronba1 |> filter(contour_surface == "14 days"),
# #           aes(fill = contour_surface, color = contour_surface))+
# #   geom_sf(data = CAR_lag_omicronba1 |> filter(contour_surface == "28 days"),
# #           aes(fill = contour_surface, color = contour_surface))+
# #   geom_sf(data = CAR_lag_omicronba1 |> filter(contour_surface == "56 days"),
# #           aes(fill = contour_surface, color = contour_surface))+
# #   geom_segment(data = CAR_velocity_omicronba1 |> 
# #                  filter(days %in% c(0,7,14,28,56)) |> 
# #                  mutate(lat = st_coordinates(geometry)[,"X"],
# #                         latend = lag(lat),
# #                         lon = st_coordinates(geometry)[,"Y"],
# #                         lonend = lag(lon)) |> 
# #                  st_transform(crs = 26915),
# #                arrow = arrow(length = unit(0.1, "cm")),
# #                show.legend = F,
# #                aes(x = lat, xend = latend,
# #                    y = lon, yend = lonend))+
# #   geom_sf(data = us_states, color = "grey70", fill = "transparent")+
# #   theme_minimal()+
# #   colorspace::scale_color_discrete_sequential(palette = "Sunset", 
# #                                               na.value = "grey70",
# #                                               rev = F,
# #                                               name = "Days before national curve peak")+
# #   colorspace::scale_fill_discrete_sequential(palette = "Sunset",
# #                                              na.value = "grey70",
# #                                              rev = F,
# #                                              name = "Days before national curve peak")+
# #   guides(color = "none")+
# #   theme(legend.position = "bottom",
# #         legend.title.position = "top",
# #         legend.title = element_text(hjust = 0.5),
# #         axis.title = element_blank())+
# #   guides(fill = guide_legend(reverse = T))+
# #   labs(title = "Omicron BA.1 wave")
# # fig4d
# # 
# # # ## Omicron BA.1
# # # CAR_lag_omicronba5 <- CAR_df_omicronera |> 
# # #   mutate(hexid = as.character(hexid),
# # #          mean = mean*1e5,
# # #          sd = sd*1e5) |> 
# # #   left_join(hexes) |> 
# # #   dplyr::select(hexid, date, mean, sd, geometry) |>
# # #   st_as_sf() |> 
# # #   filter(date %in% c(omicronba5_peak, (omicronba5_peak-7), (omicronba5_peak-14), (omicronba5_peak-28), (omicronba5_peak-56)))|> 
# # #   mutate(contour_surface = factor(case_when(mean >= threshold_mean & date == (omicronba5_peak-56) ~ 0,
# # #                                             mean >= threshold_mean & date == (omicronba5_peak-28) ~ 1,
# # #                                             mean >= threshold_mean & date == (omicronba5_peak-14) ~ 2,
# # #                                             mean >= threshold_mean & date == (omicronba5_peak-7) ~ 3,
# # #                                             mean >= threshold_mean & date == (omicronba5_peak) ~ 4),
# # #                                   labels = c("56 days", "28 days", "14 days", "7 days", "peak"),
# # #                                   ordered = T))|> 
# # #   st_transform(crs = 26915)
# # # 
# # # ## Omicron BA.5
# # # CAR_area_omicronba5 <- CAR_df_omicronera |> 
# # #   mutate(hexid = as.character(hexid),
# # #          mean = mean*1e5,
# # #          sd = sd*1e5) |> 
# # #   left_join(hexes) |> 
# # #   dplyr::select(hexid, date, mean, sd, geometry) |>
# # #   st_as_sf() |> 
# # #   filter(date %in% seq.Date(omicronba5_peak, (omicronba5_peak-63), length.out = 63))|> 
# # #   filter(mean >= threshold_mean) |> 
# # #   mutate(days = (omicronba5_peak - date))|> 
# # #   st_transform(crs = 26915) |> 
# # #   group_by(days) |> 
# # #   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
# # #   mutate(wave = "Omicron BA.5")
# # # 
# # # ## Speed/Velocity calculation
# # # CAR_area_omicronba5$speed <- units::set_units(c(0, 
# # #                                                 -diff(as.numeric(CAR_area_omicronba5$area))), km^2/day)
# # # 
# # # ## Velocity
# # # CAR_velocity_omicronba5 <- CAR_df_omicronera |> 
# # #   mutate(hexid = as.character(hexid),
# # #          mean = mean*1e5,
# # #          sd = sd*1e5) |> 
# # #   left_join(hexes) |> 
# # #   dplyr::select(hexid, date, mean, sd, geometry) |>
# # #   st_as_sf() |> 
# # #   filter(date %in% seq.Date(omicronba5_peak, (omicronba5_peak-63), length.out = 63))|> 
# # #   filter(mean >= threshold_mean) |> 
# # #   mutate(days = (omicronba5_peak - date))|> 
# # #   st_transform(crs = 26915) |> 
# # #   group_by(days) |> 
# # #   summarise(area = units::set_units(st_area(st_union(geometry)), km^2)) |> 
# # #   mutate(geometry = st_centroid(geometry)) |> 
# # #   mutate(wave = "Omicron BA.5")
# # # 
# # # CAR_area_omicronba5$velocity <- units::set_units(c(0, 
# # #                                                    -diff(as.numeric(CAR_velocity_omicronba5$area))), km^2/day)
# # # 
# # # CAR_velocity_omicronba5$velocity <- units::set_units(c(0, 
# # #                                                        -diff(as.numeric(CAR_velocity_omicronba5$area))), km^2/day)
# # # 
# # # fig4e <- ggplot()+
# # #   geom_sf(data = CAR_lag_omicronba5 |> filter(is.na(contour_surface)),
# # #           color = NA)+
# # #   geom_sf(data = CAR_lag_omicronba5 |> filter(contour_surface == "peak"),
# # #           aes(fill = contour_surface, color = contour_surface))+
# # #   geom_sf(data = CAR_lag_omicronba5 |> filter(contour_surface == "7 days"),
# # #           aes(fill = contour_surface, color = contour_surface))+
# # #   geom_sf(data = CAR_lag_omicronba5 |> filter(contour_surface == "14 days"),
# # #           aes(fill = contour_surface, color = contour_surface))+
# # #   geom_sf(data = CAR_lag_omicronba5 |> filter(contour_surface == "28 days"),
# # #           aes(fill = contour_surface, color = contour_surface))+
# # #   geom_sf(data = CAR_lag_omicronba5 |> filter(contour_surface == "56 days"),
# # #           aes(fill = contour_surface, color = contour_surface))+
# # #   geom_segment(data = CAR_velocity_omicronba5 |> 
# # #                  filter(days %in% c(0,7,14,28,56)) |> 
# # #                  mutate(lat = st_coordinates(geometry)[,"X"],
# # #                         latend = lag(lat),
# # #                         lon = st_coordinates(geometry)[,"Y"],
# # #                         lonend = lag(lon)) |> 
# # #                  st_transform(crs = 26915),
# # #                arrow = arrow(length = unit(0.1, "cm")),
# # #                show.legend = F,
# # #                aes(x = lat, xend = latend,
# # #                    y = lon, yend = lonend))+
# # #   geom_sf(data = us_states, color = "grey70", fill = "transparent")+
# # #   theme_minimal()+
# # #   colorspace::scale_color_discrete_sequential(palette = "Sunset", 
# # #                                               na.value = "grey50",
# # #                                               rev = F,
# # #                                               name = "Days before national curve peak")+
# # #   colorspace::scale_fill_discrete_sequential(palette = "Sunset",
# # #                                              na.value = "grey50",
# # #                                              rev = F,
# # #                                              name = "Days before national curve peak")+
# # #   guides(color = "none")+
# # #   theme(legend.position = "bottom",
# # #         legend.title.position = "top",
# # #         legend.title = element_text(hjust = 0.5),
# # #         axis.title = element_blank())+
# # #   guides(fill = guide_legend(reverse = T))+
# # #   labs(title = "Omicron BA.5 wave")
# # # fig4e
# # # 
# # # ## Figure S4 - Centroid and countour plots
# # # figS4 <- ((fig4a/fig4b) | (fig4d/fig4e))+
# # #   plot_layout(guides = 'collect')+
# # #   plot_annotation(tag_levels = 'A')&
# # #   theme(legend.position = "bottom")
# # # figS4
# # # 
# # # ggsave(plot = figS4,
# # #        filename = "img/extra_figures/figS4.png",
# # #        width = 16,
# # #        height = 9,
# # #        dpi = 200)
# # # 
# # # ## Figure S5 - Area growth
# # # figS5 <- ggplot(data = rbind(CAR_area_alpha, CAR_area_delta, CAR_area_omicronba1, CAR_area_omicronba5) |> 
# # #                   filter(days %in% seq(7,56, 7)),
# # #                 aes(x = days, y = area/max(area), fill = wave))+
# # #   geom_col(position = position_dodge())+
# # #   geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
# # #   geom_vline(xintercept = c(56,28,14,7), lty = "dotted", color = "grey50")+
# # #   theme_minimal()+
# # #   labs(x = "Days before national curve peak \n [days]", 
# # #        y = "Proportion of total US area")+
# # #   scale_x_continuous(breaks = seq(7,63,7))+
# # #   units::scale_y_units(labels = scales::label_percent())+
# # #   colorspace::scale_fill_discrete_divergingx()+
# # #   theme(legend.position = "bottom",
# # #         legend.title.position = "top",
# # #         legend.title = element_blank())
# # # figS5
# # # 
# # # ggsave(filename = "img/extra_figures/figS4_percent_area.png",
# # #        plot = figS4,
# # #        width = 16, 
# # #        height = 9, 
# # #        dpi = 200)
# # # 
# # # ggsave(filename = "img/extra_figures/figS4_percent_area.pdf",
# # #        plot = figS4,
# # #        width = 16, 
# # #        height = 9, 
# # #        dpi = 200)
# # # 
# # # ## CAR area 
# # # fig4f <- ggplot(data = rbind(CAR_area_alpha, CAR_area_delta, CAR_area_omicronba1, CAR_area_omicronba5) |> 
# # #                   filter(days %in% seq(7,56, 7)),
# # #                 aes(x = days, y = speed, fill = wave))+
# # #   geom_col(position = position_dodge())+
# # #   geom_vline(xintercept = 7, color = "grey80", lty = "dashed")+
# # #   geom_vline(xintercept = c(56,28,14,7), lty = "dotted", color = "grey50")+
# # #   theme_minimal()+
# # #   labs(x = "Days before national curve peak \n [in days]", 
# # #        y = "Speed of invasion spread")+
# # #   scale_x_continuous(breaks = seq(7,63,7))+
# # #   units::scale_y_units(labels = scales::label_comma())+
# # #   colorspace::scale_fill_discrete_divergingx()+
# # #   # colorspace::scale_color_discrete_divergingx()+
# # #   theme(legend.position = "bottom",
# # #         legend.title.position = "top",
# # #         legend.title = element_blank())
# # # fig4f
# # # 
# # # figS6 <- (figS5 | fig4f)+
# # #   plot_layout(guides = 'collect')&
# # #   theme(legend.position = "bottom")
# # # figS6
# # # 
# # # ggsave(plot = figS6,
# # #        filename = "img/extra_figures/figS6.png",
# # #        width = 16,
# # #        height = 9, 
# # #        dpi = 200)
# # # 
# # # ggsave(plot = figS6,
# # #        filename = "img/extra_figures/figS6.pdf",
# # #        width = 16,
# # #        height = 9, 
# # #        dpi = 200)
# # # 
# # # ## Figure S3 - layered
# # # ## Delta wave layers
# # # contour_omicronba1 <- CAR_lag_omicronba1|> 
# # #   filter(!is.na(contour_surface)) |> 
# # #   group_by(contour_surface) |> 
# # #   summarise(geometry = st_union(geometry),
# # #             area = format(set_units(st_area(st_union(geometry)), km^2), 
# # #                           big.mark = ","))
# # # 
# # # ## Omicron BA.1
# # # figs3c_layered <- ggplot()+
# # #   ## peak layer
# # #   geom_sf(data = contour_omicronba1 |> 
# # #             filter(contour_surface == "peak") |> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 2e6,
# # #                         y_add = y - 2e6),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.15)+
# # #   ## peak area label
# # #   geom_sf_label(data = contour_omicronba1 |> 
# # #                   filter(contour_surface == "peak")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 2e6,
# # #                               y_add = y - 2e6),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -2.5e6))+
# # #   ## 7 days layer
# # #   geom_sf(data = contour_omicronba1 |> 
# # #             filter(contour_surface == "7 days")|> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 1.5e6,
# # #                         y_add = y - 1.5e6),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.35)+
# # #   ## 7 days area label
# # #   geom_sf_label(data = contour_omicronba1 |> 
# # #                   filter(contour_surface == "7 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 1.5e6,
# # #                               y_add = y - 1.5e6),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -2.5e6))+
# # #   ## 14 days layer
# # #   geom_sf(data = contour_omicronba1 |> 
# # #             filter(contour_surface == "14 days")|> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 1e6,
# # #                         y_add = y - 1e6),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.55)+
# # #   ## 14 days area label
# # #   geom_sf_label(data = contour_omicronba1 |> 
# # #                   filter(contour_surface == "14 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 1e6,
# # #                               y_add = y - 1e6),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -2.5e6))+
# # #   ## 28 days layer
# # #   geom_sf(data = contour_omicronba1 |> 
# # #             filter(contour_surface == "28 days")|> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 5e5,
# # #                         y_add = y - 5e5),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.75)+
# # #   ## 28 days area label
# # #   geom_sf_label(data = contour_omicronba1 |> 
# # #                   filter(contour_surface == "28 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 5e5,
# # #                               y_add = y - 5e5),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -1e6))+
# # #   ## 56 days layer
# # #   geom_sf(data = contour_omicronba1 |>
# # #             filter(contour_surface == "56 days")|>
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x,
# # #                         y_add = y),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.95)+
# # #   ## 56 days area label
# # #   geom_sf_label(data = contour_omicronba1 |> 
# # #                   filter(contour_surface == "56 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x,
# # #                               y_add = y),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -1e6))+
# # #   colorspace::scale_color_discrete_sequential(palette = "Sunset", 
# # #                                               na.value = "grey70",
# # #                                               rev = F,
# # #                                               name = "Days before national curve peak")+
# # #   colorspace::scale_fill_discrete_sequential(palette = "Sunset",
# # #                                              na.value = "grey70",
# # #                                              rev = F,
# # #                                              name = "Days before national curve peak")+
# # #   guides(color = "none")+
# # #   theme_void()+
# # #   theme(legend.position = "bottom",
# # #         legend.title.position = "top",
# # #         legend.title = element_text(hjust = 0.5))+
# # #   guides(fill = guide_legend(reverse = T))+
# # #   labs(title = "Omicron BA.1  wave")
# # # figs3c_layered
# # # 
# # # ## Omicron BA.5 wave layers
# # # contour_omicronba5 <- CAR_lag_omicronba5 |> 
# # #   filter(!is.na(contour_surface)) |> 
# # #   group_by(contour_surface) |> 
# # #   summarise(geometry = st_union(geometry),
# # #             area = format(set_units(st_area(st_union(geometry)), km^2), 
# # #                           big.mark = ","))
# # # 
# # # 
# # # ## Omicron BA.5
# # # figs3d_layered <- ggplot()+
# # #   ## peak layer
# # #   geom_sf(data = contour_omicronba5 |> 
# # #             filter(contour_surface == "peak") |> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 2e6,
# # #                         y_add = y - 2e6),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.15)+
# # #   ## peak area label
# # #   geom_sf_label(data = contour_omicronba5 |> 
# # #                   filter(contour_surface == "peak")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 2e6,
# # #                               y_add = y - 2e6),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -2.5e6))+
# # #   ## 7 days layer
# # #   geom_sf(data = contour_omicronba5 |> 
# # #             filter(contour_surface == "7 days")|> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 1.5e6,
# # #                         y_add = y - 1.5e6),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.35)+
# # #   ## 7 days area label
# # #   geom_sf_label(data = contour_omicronba5 |> 
# # #                   filter(contour_surface == "7 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 1.5e6,
# # #                               y_add = y - 1.5e6),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -2.5e6))+
# # #   ## 14 days layer
# # #   geom_sf(data = contour_omicronba5 |> 
# # #             filter(contour_surface == "14 days")|> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 1e6,
# # #                         y_add = y - 1e6),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.55)+
# # #   ## 14 days area label
# # #   geom_sf_label(data = contour_omicronba5 |> 
# # #                   filter(contour_surface == "14 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 1e6,
# # #                               y_add = y - 1e6),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -2.5e6))+
# # #   ## 28 days layer
# # #   geom_sf(data = contour_omicronba5 |> 
# # #             filter(contour_surface == "28 days")|> 
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x - 5e5,
# # #                         y_add = y - 5e5),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.75)+
# # #   ## 28 days area label
# # #   geom_sf_label(data = contour_omicronba5 |> 
# # #                   filter(contour_surface == "28 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x - 5e5,
# # #                               y_add = y - 5e5),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -1e6))+
# # #   ## 56 days layer
# # #   geom_sf(data = contour_omicronba5 |>
# # #             filter(contour_surface == "56 days")|>
# # #             rotate_data(shear_cos_x = shear_cos_x,
# # #                         shear_sin_x = shear_sin_x,
# # #                         shear_cos_y = shear_cos_y,
# # #                         shear_sin_y = shear_sin_y,
# # #                         x_add = x,
# # #                         y_add = y),
# # #           aes(fill = contour_surface, color = contour_surface),
# # #           alpha = 0.95)+
# # #   ## 56 days area label
# # #   geom_sf_label(data = contour_omicronba5 |> 
# # #                   filter(contour_surface == "56 days")|>
# # #                   rotate_data(shear_cos_x = shear_cos_x,
# # #                               shear_sin_x = shear_sin_x,
# # #                               shear_cos_y = shear_cos_y,
# # #                               shear_sin_y = shear_sin_y,
# # #                               x_add = x,
# # #                               y_add = y),
# # #                 aes(label = area, fill = contour_surface), 
# # #                 size = 3,
# # #                 show.legend = F,
# # #                 position = position_nudge(x = 0,
# # #                                           y = -1e6))+
# # #   colorspace::scale_color_discrete_sequential(palette = "Sunset", 
# # #                                               na.value = "grey70",
# # #                                               rev = F,
# # #                                               name = "Days before national curve peak")+
# # #   colorspace::scale_fill_discrete_sequential(palette = "Sunset",
# # #                                              na.value = "grey70",
# # #                                              rev = F,
# # #                                              name = "Days before national curve peak")+
# # #   guides(color = "none")+
# # #   theme_void()+
# # #   theme(legend.position = "bottom",
# # #         legend.title.position = "top",
# # #         legend.title = element_text(hjust = 0.5))+
# # #   guides(fill = guide_legend(reverse = T))+
# # #   labs(title = "Omicron BA.5  wave")
# # # figs3d_layered
# # # 
# # # figS3_layered <- (((fig4a_layered / fig4b_layered) | (figs3c_layered / figs3d_layered)))+
# # #   plot_layout(guides = 'collect')+
# # #   plot_annotation(tag_levels = 'A')&
# # #   theme(legend.position = "bottom")
# # # figS3_layered
# # # 
# # # ggsave(filename = "img/extra_figures/figS3_layered.png",
# # #        plot = figS3_layered,
# # #        width = 16,
# # #        height = 9,
# # #        dpi = 100)
# # # 
# # # ggsave(filename = "img/extra_figures/figS3_layered.pdf",
# # #        plot = figS3_layered,
# # #        width = 16,
# # #        height = 9,
# # #        dpi = 200)
# # # 
# # # ## Pensar numa métrica que possa dizer algo sobre o mecanismo de como cada uma das ondas se sucedeu, talvez
# # # ## distância média entre os hex bins dentro da superfície sobre o número de hexes. Um valor maior significa um processo menos contínuo e mais fragmentário, processo mais semelhante a um incêndio com diversos focos iniciais, Omicron BA.1. Um valor menor significa um processo mais contínuo de semelhante a um espalhamento, Alpha e Delta.
# # # 
# # # ## filtrar polígonos para cada data, calcular a area da união de todos esses polígonos em cada data, e criar a distribuição da área por data para cada onda
# # # 
