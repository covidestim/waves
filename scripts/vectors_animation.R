rm(list = ls())
gc()

## Loading auxiliary functions
source("scripts/plot_functions.R")

## Loading databases
hexes <- sf::st_read("data-products/geo-hexes/hexes.geojson")

hexes_boundary <- sf::st_union(hexes)

features_preomicron <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt.geojson")|> 
  # filtering to only show alphas >0, 
  # and the maximum value between a_ij and a_ji
  dplyr::filter(alpha>0, alpha == pmax(alpha),
                .by = c(j,date_week))

features_omicronera <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt_omicronera.geojson")|> 
  # filtering to only show alphas >0, 
  # and the maximum value between a_ij and a_ji
  dplyr::filter(alpha>0, alpha == pmax(alpha),
                .by = c(j,date_week))

library(tidyverse)

# ## Plot alphas with hexes
hexes_arrow <- hexes_boundary |>
  ggplot()+
  geom_sf(fill = "transparent")+
  geom_segment(data = features_preomicron |>
              filter(date_week == max(date_week)),
            aes(x = start_coord_x, xend = end_coord_x,
                y = start_coord_y, yend = end_coord_y,
                color = infectionsPC_avg),
            arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  theme_void()+
  scale_y_continuous(limits = c(24,50))+
  scale_x_continuous(limits = c(-124, -66))+
  scale_color_viridis_c(option = "turbo",
                        # midpoint = 600,
                        name = "average Infections per capita",
                        breaks = seq(0,500, 50),
                        labels = c(seq(0,450, 50), "500+"),
                        limits = c(0,500),
                        oob = scales::squish,
                        guide = metR::guide_colorstrip(title.position = "top",
                                                 title.hjust = 0.5,
                                                 barwidth = grid::unit(12, "cm")))+
  theme(legend.position = "top")
hexes_arrow

## Pre-Omicron weeks
weeks <- sort(unique(features_preomicron$date_week))

frame_files <- lapply(weeks, plt_preomicron, features_preomicron, hexes_boundary, FALSE)
frame_files <- frame_files |> 
  unlist()

animation <- magick::image_animate(magick::image_read(frame_files), 
                           fps = 4, 
                           optimize = T)

animation

# Specify the output file path
output_file <- "img/weekly_model_preomicron.gif"

# Save the GIF animation
magick::image_write(animation, output_file)

## Omicron era vectors
# ## Plot alphas with hexes
hexes_arrow <- hexes |>
  ggplot()+
  geom_sf(fill = "transparent")+
  geom_segment(data = features_omicronera |>
                 filter(date_week == "2022-01-20"),
               aes(x = start_coord_x, xend = end_coord_x,
                   y = start_coord_y, yend = end_coord_y,
                   color = infectionsPC_avg),
               arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  theme_void()+
  scale_y_continuous(limits = c(24,50))+
  scale_x_continuous(limits = c(-124, -66))+
  scale_color_viridis_c(option = "turbo",
                        # midpoint = 600,
                        name = "average Infections per capita",
                        breaks = seq(0,10000, 1000),
                        labels = c(seq(0,9000, 1000), "10.000+"),
                        limits = c(0,10000),
                        oob = scales::squish,
                        guide = metR::guide_colorstrip(title.position = "top",
                                                       title.hjust = 0.5,
                                                       barwidth = grid::unit(12, "cm")))+
  theme(legend.position = "top")
hexes_arrow

weeks <- sort(unique(features_omicronera$date_week))

frame_files <- lapply(weeks, plt_omicronera, features_omicronera, hexes, FALSE)
frame_files <- frame_files |> 
  unlist()

animation <- magick::image_animate(magick::image_read(frame_files), 
                                   fps = 4, 
                                   optimize = T)

animation

# Specify the output file path
output_file <- "img/weekly_model_omicronera.gif"

# Save the GIF animation
magick::image_write(animation, output_file)
  
## Edge detection

library(magick)

image <- image_read(frame_files[106])
image

res = edge_detection(image[,,1:3], 
                     method = 'Frei_chen', 
                     conv_mode = 'same')

plot(raster::as.raster(res))
  
