rm(list = ls())
gc()

## function to plot alphas
plt_preomicron <- \(week, infections, alphas, hexes, plot_img = TRUE) {
  
  ## Stopping if not dataset and/or hexes
  if(missing(infections) | missing(alphas) | missing(hexes) | missing(week)){
    stop("'week', 'infections', 'alphas'  and/or 'hexes' is missing")
  }
  
  # Filter the data for the current week
  infections <- infections[infections$date == week, ]
  alphas <- alphas[alphas$date_week == week, ]
  
  # Create a ggplot2 plot
  # plot_infections <- ggplot()+
  #   geom_sf(data = hexes |>
  #             filter(as.integer(hexid) < 7662) |>
  #             sf::st_as_sf())+
  #   geom_sf(data = infections,
  #           aes(fill = infectionsPC),
  #           size = 3)+
  #   scale_fill_viridis_c(option = "rocket",
  #                        name = "Infections PC",
  #                        breaks = seq(0,1000, 100),
  #                        labels = c(seq(0,900, 100), "1000+"),
  #                        limits = c(0,1000),
  #                        oob = scales::squish,
  #                        guide = metR::guide_colorstrip(title.position = "left",
  #                                                       title.hjust = 0.5,
  #                                                       barheight = grid::unit(10, "cm")))+
  #   theme_void()+
  #   theme(axis.title = element_blank(),
  #         legend.position = "left",
  #         legend.title = element_text(angle = 90))+
  #   # labs(title = "Infections")+
  #   guides(alpha = "none")+
  #   coord_sf(xlim = st_bbox(hexes)[c(1,3)],
  #            ylim = st_bbox(hexes)[c(2,4)])
  # # plot_infections
  
  plot_alphas <- ggplot()+
    geom_sf(data = hexes)+
    geom_sf(data = infections,
            aes(fill = infectionsPC),
            alpha = 0.5,
            size = 3)+
    geom_segment(data = alphas,
            aes(color = alpha,
                x = start_coord_x, xend = end_coord_x,
                y = start_coord_x, yend = end_coord_x),
            arrow = grid::arrow(length = unit(x = .5, 
                                              units = "mm"), 
                                type = "closed"),
            alpha = 0.5,
            size = 3)+
    scale_color_viridis_c(option = "rocket",
                          name = "Alphas", 
                          direction = -1,
                          breaks = seq(0,1200, 200),
                          labels = c(seq(0,1000, 200), "1200+"),
                          limits = c(0,1200),
                          oob = scales::squish,
                          guide = metR::guide_colorstrip(title.position = "right",
                                                         title.hjust = 0.5,
                                                         barheight = grid::unit(12, "cm")))+
    scale_fill_viridis_c(option = "rocket",
                         name = "Infections PC",
                         direction = -1,
                         breaks = seq(0, 1000, 100),
                         labels = c(seq(0, 900, 100), "1000+"),
                         limits = c(0,1000),
                         oob = scales::squish,
                         guide = metR::guide_colorstrip(title.position = "left",
                                                        title.hjust = 0.5,
                                                        barheight = grid::unit(12, "cm")))+
    scale_alpha()+
    theme_void()+
    guides(alpha = "none",
           color = guide_legend(order = 1),
           fill = guide_legend(order = 2))+
    theme(axis.title = element_blank(), 
          legend.position = "left",
          legend.box = "vertical",
          legend.title = element_text(angle = 90))+
    # labs(title = "Alphas")+
    coord_sf(xlim = st_bbox(hexes)[c(1,3)],
             ylim = st_bbox(hexes)[c(2,4)])
  # plot_alphas
  
  library(patchwork)
  # plot <- (plot_infections | plot_alphas)
  plot <- plot_alphas
  
  if(plot_img){
    plot <- plot &
      labs(tag = paste("Week:", week))
  }
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9)
  
  # Return the temporary file path
  return(tmp_file)
}

# ## Loading auxiliary functions
# source("scripts/plot_functions.R")
# source("scripts/canny_functions.R")

library(tidyverse)

## Loading databases
hexes <- sf::st_read("data-products/geo-hexes/hexes.geojson")

hexes_centroid <- sf::st_centroid(hexes)

hexes_boundary <- sf::st_union(hexes)

hexid_observations <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicronNEW.csv")

hexes_observations <- hexes |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(hexid_observations)

# ## Pre-Omicron
features_preomicron <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_preomicron_mainNEW.geojson") |>
  # dplyr::filter(date_week == max(date_week)) |>
  dplyr::filter(alpha == max(alpha, na.rm = T),
                .by = c(j, date_week))

# features_rotated <- features_preomicron
# featuresg <- st_geometry(features_rotated)
# 
# centroids <- st_centroid(featuresg)
# rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)
# featuresg2 <- (featuresg - centroids)*rot(pi/2)*(1.75) + centroids
# 
# st_geometry(features_rotated) <- featuresg2
# 
# new_features <- st_cast(x = features_rotated, 
#                         to = "MULTILINESTRING") |> 
#   st_set_crs(st_crs(features_preomicron))
# 
# new_features2 <- st_cast(x = features_rotated, 
#                          to = "MULTILINESTRING") |> 
#   st_set_crs(st_crs(features_preomicron))

## Pre-Omicron weeks
weeks <- sort(unique(features_preomicron$date_week))

hexes_plt <- hexes |> 
  filter(as.integer(hexid) < 7662)

hexes_plt_boundary <- st_boundary(st_union(hexes_plt))

hexes_observations <- hexes_observations |> 
  sf::st_as_sf()

# samples_features <- samples_features |> 
#   filter(alpha >= 100)

frame_files <- lapply(weeks[1:20], 
                      plt_preomicron, 
                      hexes_observations, 
                      features_preomicron |> 
                        filter(alpha>0), 
                      hexes_plt, 
                      FALSE)

frame_files <- frame_files |> 
  unlist()

animation <- magick::image_animate(magick::image_read(frame_files), 
                                   fps = 4, 
                                   optimize = T)

animation

# hexes_observations |> 
#   filter(as.integer(hexid) < 7662,
#          date == max(date, na.rm = T)) |> 
#   ggplot()+
#   geom_sf(aes(fill = infectionsPC))+
#   scale_fill_viridis_c(option = "rocket",
#                        name = "Infections PC",
#                        breaks = seq(0, 70, 10),
#                        labels = seq(0,70, 10),
#                        limits = c(0,70),
#                        oob = scales::squish,
#                        guide = metR::guide_colorstrip(title.position = "left",
#                                                       title.hjust = 0.5,
#                                                       barheight = grid::unit(12, "cm")))
