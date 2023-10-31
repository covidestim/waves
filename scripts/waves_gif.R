rm(list = ls())
gc()

library(tidyverse)
library(sf)
library(magick)
library(metR)

features_from_wkt <- st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt.geojson")

weeks <- sort(unique(features_from_wkt$date))

us_map <- tigris::states(cb = T) |> 
  tigris::shift_geometry() |> 
  filter(GEOID <= 56, 
         !NAME %in% c("Alaska", "Hawaii"))

ggplot()+
  geom_sf(data = us_map)+
  theme_dark()

# range_values<-pretty(features_from_wkt$infectionsPC_avg, n = 10)

# test <- data.table(cbind(j = features_from_wkt$j, 
#                          week = features_from_wkt$week, 
#                          start_coord_x = features_from_wkt$start_coord_x,
#                          end_coord_x = features_from_wkt$end_coord_x,
#                          start_coord_y = features_from_wkt$start_coord_y,
#                          end_coord_y = features_from_wkt$end_coord_y,
#                          infectionsPC = features_from_wkt$infectionsPC_avg))
# features_from_wkt |>
#   filter(week == week) |>
#   ggplot() +
#   geom_sf(data = us_map)+
#   geom_sf(aes(col = infectionsPC_avg))+
#   metR::scale_color_divergent(midpoint = 400,
#                               guide = "colorbar",
#                               name = "average Infections per capita",
#                               breaks = range_values, 
#                               limits = c(0, 1000))+
#   theme_dark() +
#   # labs(title = paste("Week:", week))+
#   theme(legend.position = "top")
# 
# # test <- test[, c("t.dx", "t.dy") := c(("start_coord_x" - "end_coord_x"), 
# #                                       ("start_coord_y" - "end_coord_y")), 
# #             by = j]

# define the arrow length in decimal degrees
arrow_length <- 1

week <- weeks[1]

plot_and_save_frame <- function(week) {
  # Filter the data for the current week
  data <- features_from_wkt[features_from_wkt$date == week, ]
  
  # Create a ggplot2 plot
  plot <- ggplot() +
    geom_sf(data = us_map)+
    geom_sf(data= data, 
            aes(col = infectionsPC_avg)) +
    # extract coordinates
    # stat_sf_coordinates() +
    # draw arrows with orientation from attribute
    # geom_segment(mapping = aes(geometry = geometry, 
    #                            col = infectionsPC_avg,
    #                            # size = infectionsPC_avg,
    #                            x = start_coord_x, 
    #                            y = start_coord_y, 
    #                            xend = end_coord_x + arrow_length, 
    #                            yend = end_coord_y + arrow_length), 
    #              arrow = arrow(angle = 5, ends = "last", type = "open")) +
    # # draw circle markers
    # geom_sf(stat = "sf_coordinates", 
    #         mapping = aes(geometry = geometry, 
    #                       x = after_stat(x), 
    #                       y = after_stat(y)), 
    #         size = 4, 
    #         shape = 21, 
    #         fill = "white") +
    scale_color_viridis_c(option = "turbo",
                          # midpoint = 600, 
                          name = "average Infections per capita",
                          breaks = seq(0,400,50), 
                          limits = c(0,400),  
                          oob = scales::squish,
                          guide = guide_colorstrip(title.position = "top",
                                                   title.hjust = 0.5,
                                                   barwidth = grid::unit(12, "cm"))) +
    theme_void() +
    labs(title = paste("Week:", week))+
    theme(legend.position = "top")
  # plot
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9)
  
  # Return the temporary file path
  return(tmp_file)
}

frame_files <- lapply(weeks, plot_and_save_frame)
frame_files <- frame_files |> 
  unlist()

animation <- image_animate(image_read(frame_files), 
                           fps = 5)

animation

# Specify the output file path
output_file <- "animation2.gif"

# Save the GIF animation
image_write(animation, output_file)

