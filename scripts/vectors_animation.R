rm(list = ls())
gc()

hexes <- st_read("data-products/geo-hexes/hexes.geojson") |> 
  st_as_sf(crs = 4326)

features_from_wkt <- st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt.geojson") |> 
  st_as_sf(crs = 4326)

## To-do: Map the vectors with the counties and see if the centroids point to counties centroids

# arrows_length <- mean(st_length(features_from_wkt$geography), na.rm = T)

# ## Plot alphas with hexes
hexes_arrow <- hexes |>
  ggplot()+
  geom_sf(fill = "transparent")+
  # geom_sf(data = features_from_wkt |>
  #           filter(date_week == max(date_week)),
  #         aes(color = infectionsPC_avg))+
  geom_segment(data = features_from_wkt |>
              filter(date_week == max(date_week)),
            aes(x = start_coord_x, xend = end_coord_x,
                y = start_coord_y, yend = end_coord_y,
                color = infectionsPC_avg),
            arrow = grid::arrow(length = grid::unit(x = 1.2, "mm")))+
  theme_void()+
  scale_y_continuous(limits = c(23,50))+
  scale_x_continuous(limits = c(-130, -65))+
  scale_color_viridis_c(option = "turbo",
                        # midpoint = 600,
                        name = "average Infections per capita",
                        breaks = seq(0,400,50),
                        limits = c(0,400),
                        oob = scales::squish,
                        guide = metR::guide_colorstrip(title.position = "top",
                                                 title.hjust = 0.5,
                                                 barwidth = grid::unit(12, "cm")))+
  theme(legend.position = "top")
hexes_arrow

week <- max(features_from_wkt$date_week, na.rm = T)

plot_and_save_frame <- function(week) {
  # Filter the data for the current week
  data <- features_from_wkt[features_from_wkt$date_week == week, ]
  
  # Create a ggplot2 plot
  plot <- ggplot()+
    geom_sf(data = hexes, 
            fill = "transparent")+
    # geom_sf(data = features_from_wkt |>
    #           filter(date_week == max(date_week)),
    #         aes(color = infectionsPC_avg))+
    geom_segment(data = data,
                 aes(x = start_coord_x, xend = end_coord_x,
                     y = start_coord_y, yend = end_coord_y,
                     color = infectionsPC_avg),
                 arrow = grid::arrow(length = grid::unit(x = 1.2, "mm")))+
    theme_void()+
    scale_y_continuous(limits = c(23,50))+
    scale_x_continuous(limits = c(-130, -65))+
    scale_color_viridis_c(option = "turbo",
                          # midpoint = 600,
                          name = "average Infections per capita",
                          breaks = seq(0,400,50),
                          limits = c(0,400),
                          oob = scales::squish,
                          guide = metR::guide_colorstrip(title.position = "top",
                                                         title.hjust = 0.5,
                                                         barwidth = grid::unit(12, "cm")))+
    # labs(title = paste("Week:", date_week))+
    theme(legend.position = "top")
  plot
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9)
  
  # Return the temporary file path
  return(tmp_file)
}

weeks <- unique(features_from_wkt$date_week)

frame_files <- lapply(weeks[1:20], plot_and_save_frame)
frame_files <- frame_files |> 
  unlist()

animation <- magick::image_animate(magick::image_read(frame_files), 
                           fps = 4, 
                           optimize = T)

animation

# Specify the output file path
output_file <- "weekly_model.gif"

# Save the GIF animation
magick::image_write(animation, output_file)
