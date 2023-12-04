rm(list = ls())
gc()

library(tidyverse)
library(sf)

## Reading hex grid
hexes <- st_read("data-products/geo-hexes/hexes.shp") 
# |> 
#   tigris::shift_geometry()

hexes_obversation <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv")

us_states <- tigris::counties(cb = T) |> 
  st_transform(crs = st_crs(hexes)) |> 
  # tigris::shift_geometry() |> 
  filter(!STATE_NAME %in% c("Alaska", 
                            "Hawaii", 
                            "Puerto Rico", 
                            "Guam", 
                            "Commonwealth of the Northern Mariana Islands",
                            "American Samoa",
                            "United States Virgin Islands"))

us_map <- st_union(us_states) |> 
  st_as_sf()

ggplot()+
  geom_sf(data = us_states, 
          aes(color = "Counties"),
          fill = "transparent")+
  geom_sf(data = hexes, 
          aes(color = "Hexes"), 
          fill = "transparent")+
  theme_minimal()+
  

hexes_within_us_border <- st_intersection(hexes, us_map)

hexes_filtered <- hexes |> 
  filter(hexid %in% hexes_within_us_border$hexid)

plot(us_map)
plot(st_union(hexes_filtered), add = TRUE)

hexes_joined <- hexes |> 
  right_join(hexes_obversation |> 
               mutate(hexid = as.character(hexid))) 

dates <- sort(unique(hexes_joined$date))

week <- dates[150]

plot_and_save_frame <- function(week) {
  # Filter the data for the current week
  data <- hexes_joined[hexes_joined$date == week, ] 
  
  # Create a ggplot2 plot
  plot <- ggplot() +
    geom_sf(data = hexes,
            fill = "white")+
    geom_sf(data= data, 
            aes(fill = infectionsPC)) +
    # scale_fill_manual(values = moma.colors(palette_name = "Picasso",
    #                                        type = "continuous"),
    #                   # direction = -1,
    #                   # name = "average Infections per capita",
    #                   # breaks = seq(0,400,50), 
    #                   # limits = c(0,400),  
    #                   # oob = scales::squish,
    #                   # guide = guide_colorsteps(),
    #                   # name = "Infections per Capita", 
    #                   na.value = "white")+
    scale_fill_viridis_c(option = "rocket",
                         direction = -1,
                         na.value = "darkblue",
                         breaks = seq(0,400, 50),
                         limits = c(0,400),
                         oob = scales::squish,
                         guide = metR::guide_colorstrip(title.position = "top",
                                                        title.hjust = 0.5,
                                                        barwidth = grid::unit(12, "cm")))+
    theme_void() +
    theme(legend.position = "top")
  plot
  
  # plot_rt <- ggplot() +
  #   geom_sf(data= data, 
  #           aes(fill = Rt)) +
  #   MoMAColors::scale_fill_moma_c(palette_name = "Doughton", 
  #                                 guide = guide_colorsteps(),
  #                                 direction = 1)+
  #   theme_void() +
  #   theme(legend.position = "top")
  # # plot_rt
  # 
  # library("patchwork")
  # plot <- (plot_infections | plot_rt)
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9)
  
  # Return the temporary file path
  return(tmp_file)
}

frame_files <- lapply(dates[1:150], plot_and_save_frame)
frame_files <- frame_files |> 
  unlist()

library(magick)

animation <- image_animate(image_read(frame_files), 
                           fps = 5)

animation

# Specify the output file path
output_file <- "hexes.gif"

# Save the GIF animation
image_write(animation, output_file)

hexes_joined |> 
  # filter(date == min(date)) |> 
  ggplot()+
  geom_sf(aes(fill = infectionsPC))+
  theme_minimal()

## Reading the monthly model output
alphas_month <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas-reformat.csv",
                             col_types = c("d", "d", "D", "d", "d", "d"))

## Creating unique variable to each hex number
alphas_month <- alphas_month |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))

## Reading the weekly model output
alphas_week <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly-reformat.csv")

alphas_week <- alphas_week |> 
  ## Writing the hexbin code as a 4-digit number
  mutate(hex_i = sprintf("%04d", i),
         hex_j = sprintf("%04d", j)) |> 
  ## Creating a 8-digit hexbin code to identifying uniquely them
  mutate(i_to_j = str_c(hex_i, hex_j),
         j_to_i = str_c(hex_j, hex_i))