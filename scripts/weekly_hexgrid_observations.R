rm(list = ls())
gc()

library(tidyverse)
library(vroom)
library(sf)

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

dates <- unique(hexgrid_preomicron$date)
dates <- dates[seq(1,628, 7)]

hexgrid_preomicron <- hexgrid_preomicron |> 
  filter(date %in% dates)

st_write(obj = hexgrid_preomicron,
         dsn = "data-products/hexgrid_infections_preomicron_weekly.geojson",
         delete_dsn = T,
         delete_layer = T)

# geojson::geo_write(geojsonio::(hexgrid_preomicron), 
#       file = gzfile("data-products/hexgrid_infections_preomicron_weekly.geojson.xz"))

## Omicron-era
hexgrid_omicronera <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronera.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  # select(-geometry) |>
  # mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

st_write(obj = hexgrid_omicronera,
         dsn = "data-products/hexgrid_infections_omicronera_weekly.geojson",
         delete_dsn = T,
         delete_layer = T)


## Joinning everything
hexgrid_preomicron <- hexgrid_preomicron |> 
  select(-population) |> 
  st_drop_geometry()

hexgrid_omicronera <- hexgrid_omicronera |> 
  st_drop_geometry()

hexgrid_full <- bind_rows(hexgrid_preomicron, hexgrid_omicronera) |> 
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf() |> 
  sf::st_transform(crs=26915)

hexgrid_full |> 
  group_by(date) |> 
  summarise(infectionsPC = sum(infectionsPC)) |> 
  ggplot(aes(x=sort(date), y = infectionsPC))+
  geom_line()

## Breakdowns of each peaks
breaks_plt <- seq(0,2500, 250)
labels_plt <- c(seq(0,2250, 250), '2,500+')
limits_plt <- c(0,2500)
color_option <- "magma"
delta_peak_date <- as.Date("2021-08-26")
omicron_peak_date <- as.Date("2022-01-20")

## function to plot hexes with infections
plt_fun <- \(week, is.omicron = FALSE, infections, hexes, plot_img = TRUE) {
  
  # Filter the data for the current week
  infections <- infections |> 
    filter(date == week)
  
  plot_alphas <- ggplot()+
    geom_sf(data = hexes,
            fill = "transparent")+
    geom_sf(data = infections,
            aes(fill = infectionsPC),
            alpha = 0.75,
            size = 3)+
    scale_fill_viridis_b(option = color_option,
                         name = "Estimated Infections/100k/week",
                         direction = -1,
                         breaks = breaks_plt,
                         labels = labels_plt,
                         limits = limits_plt,
    )+
    theme_minimal()+
    theme(legend.title.position = "top",
          legend.location = "plot",
          legend.position = "bottom", 
          legend.key.width = grid::unit(3, "cm"))
  
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

weeks <- sort(unique(hexgrid_full$date))

hexes <- hexes |>
  filter(as.integer(hexid)<7662) |> 
  st_transform(crs = 26915) |> 
  sf::st_as_sf()

frame_files <- lapply(weeks, 
                      plt_fun, 
                      TRUE,
                      st_transform(hexgrid_full, 
                                   crs = 26915), 
                      st_transform(hexes, 
                                   crs = 26915),
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation2 <- magick::image_animate(magick::image_read(frame_files), 
                                    # fps = 3, 
                                    optimize = T)
animation2

# Specify the output file path
output_file <- "img/weekly_hex_complete.gif"

# Save the GIF animation
magick::image_write(animation2, output_file)
