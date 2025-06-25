rm(list = ls())
gc()

library(tidyverse)
library(vroom)
library(sf)

# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

## Hexes, Population
hexpop <- sf::st_read("data-products/geo-hexes/meta_population/hexid-population_new.shp")

## 
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

## Pre-Omicron
hexgrid_preomicron <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron_meta30m.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

# hexgrid_test <- hexgrid_preomicron |> 
#   filter(date %in% seq.Date(to = delta_peak_date, 
#                             from = delta_peak_date-63, length.out = 64))
# 
# sf::st_write(obj = hexgrid_test,
#              dsn = "data-products/hexgrid-infections-delta.geojson",
#              delete_dsn = T)

# ## Omicron-era
# hexgrid_omicronera <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronera.csv") |>
#   mutate(hexid = as.character(hexid),
#          date = as.Date(date)) |>
#   # select(-geometry) |>
#   # mutate(infectionsPC = (infections/population)*1e5) |>
#   filter(infectionsPC >= 1) |>
#   left_join(hexes, by = "hexid") |>
#   sf::st_as_sf()

## Breakdowns of each peaks
breaks_plt <- seq(0,500, 100)
labels_plt <- c(seq(0,400, 100), '500+')
limits_plt <- c(0,500)
color_option <- "magma"

hexgrid_infections_test <- hexObservationsAllSF |> 
  filter(date == delta_peak) |>
  sf::st_transform(crs = 26915) |> 
  sf::st_as_sf()

hexgrid_preomicron <- hexObservationsAllSF

plt_peak_delta <- ggplot()+
  geom_sf(data = hexgrid_infections_test,
          mapping = aes(fill = log10(infections+1)))+
  # geom_sf(data = hexgrid_infections_test |>
  #           filter(infectionsPC == 0),
  #         mapping = aes(),
  #         fill = "grey70")+
  # scale_fill_viridis_b(option = color_option,
  #                      name = "Estimated Infections/100k/week",
  #                      direction = -1,
  #                      breaks = breaks_plt,
  #                      labels = labels_plt,
  #                      limits = limits_plt,
  #                      )+
  # khroma::scale_fill_batlow(
  #   name = "Estimated Infections/100k/week",
  #   reverse = T,
  #   breaks = scales::breaks_extended(n = 7),
  #   # labels = scales::label_math(),
  #   limits = c(0,3800),
  #   na.value = "transparent"
  # )+
  scico::scale_fill_scico(name = "Estimated Infections/100k/day",
                          palette = "lipari",
                          na.value = "grey70",
                          breaks = scales::breaks_extended(n=5),
                          limits = c(0,4),
                          direction = 1)+
  theme_minimal()+
  theme(legend.title.position = "top",
        legend.location = "plot",
        legend.position = "bottom", 
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = delta_peak,
       subtitle = "Delta wave peak")
plt_peak_delta

## function to plot hexes with infections
plt_fun <- \(week, is.omicron = FALSE, infections, hexes, plot_img = TRUE) {
  
  # Filter the data for the current week
  infections <- infections |> 
    filter(date == week)
    
    plot_alphas <- ggplot()+
      geom_sf(data = hexes,
              fill = "transparent")+
      geom_sf(data = infections,
              aes(fill = log10(infections+1)))+
      # scale_fill_viridis_b(option = color_option,
      #                      name = "Estimated Infections/100k/week",
      #                      direction = -1,
      #                      breaks = breaks_plt,
      #                      labels = labels_plt,
      #                      limits = limits_plt,
      #                      )+
      # khroma::scale_fill_batlow(
      #   name = "Estimated Infections/100k/day",
      #   reverse = T,
      #   breaks = scales::breaks_extended(n = 7),
      #   # labels = scales::label_math(),
      #   limits = c(0,3800),
      #   na.value = "transparent"
      # )+
      scico::scale_fill_scico(name = "Estimated Infections/100k/day",
                              palette = "lipari",
                              breaks = scales::breaks_extended(n=5),
                              limits = c(0,4), 
                              direction = -1)+
      theme_minimal()+
      theme(legend.title.position = "top",
            # panel.background = element_rect(fill = "black"), ## Uncomment to not have background color
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

weeks <- sort(unique(hexgrid_preomicron$date))

hexes <- hexes |>
  filter(as.integer(hexid)<7662) |> 
  st_transform(crs = 26915) |> 
  sf::st_as_sf()

frame_files <- lapply(na.omit(weeks[seq(1,649,7)]), 
                      plt_fun, 
                      TRUE,
                      st_transform(hexgrid_preomicron, 
                                   crs = 26915), 
                      st_transform(hexes, 
                                   crs = 26915),
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation2 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)
animation2

# Specify the output file path
output_file <- "img/weekly_hex_preomicron_meta30m_lipari.gif"

# Save the GIF animation
magick::image_write(animation2, output_file)

## Gif to Omicron-era
weeks <- sort(unique(hexgrid_omicronera$date))

frame_files <- lapply(weeks, 
                      plt_fun, 
                      TRUE,
                      st_transform(hexgrid_omicronera, 
                                   crs = 26915), 
                      st_transform(hexes, 
                                   crs = 26915),
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation3 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)
animation3

# Specify the output file path
output_file <- "img/weekly_hex_omicronera.gif"

# Save the GIF animation
magick::image_write(animation3, output_file)

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

animation4 <- magick::image_animate(magick::image_read(frame_files), 
                                    # fps = 3, 
                                    optimize = T)
animation4

# Specify the output file path
output_file <- "img/weekly_hex_complete.gif"

# Save the GIF animation
magick::image_write(animation4, output_file)

## gif for the infections on counties shapes
observationFips_preomicron <- st_read("data-products/geo-hexes/observations_preomicron.shp")

preomicron_testDate <- "2020-07-26"

observationFips_omicronera <- st_read("data-products/geo-hexes/observations_omicronera.shp")

omicronera_testDate <- "2022-01-20"

## US map

us_states <- tigris::counties(cb = T) |> 
  filter(!STATEFP %in% c(
    "02", "60", "03", "81", "07", "64",
    "14", "66", "84", "86", "67", "89",
    "68", "71", "76", "69", "70", "95",
    "43", "72", "74", "78", "79", "15", "11"
  )) |> 
  tigris::shift_geometry()
# 
# ### Explore with a plot
# ggplot() + 
#   geom_sf(observationFips_preomicron %>% filter(date == preomicron_testDate),
#           mapping=aes(fill=infctPC)) +
#   theme_minimal() +
#   scale_fill_gradient(low = "lightgreen", high = "deeppink2")

## function to plot alphas
plt_preomicron <- \(week, infections, hexes, plot_img = TRUE) {
  
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

## Gif on county polygons maps - Pre-Omicron
weeks <- sort(unique(observationFips_preomicron$date))

frame_files <- lapply(weeks[seq(1,649, 7)], 
                      plt_preomicron, 
                      observationFips_preomicron |> 
                        rename(infectionsPC = infctPC), 
                      us_states,
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation4 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)

animation4

# Specify the output file path
output_file <- "img/weekly_fips_preomicron.gif"

# Save the GIF animation
magick::image_write(animation4, output_file)

## Gif on county polygons maps - Omicron Era
weeks <- sort(unique(observationFips_omicronera$date))

frame_files <- lapply(weeks[1:50], 
                      plt_preomicron, 
                      observationFips_omicronera |> 
                        rename(infectionsPC = infctPC), 
                      us_states,
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation5 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)

animation5

# Specify the output file path
output_file <- "img/weekly_fips_omicronera.gif"

# Save the GIF animation
magick::image_write(animation5, output_file)

# ## Pre-Omicron
features_preomicron <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_preomicron_mainNEW.geojson") |>
  # dplyr::filter(date_week == max(date_week)) |>
  dplyr::filter(alpha == max(alpha, na.rm = T),
                alpha > 0,
                .by = c(j, date_week))

# features_preomicron <- geojsonsf::geojson_sf("data-products/geo-hexes/vectors/vectors_weekly_preomicron_mainNEW.geojson")

# ## Omicron-era
features_omicronera <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_omicronera_mainNEW.geojson") |>
  # dplyr::filter(date_week == max(date_week)) |>
  dplyr::filter(alpha == max(alpha, na.rm = T),
                alpha > 0,
                .by = c(j, date_week))

# features_omicronera <- geojsonsf::geojson_sf("data-products/geo-hexes/vectors/vectors_weekly_omicronera_mainNEW.geojson")

## statistics over the vectors
## function to plot alphas
plt_vectors <- \(week, alphas, hexes, plot_img = TRUE) {
  
  # Filter the data for the current week
  alphas <- alphas |> 
    filter(date_week == week)
  
  plot_alphas <- ggplot()+
    geom_sf(data = hexes,
            fill = "transparent")+
    ggquiver::geom_quiver(data = alphas,
                          aes(x = lon, y = lat,
                              u = -delta_lon, v = -delta_lat, 
                              color = alpha,
                              alpha = alpha))+
    scale_color_viridis_b(option = "inferno",
                          name = "Alpha value",
                          # direction = -1,
                          breaks = seq(0, 1400, 100),
                          labels = c(seq(0, 1300, 100), "1,400+"),
                          limits = c(0,1400),
                          oob = scales::squish,
                          guide = metR::guide_colorstrip(title.position = "left",
                                                         title.hjust = 0.5,
                                                         barheight = grid::unit(12, "cm")))+
    ggdark::dark_theme_minimal()+
    theme(legend.title = element_text(angle = 90))+
    guides(alpha = "none")+
    coord_sf(xlim = sf::st_bbox(hexes)[c(1,3)],
             ylim = sf::st_bbox(hexes)[c(2,4)])
  
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

library(ggquiver)
features_delta_preomicron <- features_preomicron |> 
  mutate(lon = start_coord_x, lat = start_coord_y,
         delta_lon = end_coord_x - lon, delta_lat = end_coord_y - lat,
         .by = date_week) 
# |>
#   sf::st_transform(crs = 26915)

features_delta_omicronera <- features_omicronera |> 
  mutate(lon = start_coord_x, lat = start_coord_y,
         delta_lon = end_coord_x - lon, delta_lat = end_coord_y - lat,
         .by = date_week) 
# |>
#   sf::st_transform(crs = 26915)


edgelist <- vroom::vroom("data-products/geo-hexes/hexid-neighbors.csv")

features_test <- features_delta_preomicron |> 
  sf::st_drop_geometry() |> 
  data.table::setDT()

features <- features_test |> 
  # dplyr::filter(date_week %in% c(as.Date("2020-04-23"),
  #                                as.Date("2020-04-30"),
  #                                as.Date("2020-05-07"))) |>
  dplyr::filter(date_week == as.Date("2020-04-23")) |>
  dplyr::filter(j %in% c(1,5,6,20,21,33)) |>
  # dplyr::mutate(time = as.numeric(date_week)) |> 
  dplyr::select(lon, lat, delta_lon, delta_lat, date_week, alpha)

ggplot()+
  geom_sf(data = hexes |>
            dplyr::filter(hexid %in% c(1,5,6,20,21,33)), 
          mapping = aes(),
          fill = "transparent")+
  geom_vector(features, 
              mapping = aes(lon, lat, dx = delta_lon, dy = delta_lat,
                            color = alpha))+
  scale_color_gradient(low = "thistle3", high = "deeppink1")


# xy.outs <- hexes |> 
#   dplyr::filter(hexid %in% c(1,5,6,20,21,33)) |>
#   mutate(X = sf::st_coordinates(sf::st_centroid(geometry))[,1],
#          Y = sf::st_coordinates(sf::st_centroid(geometry))[,2])

trajectories <- metR::GeostrophicWind(features$alpha, features$lon, features$lat)

ggplot(trajectories, aes(lon, lat)) +
  geom_path(aes(group = id))

library(metR)
ggplot() +
  # geom_sf(data = hexes |> 
  #           sf::st_transform(crs = 26915),
  #         fill = "transparent")+
  geom_segment(data = features_delta |> 
                 filter(date_week == alpha_peak_date) |> 
                 sf::st_transform(crs = 26915),
               mapping = aes(x = start_coord_x, 
                             y = start_coord_y, 
                             xend = end_coord_x, 
                             yend = end_coord_y,
                             color = alpha,
                             alpha = alpha),
               arrow = arrow(length = unit(0.1,"cm")))

ggplot(data = features_delta_omicronera |>
         # sf::st_transform(crs = 26915) |>
         filter(date_week == omicron_peak_date),
       aes(start_coord_x, start_coord_y, 
           v = -(end_coord_x-start_coord_x), 
           u = -(end_coord_y-start_coord_y),
           color = alpha,
           alpha = alpha))+
  geom_sf(data = hexes |> 
            filter(as.integer(hexid) < 7662),
          fill = "transparent", 
          # color = "grey90",
          inherit.aes = FALSE)+
  ggquiver::geom_quiver(vecsize = 0)+
  guides(alpha = "none")+
  ggdark::dark_theme_minimal()+
  # scale_color_viridis_c(option = "inferno",
  #                       name = "Alpha value",
  #                       # direction = -1,
  #                       breaks = seq(0, 1800, 200),
  #                       labels = c(seq(0, 1600, 200), "1,800+"),
  #                       limits = c(0,1800),
  #                       oob = scales::squish,
  #                       guide = metR::guide_colorstrip(title.position = "left",
  #                                                      title.hjust = 0.5,
  #                                                      barheight = grid::unit(12, "cm")))
  MoMAColors::scale_color_moma_c(palette_name = "vonHeyl",
                                 direction = -1,
                                 breaks = seq(0, 1800, 200),
                                 labels = c(seq(0, 1600, 200), "1,800+"),
                                 limits = c(0,1800),
                                 oob = scales::squish,
                                 guide = metR::guide_colorstrip(title.position = "left",
                                                                title.hjust = 0.5,
                                                                barheight = grid::unit(12, "cm")))


ggplot()+
  geom_streamline(data = features_delta_preomicron |> 
                    filter(date_week == alpha_peak_date),
                  aes(lon, lat,
                      dx = -delta_lon,
                      dy = -delta_lat,
                      color = alpha))+
  ggdark::dark_theme_minimal()

hexes <- hexes |> 
  filter(as.integer(hexid) < 7662)

weeks <- sort(unique(features_delta_preomicron$date))

frame_files <- lapply(weeks, 
                      plt_vectors, 
                      features_delta_preomicron,
                      hexes,
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation4 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)

animation4

dataset <- "preomicron"

## If reruns need to be made from a saved df, read it as a data.frame and use the following code to rebuilty it as a list to rerun at the above for loop
CAR_df <- vroom::vroom(paste0("data-products/tsa_",
                                    dataset, 
                                    ".csv"))

dataset <- "omicronera"

CAR_df <- vroom::vroom(paste0("data-products/tsa_",
                              dataset, 
                              ".csv"))

## Returning the data.frame into a list format
CAR_list <- CAR_df |> 
  group_split(date) |> 
  as.list()

## Turning the CAR_df mean and sd columns into exponentiated values if the log transform was needed, usually at the Omicron-era dataset
CAR_df <- bind_rows(CAR_list) |> 
  ## To the Omicron dataset we rescaled the infectionsPC to 1e5, now need to rescale back
  mutate(mean = mean*1e5,
         sd = sd*1e5)

## Breakdowns of each peaks, Pre-Omicron
breaks_plt <- c(0,seq(150,300, 20))
labels_plt <- c("150< ",seq(150,280, 20), ' >300')
limits_plt <- c(0,350)
color_option <- "magma"

## Breakdowns of each peaks, Omicron-era
breaks_plt <- c(seq(0, 5000, 500))
labels_plt <- c("500< ",seq(500, 4500, 500), ' >5,000')
limits_plt <- c(0,36000)
color_option <- "magma"

## function to plot hexes with infections
plt_fun <- \(week, hexes, CAR_df, plot_img = TRUE) {
  
  plot_alphas <- ggplot() +
    geom_sf(data = hexes |> 
              mutate(hexid = as.character(hexid)) |> 
              cbind(predCAR_B = CAR_df |> filter(date == week) |> pull(var = "mean")) |> 
              st_transform(crs = 26915),
            aes(fill = predCAR_B))+
    scale_fill_viridis_b(option = color_option,
                         # name = "Estimated Infections/1000/week",
                         direction = -1,
                         breaks = breaks_plt,
                         labels = labels_plt,
                         limits = limits_plt,
    )+
    # scale_fill_viridis_b(option = color_option,
    #                      n.breaks = 10,
    #                      direction = -1)+
    theme_minimal()+
    guides(fill = guide_bins(title = "Trend surface estimated infections per capita/100k",
                             title.position = "top",
                             title.vjust = 0.5))+
    theme(legend.position = "bottom",
          legend.title.position = "top",
          legend.key.width = grid::unit(1, "cm"))+
    coord_sf(xlim = st_bbox(hexes |> 
                              st_transform(crs = 26915))[c(1,3)],
             ylim = st_bbox(hexes |> 
                              st_transform(crs = 26915))[c(2,4)])
  
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

weeks <- sort(unique(CAR_df$date))

## Size of gifs, in days

size <- 63

weeks_alpha <- seq.Date(from = (alpha_peak - size),
                        to = (alpha_peak + size),
                        by = "week")

weeks_delta <- seq.Date(from = (delta_peak - size),
                        to = (delta_peak + size),
                        by = "week")

weeks_plt <- weeks[which(weeks %in% weeks_delta)]

hexes <- hexes |> 
  filter(as.integer(hexid) < 7662)

frame_files <- lapply(weeks_plt, 
                      plt_fun, 
                      hexes,
                      bind_rows(CAR_list),
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation5 <- magick::image_animate(magick::image_read(frame_files), 
                                   # delay = 1,
                                   fps = 10, ## Change to a bigger number to a higher rate of fps and vice-versa
                                   optimize = T)
animation5

# Specify the output file path, the pattern nomenclature should be: daily_hex_tsa_preomicron.gif, if doing daily gif over some specific periods. DO NOT DO DAILY GIF FOR THE WHOLE PRE-OMICRON PERIOD! Or weekly_hex_tsa_preomicron.gif and weekly_hex_tsa_omicronera.gif for the gif over the entirety of both datasets

output_file <- "img/extra_figures/tsa_alpha_fps10.gif"

# Save the GIF animation
magick::image_write(animation5, output_file)


magick::image_write_gif(image = animation5, 
                        path = output_file,
                        # delay = 1/10 # frame per second inverse
                        )

output_file <- "img/extra_figures/tsa_alpha_fps10.mp4"

magick::image_write_video(image = animation5, 
                        path = output_file,
                        delay = 1/10 # frame per second inverse
)

#


