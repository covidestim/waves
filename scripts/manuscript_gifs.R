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

cbgpop <- sf::st_read("data-products/geo-hexes/cbg_population.shp")

hexpop <- sf::st_read("data-products/geo-hexes/hexid-population.shp")

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()

new_england_states <- us_states |> 
  dplyr::filter(GEOID %in% c("09","23","25","33","44","50"))

new_england_counties <- tigris::counties(state = c("09","23","25","33","44","50"))

ct_counties <- tigris::counties(state = 09)

cbgpop <- sf::st_transform(cbgpop, crs = 26915)
us_states <- sf::st_transform(us_states, crs = 26915)

us_cbg <- ggplot() + 
  geom_sf(cbgpop,
          mapping = aes(fill=population, color=population)) +
  geom_sf(us_states,
          mapping=aes(),
          color = "deeppink3",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4")+
  scale_color_gradient(low = "thistle1", high = "deeppink4")
us_cbg

new_england <- ggplot() + 
  geom_sf(cbgpop |> 
            filter(substr(GEOID,1,2) %in% c("09","23","25","33","44","50")), 
          mapping = aes(fill=population, color=population)) + 
  geom_sf(new_england_states,
          mapping=aes(),
          color = "deeppink3",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4")+
  scale_color_gradient(low = "thistle1", high = "deeppink4")
new_england

ct <- ggplot() + 
  geom_sf(cbgpop |> 
            filter(substr(GEOID,1,2) == "09"), 
          mapping = aes(fill=population, 
                        color=population)) + 
  geom_sf(ct_counties,
          mapping=aes(),
          color = "deeppink3",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4")+
  scale_color_gradient(low = "thistle1", high = "deeppink4")
ct

library(patchwork)
pop_zoom <- (new_england | ct)+
  plot_layout(guides = 'collect')
pop_zoom

ggsave(plot = pop_zoom,
       file = "img/extra_figures/population_zoom.png",
       width = 16,
       height = 9,
       dpi = 100
)

## Hexgrid plots
us_hex_plt <- ggplot() + 
  geom_sf(hexpop, 
          mapping=aes(fill=population)) +
  # geom_sf(us_states,
  #         mapping=aes(),
  #         color = "thistle4",
  #         fill = "transparent",
  #         size = 1.2)+
  scale_fill_gradient(low = "thistle1", high = "deeppink3")+
  scale_color_gradient(low = "thistle1", high = "deeppink3")+
  theme_minimal()
us_hex_plt

hexes_to_county <- vroom::vroom("data-products/geo-hexes/hexid-fips-map.csv")

hexpop <- hexpop |> 
  left_join(hexes_to_county |> select(hexid, fips)) |> 
  mutate(STATEFP = str_sub(fips, 1, 2))

new_england_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09","23","25","33","44","50"))

new_england_hex_plt <- ggplot()+
  geom_sf(new_england_hexpop,
          mapping=aes(fill=population))+ 
  geom_sf(new_england_states,
          mapping=aes(),
          color = "deeppink3",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4")
new_england_hex_plt

ct_hexpop <- hexpop |> 
  filter(STATEFP %in% c("09"))

ct_hex_plt <- ggplot()+
  geom_sf(ct_hexpop,
          mapping=aes(fill=population))+ 
  geom_sf(ct_counties,
          mapping=aes(),
          color = "deeppink3",
          fill = "transparent")+
  theme_minimal() + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4")
ct_hex_plt

library(patchwork)
hexpop_zoom <- ((new_england_hex_plt | ct_hex_plt)/(new_england | ct))+
  plot_layout(guides = 'collect')
hexpop_zoom

hexes <- sf::st_read("data-products/geo-hexes/hexes.shp")

## Pre-Omicron
hexgrid_infections <- vroom::vroom("data-products/geo-hexes/hexid-observations_preomicron.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

# ## Omicron-era
hexgrid_infections <- vroom::vroom("data-products/geo-hexes/hexid-observations_omicronera.csv") |>
  mutate(hexid = as.character(hexid),
         date = as.Date(date)) |>
  select(-geometry) |>
  mutate(infectionsPC = (infections/population)*1e5) |>
  filter(infectionsPC >= 1) |>
  left_join(hexes, by = "hexid") |>
  sf::st_as_sf()

hexgrid_no_geometry <- hexgrid_infections |> 
  st_drop_geometry() |> 
  select(-population)

# vroom_write(hexgrid_no_geometry,
#             file = "data-products/geo-hexes/hexid-observations_preomicron.csv")

alpha_peak_date <- as.Date("2021-08-26")

hexgrid_infections_test <- hexgrid_infections |> 
  filter(date == alpha_peak_date) |>
  sf::st_transform(crs = 26915)

plt_peak_alpha <- ggplot()+
  geom_sf(data = hexgrid_infections_test,
          mapping = aes(fill = infectionsPC))+
  geom_sf(data = hexgrid_infections |>
            filter(date == alpha_peak_date) |>
            filter(infectionsPC == 0),
          mapping = aes(),
          fill = "grey70")+
  scale_fill_viridis_c(option = "magma",
                       name = "Infections PC",
                       # direction = -1,
                       breaks = seq(1, 1001, 100),
                       labels = c(">1", seq(100, 900, 100), "1,000+"),
                       limits = c(1,1001),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "left",
                                                      title.hjust = 0.5,
                                                      barheight = grid::unit(12, "cm")))+
  theme_minimal()+
  theme(legend.title = element_text(angle = 90))+
  labs(title = alpha_peak_date,
       subtitle = "Alpha wave peak")
plt_peak_alpha

ggsave(plot = plt_peak_alpha,
       filename = "img/extra_figures/plot_peak_alpha.png",
       width = 16, 
       height = 9, 
       dpi = 100)

omicron_peak_date <- as.Date("2022-01-20")

hexgrid_infections_test <- hexgrid_infections |> 
  filter(date == omicron_peak_date) |>
  sf::st_transform(crs = 26915)

plt_peak_omicron <- ggplot()+
  geom_sf(data = hexgrid_infections_test,
          mapping = aes(fill = infectionsPC))+
  geom_sf(data = hexgrid_infections |>
            filter(date == omicron_peak_date) |>
            filter(infectionsPC == 0),
          mapping = aes(),
          fill = "grey70")+
  scale_fill_viridis_c(option = "magma",
                       name = "Infections PC",
                       direction = -1,
                       breaks = seq(1, 5001, 499),
                       labels = c(">1", seq(500, 4500, 500), "5,000+"),
                       limits = c(1,5001),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "left",
                                                      title.hjust = 0.5,
                                                      barheight = grid::unit(12, "cm")))+
  theme_dark()+
  theme(legend.title = element_text(angle = 90))+
  labs(title = omicron_peak_date,
       subtitle = "Omicron wave peak")
plt_peak_omicron

ggsave(plot = plt_peak_omicron,
       filename = "img/extra_figures/plot_peak_omicron.png",
       width = 16, 
       height = 9, 
       dpi = 100)

## function to plot alphas
plt_fun <- \(week, is.omicron = FALSE, infections, hexes, plot_img = TRUE) {
  
  # Filter the data for the current week
  infections <- infections |> 
    filter(date == week)
  # alphas <- alphas[alphas$date_week == week, ]
  
  if(!is.omicron){
    
    plot_alphas <- ggplot()+
      geom_sf(data = hexes,
              fill = "transparent")+
      geom_sf(data = infections,
              aes(fill = infectionsPC),
              alpha = 0.75,
              size = 3)+
      scale_fill_viridis_c(option = "magma",
                           name = "Infections PC",
                           # direction = -1,
                           breaks = seq(1, 1001, 100),
                           labels = c(">1", seq(100, 900, 100), "1,000+"),
                           limits = c(1,1001),
                           oob = scales::squish,
                           guide = metR::guide_colorstrip(title.position = "left",
                                                          title.hjust = 0.5,
                                                          barheight = grid::unit(12, "cm")))+
      theme_minimal()+
      theme(legend.title = element_text(angle = 90))
    # guides(alpha = "none",
    #        color = guide_legend(order = 1),
    #        fill = guide_legend(order = 2))+
    # theme(axis.title = element_blank(), 
    #       legend.position = "left",
    #       legend.box = "vertical",
    #       legend.title = element_text(angle = 90))+
    # labs(title = "Alphas")+
    # coord_sf(xlim = st_bbox(hexes)[c(1,3)],
    #          ylim = st_bbox(hexes)[c(2,4)])
    # plot_alphas 
  }else{
    plot_alphas <- ggplot()+
      geom_sf(data = hexes,
              fill = "transparent")+
      geom_sf(data = infections,
              aes(fill = infectionsPC),
              alpha = 0.75,
              size = 3)+
      scale_fill_viridis_c(option = "magma",
                           name = "Infections PC",
                           # direction = -1,
                           breaks = seq(1, 5001, 500),
                           labels = c(">1", seq(100, 4500, 500), "5,000+"),
                           limits = c(1,5001),
                           oob = scales::squish,
                           guide = metR::guide_colorstrip(title.position = "left",
                                                          title.hjust = 0.5,
                                                          barheight = grid::unit(12, "cm")))+
      theme_minimal()+
      theme(legend.title = element_text(angle = 90))
    # guides(alpha = "none",
    #        color = guide_legend(order = 1),
    #        fill = guide_legend(order = 2))+
    # theme(axis.title = element_blank(), 
    #       legend.position = "left",
    #       legend.box = "vertical",
    #       legend.title = element_text(angle = 90))+
    # labs(title = "Alphas")+
    # coord_sf(xlim = st_bbox(hexes)[c(1,3)],
    #          ylim = st_bbox(hexes)[c(2,4)])
    # plot_alphas
  }
  
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

weeks <- sort(unique(hexgrid_infections$date))

hexes <- hexes |>
  filter(as.integer(hexid)<7662) |> 
  sf::st_as_sf()

frame_files <- lapply(weeks, 
                      plt_fun, 
                      TRUE,
                      st_transform(hexgrid_infections, 
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
output_file <- "img/weekly_hex_omicronera.gif"

# Save the GIF animation
magick::image_write(animation2, output_file)

## gif for the infections on counties shapes
observationFips <- st_read("data-products/geo-hexes/observations_preomicron.shp")

testDate <- "2020-07-26"

observationFips <- st_read("data-products/geo-hexes/observations_omicronera.shp")

testDate <- "2022-01-20"

### Explore with a plot
ggplot() + 
  geom_sf(observationFips_omicronera %>% filter(date == testDate),
          mapping=aes(fill=infctPC)) +
  theme_minimal() +
  scale_fill_gradient(low = "lightgreen", high = "deeppink2")

## function to plot alphas
plt_preomicron <- \(week, infections, hexes, plot_img = TRUE) {
  
  # Filter the data for the current week
  infections <- infections |> 
    filter(date == week)
  # alphas <- alphas[alphas$date_week == week, ]
  
  plot_alphas <- ggplot()+
    geom_sf(data = hexes,
            fill = "transparent")+
    geom_sf(data = infections,
            aes(fill = infectionsPC),
            alpha = 0.75,
            size = 3)+
    scale_fill_viridis_c(option = "magma",
                         name = "Infections PC",
                         # direction = -1,
                         breaks = seq(0, 5000, 1000),
                         labels = c(seq(0, 4000, 1000), "5,000+"),
                         limits = c(0,5000),
                         oob = scales::squish,
                         guide = metR::guide_colorstrip(title.position = "left",
                                                        title.hjust = 0.5,
                                                        barheight = grid::unit(12, "cm")))+
    theme_minimal()+
    theme(legend.title = element_text(angle = 90))
  # guides(alpha = "none",
  #        color = guide_legend(order = 1),
  #        fill = guide_legend(order = 2))+
  # theme(axis.title = element_blank(), 
  #       legend.position = "left",
  #       legend.box = "vertical",
  #       legend.title = element_text(angle = 90))+
  # labs(title = "Alphas")+
  # coord_sf(xlim = st_bbox(hexes)[c(1,3)],
  #          ylim = st_bbox(hexes)[c(2,4)])
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


weeks <- sort(unique(observationFips$date))

frame_files <- lapply(weeks, 
                      plt_preomicron, 
                      observationFips |> 
                        rename(infectionsPC = infctPC), 
                      us_states,
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation3 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)

animation3


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

plot_alphas <- ggplot()+
  geom_point()

## function to plot alphas
plt_vectors <- \(week, alphas, hexes, plot_img = TRUE) {
  
  # Filter the data for the current week
  alphas <- alphas |> 
    filter(date_week == week)
  # alphas <- alphas[alphas$date_week == week, ]
  
  plot_alphas <- ggplot()+
    geom_sf(data = hexes,
            fill = "transparent")+
    # geom_sf(data = alphas,
    #         aes(fill = alpha),
    #         alpha = 0.75,
    #         size = 3,  
    #         arrow = arrow(ends = "last", 
    #                       type = "open", 
    #                       length = unit(0.5, "cm")))+
    ggquiver::geom_quiver(data = alphas,
                          aes(x = lon, y = lat,
                              u = -delta_lon, v = -delta_lat, 
                              color = alpha,
                              alpha = alpha))+
    # scale_color_viridis_c(option = "inferno",
    #                       name = "Alpha value",
    #                       # direction = -1,
    #                       breaks = seq(0, 1400, 100),
    #                       labels = c(seq(0, 1300, 100), "1,400+"),
    #                       limits = c(0,1400),
    #                       oob = scales::squish,
    #                       guide = metR::guide_colorstrip(title.position = "left",
    #                                                      title.hjust = 0.5,
    #                                                      barheight = grid::unit(12, "cm")))+
    MoMAColors::scale_color_moma_c(name = "Infections per capita/week",
                                   palette_name = "vonHeyl",
                                   direction = -1,
                                   breaks = seq(0, 1000, 100),
                                   labels = c(seq(0, 900, 100), "1,000+"),
                                   limits = c(0,1000),
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

