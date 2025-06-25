
library(tidyverse)
library(sf)

CAR_df2 <- vroom::vroom("data-products/tsa_meta30m_run_preomicron_daily.csv")|>
  dplyr::mutate(cases_fitted = mean,
                incidence_fitted = exp(log10(cases_fitted+1) - logpopulation)*1e5,
                log_incidence = log10(incidence_fitted+1))

hexes_to_keep <- st_read("data-products/geo-hexes/hexid_to_keep.geojson")

# CAR_df2 <- bind_rows(CAR_list) |>
#   filter(date != dates_to_rerun)

hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545,
         hexid %in% as.character(hexes_to_keep$hexid)) |> 
  st_transform(crs = 4326) |> 
  mutate(hexid = as.character(1:n()))

## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,350, 20))
labels_plt <- c("150< ",seq(150,330, 20), ' >350')
limits_plt <- c(0,350)
color_option <- "magma"

# breaks_plt1 <- seq(0,1000, 100)
# labels_plt1 <- c("<100",seq(100,900, 100), '>1,000')
# limits_plt1 <- c(0,1000)
# color_option <- "magma"
# 
# breaks_plt2 <- seq(0,2000, 200)
# labels_plt2 <- c("<200",seq(200, 1800, 200), '>2,000')
# limits_plt2 <- c(0,2000)
# color_option <- "magma"
# 
# breaks_plt3 <- seq(0,3000, 300)
# labels_plt3 <- c("<300",seq(300,2700, 300), '>3,000')
# limits_plt3 <- c(0,3000)
# color_option <- "magma"
# 
# breaks_plt4 <- seq(0,4000, 400)
# labels_plt4 <- c("<400",seq(400,3600, 400), '>4,000')
# limits_plt4 <- c(0,4000)
# color_option <- "magma"
# 
# breaks_plt5 <- seq(0,5000, 500)
# labels_plt5 <- c("<500",seq(500,4500, 500), '>5,000')
# limits_plt5 <- c(0,5000)
# color_option <- "magma"
# 
# breaks_plt6 <- seq(0,6000, 600)
# labels_plt6 <- c("<600",seq(600,5400, 600), '>6,000')
# limits_plt6 <- c(0,6000)
# color_option <- "magma"
# 
# breaks_plt7 <- seq(0,7000, 700)
# labels_plt7 <- c("<700",seq(700,6300, 700), '>7,000')
# limits_plt7 <- c(0,7000)
# color_option <- "magma"

## Peaks dates
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

# alpha_peak_week <- as.Date("2020-11-28")
# delta_peak_week <- as.Date("2021-09-04")

hex_test <- CAR_df2 |> 
  filter(date == delta_peak)

plt_peak_delta <- ggplot()+
  geom_sf(data = hex_test |>
            mutate(hexid = as.character(hexid)) |>
            left_join(hexes) |> 
            st_as_sf() |> 
            st_transform(crs=26915),
          mapping = aes(fill = mean))+
  scale_fill_viridis_b(option = "magma",
                       name = "Estimated Infections/100k/week",
                       direction = -1,
                       # breaks = seq(0,5, 0.5),
                       # n.breaks = 20,
                       breaks = breaks_plt,
                       labels = labels_plt,
                       limits = limits_plt,
                       na.value = "transparent"
  )+
  # scale_color_viridis_b(option = "magma",
  #                      name = "Estimated Infections/100k/week",
  #                      direction = -1,
  #                      # breaks = seq(0,5, 0.5),
  #                      # n.breaks = 20,
  #                      breaks = breaks_plt3,
  #                      labels = labels_plt3,
  #                      limits = limits_plt3,
  #                      na.value = "transparent"
  # )+
  theme_minimal(base_size = 12)+
  theme(legend.title.position = "top",
        plot.background = element_rect(fill = "white", colour = NA),
        legend.location = "plot",
        legend.position = "bottom", 
        legend.key.width = grid::unit(3, "cm"))+
  labs(title = delta_peak,
       subtitle = "Delta wave peak")+
  coord_sf(label_axes = list(bottom = "E", right = "N"))
plt_peak_delta

## function to plot hexes with infections
plt_fun <- \(week, is.omicron = FALSE, infections, hexes, plot_img = TRUE) {
  
  hexes <- hexes |> 
    mutate(hexid = as.character(hexid))
  
  infections <- infections |> 
    mutate(hexid = as.character(hexid))
  
  # Filter the data for the current week
  infections <- infections |> 
    filter(date == week)
  
  plot_alphas <- ggplot()+
    geom_sf(data = infections|> 
              left_join(hexes) |> 
              st_as_sf() |> 
              st_transform(crs=26915),
            mapping = aes(fill = mean))+
    scale_fill_viridis_b(option = "magma",
                         name = "Estimated Infections/100k/week",
                         direction = -1,
                         # breaks = seq(0,5, 0.5),
                         # n.breaks = 20,
                         breaks = breaks_plt,
                         labels = labels_plt,
                         limits = limits_plt,
                         na.value = "transparent"
    )+
    theme_minimal(base_size = 12)+
    theme(legend.title.position = "top",
          plot.background = element_rect(fill = "white", colour = NA),
          legend.location = "plot",
          legend.position = "bottom", 
          legend.key.width = grid::unit(3, "cm"))+
    coord_sf(label_axes = list(bottom = "E", right = "N"))
  
  library(patchwork)
  # plot <- (plot_infections | plot_alphas)
  plot <- plot_alphas
  
  if(plot_img){
    plot <- plot &
      labs(tag = paste("Week:", week))
  }
  
  # # Save the plot as a temporary file
  ggsave(paste0("img/movies/frame_for_week_", week, ".png"), 
         plot, 
         width = 16, 
         height = 9,
         dpi = 300)
  
  # Return the temporary file path
  return(paste0("img/movies/frame_for_week_", week, ".png"))
}

weeks <- sort(unique(na.omit(CAR_df2$date)))

hexes <- hexes |>
  filter(as.integer(hexid)<7662) |> 
  st_transform(crs = 26915) |> 
  sf::st_as_sf()

frame_files <- lapply(na.omit(weeks[seq(1,length(weeks), 7)]), 
                      plt_fun, 
                      TRUE,
                      CAR_df2, 
                      hexes,
                      TRUE)

frame_files <- frame_files |> 
  unlist() 

animation2 <- frame_files%>% 
  magick::image_read() %>% 
  magick::image_animate(fps = 4, 
                        optimize = T)

animation2

# Specify the output file path
output_file <- "img/besag2_meta30m_daily.gif"

# Save the GIF animation
magick::image_write(animation2, output_file, quality = 90, comment = "waves movie")

## Alpha Peak Movie
j <- which(weeks == alpha_peak)

frame_files <- lapply(na.omit(weeks[seq(j-90,j+60, 2)]), 
                      plt_fun, 
                      TRUE,
                      CAR_df2, 
                      hexes,
                      TRUE)

frame_files <- frame_files |> 
  unlist() 

animation3 <- frame_files%>% 
  magick::image_read() %>% 
  magick::image_animate(fps = 4, 
                        optimize = T)
animation3

# Specify the output file path
output_file <- "img/besag2_meta30m_alpha_daily.gif"

# Save the GIF animation
magick::image_write(animation3, 
                    output_file, 
                    quality = 90, 
                    comment = "Alpha Wave movie")

## Delta Peak Movie
j <- which(weeks == delta_peak)

frame_files <- lapply(na.omit(weeks[seq(j-60,j+60, 1)]), 
                      plt_fun, 
                      TRUE,
                      CAR_df2, 
                      hexes,
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation4 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 4, 
                                    optimize = T)
animation4

# Specify the output file path
output_file <- "img/besag2_meta30m_delta_daily.gif"

# Save the GIF animation
magick::image_write(animation4, 
                    output_file, 
                    quality = 90, 
                    comment = "Delta Wave movie")


for (i in weeks) {
  hex_test <- CAR_df2 |> 
    filter(date == i)
  
  plt <- ggplot()+
    geom_sf(data = hex_test |>
              mutate(hexid = as.character(hexid)) |> 
              left_join(hexes) |> 
              st_as_sf() |> 
              st_transform(crs=26915),
            mapping = aes(fill = mean))+
    scale_fill_viridis_b(option = "magma",
                         name = "Estimated Infections/day",
                         direction = -1,
                         breaks = breaks_plt,
                         labels = labels_plt,
                         na.value = "steelblue4",
                         limits = limits_plt
    )+
    theme_minimal()+
    theme(legend.title.position = "top",
          legend.location = "plot",
          legend.position = "bottom", 
          legend.key.width = grid::unit(3, "cm"))+
    labs(title = as.Date(i))+
    coord_sf()
  print(plt)
}

## Population hexes
hex_pop <- sf::st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662,
         ## Taking out the isolated hex at Keywest
         as.integer(hexid) != 6545) |> ## Filtering out Puerto Rico hexes
  st_transform(crs = 26915) |>
  rename(population = metapop_30m) |> 
  mutate(logpopulation = log(population))

ggplot() +
  geom_sf(data = hexes|>
            dplyr::mutate(population = hex_pop$population,
                          logpopulation = hex_pop$logpopulation) |>
            dplyr::mutate(cases_fitted = CAR_list[[j-120]]$mean,
                          incidence_fitted = exp(log(cases_fitted) - logpopulation)*1e5,
                          log_incidence = log10(incidence_fitted)) |>
            # filter(infections > 1) |>
            st_transform(crs = 26915),
          aes(fill = cases_fitted))+
  scale_fill_viridis_b(option = color_option,
                       name = "Estimated Infections",
                       direction = -1,
                       breaks = breaks_plt1,
                       labels = labels_plt1,
                       limits = limits_plt1
  )+
  theme_minimal()+
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  guides(fill = guide_bins(title = "Infections per capita/100k",
                           title.position = "top",
                           title.vjust = 0.5))

sd_list <- sapply(CAR_list, function(x){x <- sd(x$sd)})
sd_list <- data.frame(cbind(week = as.Date(weeks), 
                            sd = sd_list))

ggplot(data = sd_list, aes(x = week, y = sd))+
  geom_point()
