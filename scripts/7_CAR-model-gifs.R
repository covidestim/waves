gc()
rm(list = ls())
###############################################################################
##### Load in the required packages                                       #####
##############################################################################|
library(tidyverse)
library(sf)
library(spdep)
# library(spatialreg)
# library(Matrix)
# library(INLA)
# library(foreach)
# library(doParallel)
library(dplyr)
library(tibble)

## Loading CAR model output
CAR_df_preomicron_w1 <- readRDS("Data/data-products/car_list_wave1_adaptStart.rds") |> 
  bind_rows() |> 
  mutate(wave = "1st Wave")
CAR_df_preomicron_w2 <- readRDS("Data/data-products/car_list_wave2_adaptStart.rds") |> 
  bind_rows() |> 
  mutate(wave = "2nd Wave")

CAR_df <- rbind(CAR_df_preomicron_w1, CAR_df_preomicron_w2)

hexgrid <- st_read("Data/data-products/geo-hexes/hexgrid_1100_km.shp") |> 
         filter(
         # Taking out the isolated hex at Keywest
         as.integer(hexid) != 6644) %>%
         # INLA requires the id to only be 1:N, where N is the total
         # number of observations; because of this we need to rename the 
         # hexids to be continuous. 
         mutate(hexid = ifelse(as.numeric(hexid) < 6645, as.numeric(hexid), 
                               as.numeric(hexid) - 1),
                hexid = as.character(hexid)) 

## Joining to the hexgrid in case we need
# CAR_df <- CAR_df |> 
#   left_join(hexgrid) |> 
#   st_as_sf()

# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
  "02", "60", "03", "81", "07", "64",
  "14", "66", "84", "86", "67", "89",
  "68", "71", "76", "69", "70", "95",
  "43", "72", "74", "78", "79", "15", "11"
)

us_states <- tigris::states(cb = T) |> 
  dplyr::filter(!STATEFP %in% excludes) |> 
  tigris::shift_geometry()|> 
  st_transform(crs = 5070)

## Breakdowns of each peaks
breaks_plt <- c(0,seq(150,350, 20))
labels_plt <- c("150< ",seq(150,330, 20), ' >350')
limits_plt <- c(0,350)
color_option <- "magma"

## Peaks dates
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-04")

# alpha_peak_week <- as.Date("2020-11-28")
# delta_peak_week <- as.Date("2021-09-04")

hex_test <- CAR_df |> 
  filter(date == delta_peak-21)

plt_peak_delta <- ggplot()+
  geom_sf(data = hex_test |>
            mutate(hexid = as.character(hexid)) |>
            left_join(hexgrid) |> 
            st_as_sf() |> 
            st_transform(crs=5070),
          mapping = aes(fill = mean))+
  geom_sf(us_states,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
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
  # scale_fill_viridis_c(option = "magma")+
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
              st_transform(crs=5070),
            mapping = aes(fill = mean))+
    geom_sf(us_states,
          mapping=aes(),
          color = "black",
          fill = "transparent")+
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
  
  # library(patchwork)
  # plot <- (plot_infections | plot_alphas)
  plot <- plot_alphas
  
  if(plot_img){
    plot <- plot +
      labs(tag = paste("Week:", week))
  }
  
  # # Save the plot as a temporary file
  ggsave(paste0("img/frames/frame_for_week_", week, ".png"), 
         plot, 
         width = 16, 
         height = 9,
         dpi = 300)
  
  # Return the temporary file path
  return(paste0("img/frames/frame_for_week_", week, ".png"))
}

weeks <- sort(unique(na.omit(CAR_df$date)))

frame_files <- lapply(na.omit(weeks[seq(1,length(weeks), 1)]), 
                      plt_fun, 
                      TRUE,
                      CAR_df, 
                      hexgrid,
                      TRUE)

frame_files <- frame_files |> 
  unlist() 

animation2 <- frame_files%>% 
  magick::image_read() %>% 
  magick::image_animate(fps = 2, 
                        optimize = T)

animation2

# Specify the output file path
output_file <- "img/wave1and2_daily.gif"

# Save the GIF animation
magick::image_write(animation2, output_file, quality = 90, comment = "waves movie")

## Alpha Peak Movie
j <- which(weeks == alpha_peak)

frame_files <- lapply(na.omit(weeks[seq(j-63,j, 1)]), 
                      plt_fun, 
                      TRUE,
                      CAR_df, 
                      hexgrid,
                      TRUE)

frame_files <- frame_files |> 
  unlist() 

animation3 <- frame_files%>% 
  magick::image_read() %>% 
  magick::image_animate(fps = 2, 
                        optimize = T)
animation3

# Specify the output file path
output_file <- "img/wave1_daily.gif"

# Save the GIF animation
magick::image_write(animation3, 
                    output_file, 
                    quality = 90, 
                    comment = "Alpha Wave movie")

## Delta Peak Movie
j <- which(weeks == delta_peak)

frame_files <- lapply(na.omit(weeks[seq(j-63,j, 1)]), 
                      plt_fun, 
                      TRUE,
                      CAR_df, 
                      hexgrid,
                      TRUE)

frame_files <- frame_files |> 
  unlist()

animation4 <- magick::image_animate(magick::image_read(frame_files), 
                                    fps = 2, 
                                    optimize = T)
animation4

# Specify the output file path
output_file <- "img/wave2_daily.gif"

# Save the GIF animation
magick::image_write(animation4, 
                    output_file, 
                    quality = 90, 
                    comment = "Delta Wave movie")


# for (i in weeks) {
#   hex_test <- CAR_df2 |> 
#     filter(date == i)
  
#   plt <- ggplot()+
#     geom_sf(data = hex_test |>
#               mutate(hexid = as.character(hexid)) |> 
#               left_join(hexes) |> 
#               st_as_sf() |> 
#               st_transform(crs=26915),
#             mapping = aes(fill = mean))+
#     scale_fill_viridis_b(option = "magma",
#                          name = "Estimated Infections/day",
#                          direction = -1,
#                          breaks = breaks_plt,
#                          labels = labels_plt,
#                          na.value = "steelblue4",
#                          limits = limits_plt
#     )+
#     theme_minimal()+
#     theme(legend.title.position = "top",
#           legend.location = "plot",
#           legend.position = "bottom", 
#           legend.key.width = grid::unit(3, "cm"))+
#     labs(title = as.Date(i))+
#     coord_sf()
#   print(plt)
# }

# ## Population hexes
# hex_pop <- sf::st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
#   filter(as.integer(hexid) < 7662,
#          ## Taking out the isolated hex at Keywest
#          as.integer(hexid) != 6545) |> ## Filtering out Puerto Rico hexes
#   st_transform(crs = 26915) |>
#   rename(population = metapop_30m) |> 
#   mutate(logpopulation = log(population))

# ggplot() +
#   geom_sf(data = hexes|>
#             dplyr::mutate(population = hex_pop$population,
#                           logpopulation = hex_pop$logpopulation) |>
#             dplyr::mutate(cases_fitted = CAR_list[[j-120]]$mean,
#                           incidence_fitted = exp(log(cases_fitted) - logpopulation)*1e5,
#                           log_incidence = log10(incidence_fitted)) |>
#             # filter(infections > 1) |>
#             st_transform(crs = 26915),
#           aes(fill = cases_fitted))+
#   scale_fill_viridis_b(option = color_option,
#                        name = "Estimated Infections",
#                        direction = -1,
#                        breaks = breaks_plt1,
#                        labels = labels_plt1,
#                        limits = limits_plt1
#   )+
#   theme_minimal()+
#   theme(legend.position = "bottom",
#         legend.title.position = "top",
#         legend.key.width = grid::unit(1, "cm"))+
#   guides(fill = guide_bins(title = "Infections per capita/100k",
#                            title.position = "top",
#                            title.vjust = 0.5))

# sd_list <- sapply(CAR_list, function(x){x <- sd(x$sd)})
# sd_list <- data.frame(cbind(week = as.Date(weeks), 
#                             sd = sd_list))

# ggplot(data = sd_list, aes(x = week, y = sd))+
#   geom_point()
