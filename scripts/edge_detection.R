rm(list = ls())
gc()

## Loading auxiliary functions
source("scripts/plot_functions.R")
source("scripts/canny_functions.R")

library(tidyverse)

## Loading databases
hexes <- sf::st_read("data-products/geo-hexes/hexes.geojson")

hexes_centroid <- sf::st_centroid(hexes)

hexes_boundary <- sf::st_union(hexes)

hexid_observations <- vroom::vroom("data-products/geo-hexes/hexid-observations.csv") |> 
  filter(date == max(date))

hexes_observations <- hexes |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(hexid_observations)

edgelist <- vroom::vroom("data-products/geo-hexes/hexid-neighbors.csv")|>
  # ## Writing the hexbin code as a 4-digit number
  dplyr::mutate(hex_i = sprintf("%04d", i),
                hex_j = sprintf("%04d", j)) |>
  # ## Creating a 8-digit hexbin code to identifying uniquely them
  dplyr::mutate(i_j = str_c(i, ",", j))

# ## Pre-Omicron
features_preomicron <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt_preomicron_SAII.geojson") |>
  dplyr::filter(date_week == max(date_week)) |>
  dplyr::filter(alpha == max(alpha, na.rm = T),
                .by = c(j, date_week))

features_preomicron <- features_preomicron |> 
  mutate(j = as.double(j)) |> 
  left_join(edgelist)

alphas_ij <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_preomicron_SAII.csv") |> 
  # dplyr::select(i,j,date_week, alpha) |>
  # ## Writing the hexbin code as a 4-digit number
  dplyr::mutate(hex_i = sprintf("%04d", i),
                hex_j = sprintf("%04d", j)) |>
  # ## Creating a 8-digit hexbin code to identifying uniquely them
  dplyr::mutate(i_j = str_c(hex_i, hex_j)) |>
  dplyr::filter(date_week == max(date_week)) |>
  dplyr::filter(alpha == max(alpha, na.rm = T),
                .by = c(i_j, date_week))

# samples_id_hexes <- c(6,21,49,80,1,11,33,64,96,5,20,48,79,10,32,63,95,12,34,65,19,47,78)
samples_id_hexes <- c(1,5,11,6,21,33,20)
samples_id_hexes <- c(1,5,11,6,21,33,20,10,32,48,64,49,34,12)
samples_id_hexes <- c(1,5,11,6,21,33,20,10,32,48,64,49,34,12,19,47,63,79,96,80,65,78,95)
samples_id_hexes <- 1:100
samples_id_hexes <- 1:500
samples_id_hexes <- 1:1000
samples_id_hexes <- 1:2500
samples_id_hexes <- 1:10000

sample_edgelist <- edgelist |> 
  filter(i %in% samples_id_hexes)

samples_features <- features_preomicron |> 
  filter(j %in% samples_id_hexes) |>
  filter(alpha>0)

samples_alphas <- alphas_ij |> 
  filter(i %in% samples_id_hexes,
         j %in% samples_id_hexes) |>
  filter(alpha>0)

samples_hexes <- hexid_observations |> 
  inner_join(edgelist, by = c("hexid" = "i")) |> 
  rename(i = hexid,
         alpha = infectionsPC) |> 
  select(i_j, alpha)

# samples_features <- samples_features |> 
#   mutate(hexid = as.double(hexid)) |> 
#   left_join(samples_alphas |> 
#               select(i,j, alpha),
#             by = c("hexid" = "i"))

samples_alphas <- samples_alphas |> 
  select(i,j,alpha)

canny_alphas_edge <- canny_edge_detector(m = samples_alphas, 
                                         gaussian_kernel = '5x5',
                                         low_threshold = 0.3, 
                                         high_threshold = 0.7)

## Try to use quantile function
## low threshold = mean - sd, high threshold = mean + sd
## low threshold = mean - 1.96sd, high threshold = mean + 1.96sd

# canny_obser_edge <- canny_edge_detector(m = samples_hexes,
#                                         low_threshold = 0.3,
#                                         high_threshold = 0.7)

canny_alphas_df <- data.frame(i = c(t(row(canny_alphas_edge))),
                              j = c(t(col(canny_alphas_edge))),
                              edge = c(t(canny_alphas_edge)))

canny_alphas <- inner_join(edgelist, canny_alphas_df)

plot_infections <- ggplot()+
  geom_sf(data = hexes_observations |> 
            # filter(as.integer(hexid) < 7662) |> 
            filter(hexid %in% samples_id_hexes) |> 
            sf::st_as_sf(),
          aes(fill = infectionsPC),
          size = 3)+
  geom_sf_text(data = hexes_centroid |>
                 filter(hexid %in% samples_id_hexes),
               aes(label = hexid),
               color = "gray50",
               size = 12)+
  scale_fill_viridis_c(option = "rocket",
                       name = "Infections PC",
                       breaks = seq(0,1000, 100),
                       labels = c(seq(0,900, 100), "1000+"),
                       limits = c(0,1000),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "left",
                                                      title.hjust = 0.5,
                                                      barheight = grid::unit(12, "cm")))+
  theme_void()+
  theme(axis.title = element_blank(), 
        legend.position = "left",
        legend.title = element_text(angle = 90))+
  labs(title = "Infections")+
  guides(alpha = "none")
plot_infections

plot_alphas <- hexes |> 
  filter(as.integer(hexid) < 7662) |> 
  sf::st_as_sf() |> 
  # mutate(hexid = as.double(hexid)) |>
  # left_join(canny_alphas,
  #           by = c("hexid" = "i")) |>
  ggplot()+
  geom_sf(size = 0.1)+
  # geom_sf_text(data = hexes_centroid |>
  #                filter(hexid %in% samples_id_hexes),
  #         aes(label = hexid),
  #         color = "gray50",
  #         size = 12)+
  geom_curve(data = samples_features |> 
               filter(alpha >= 100),
             aes(x = start_coord_x, xend = end_coord_x,
                 y = start_coord_y, yend = end_coord_y,
                 color = alpha),
             arrow = grid::arrow(length = unit(x = 1.2, units = "mm"), type = "closed"))+
  # scale_fill_viridis_d(option = "rocket", direction = -1)+
  scale_color_viridis_c(option = "rocket",
                        name = "Alphas",
                        breaks = pretty(samples_features$alpha, n = 10),
                        oob = scales::squish,
                        guide = metR::guide_colorstrip(title.position = "left",
                                                       title.hjust = 0.5,
                                                       barheight = grid::unit(10, "cm")))+
  theme_void()+
  theme(axis.title = element_blank(), 
        legend.position = "left",
        legend.title = element_text(angle = 90))+
  labs(title = "Alphas")+
  guides(alpha = "none")
plot_alphas

plot_edge <- hexes |> 
  filter(as.integer(hexid) < 7662) |> 
  sf::st_as_sf() |> 
  # mutate(hexid = as.double(hexid)) |>
  # left_join(canny_alphas,
  #           by = c("hexid" = "i")) |>
  ggplot()+
  geom_sf(size = 0.1)+
  # geom_sf_text(data = hexes_centroid |>
  #                filter(hexid %in% samples_id_hexes),
  #              aes(label = hexid),
  #              color = "gray50",
  #              size = 12)+
  geom_curve(data = samples_features |> 
               inner_join(canny_alphas) |> 
               filter(edge == TRUE),
             aes(x = start_coord_x, xend = end_coord_x,
                 y = start_coord_y, yend = end_coord_y,
                 color = edge),
             arrow = grid::arrow(length = unit(x = 1.2, units = "mm"), type = "closed"))+
  # scale_fill_viridis_d(option = "rocket", direction = -1)+
  scale_color_viridis_d(option = "rocket")+
  theme_void()+
  theme(axis.title = element_blank(),
        legend.position = "none")+
  labs(title = "Edges")+
  guides(alpha = "none")
plot_edge

library(patchwork)
patchwork_edges <- (plot_infections/ plot_alphas)
patchwork_edges

## New England Alpha Wave
samples_id_ne <- 6972:7661
dates_ne <- as.Date("2020-04-30")

ne_observations <-  vroom::vroom("data-products/geo-hexes/hexid-observations.csv") |> 
  filter(as.integer(hexid) %in% samples_id_ne,
         date > "2020-03-01" & date < "2020-09-01") |> 
  mutate(date_week = floor_date(date, week_start = "Thursday", unit = "week")) |> 
  reframe(infectionsPC = sum(infectionsPC),
          .by = c("hexid", "date_week"))

ne_observations |> 
  group_by(date_week) |> 
  summarise(infectionsPC = sum(infectionsPC)) |> 
  ggplot(aes(date_week, infectionsPC))+
  geom_line()+
  theme_minimal()

ne_hexes <- hexes |> 
  filter(as.integer(hexid) %in% samples_id_ne) 

ne_bbox <- sf::st_bbox(ne_hexes)

ne_infections <- ne_hexes |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(ne_observations)

ne_hexid <- hexes_centroid |> 
  filter(as.integer(hexid) %in% samples_id_ne)

## Canny edge detector
## New England Alpha Wave
samples_id_ne <- 6972:7661
# dates_ne <- as.Date("2020-04-30")
date_ne <- as.Date("2020-04-09")

ne_observations <-  vroom::vroom("data-products/geo-hexes/hexid-observations.csv") |> 
  filter(as.integer(hexid) %in% samples_id_ne) |> 
  mutate(date_week = floor_date(date, week_start = "Thursday", unit = "week")) |> 
  reframe(infectionsPC = sum(infectionsPC),
          .by = c("hexid", "date_week"))

ne_observations |> 
  group_by(date_week) |> 
  summarise(infectionsPC = sum(infectionsPC)) |> 
  ggplot(aes(date_week, infectionsPC))+
  geom_line()+
  theme_minimal()

ne_hexes <- hexes |> 
  filter(as.integer(hexid) %in% samples_id_ne) 

ne_bbox <- sf::st_bbox(ne_hexes)

ne_infections <- ne_hexes |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(ne_observations)

ne_hexid <- hexes_centroid |> 
  filter(as.integer(hexid) %in% samples_id_ne)

ne_features <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt_preomicron_SAII.geojson") |>
  dplyr::mutate(j = as.double(j)) |>
  dplyr::inner_join(edgelist) |>
  dplyr::filter(alpha == max(alpha, na.rm = T),
                .by = c(i,j, date_week)) |>
  filter(i %in% samples_id_ne,
         j %in% samples_id_ne) |> 
  dplyr::filter(date_week == date_ne)

ne_alphas <- vroom::vroom("data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat_preomicron_SAII.csv") |> 
  # dplyr::select(i,j,date_week, alpha) |>
  # ## Writing the hexbin code as a 4-digit number
  # dplyr::mutate(hex_i = sprintf("%04d", i),
  #               hex_j = sprintf("%04d", j)) |>
  # ## Creating a 8-digit hexbin code to identifying uniquely them
  # dplyr::mutate(i_j = str_c(hex_i, hex_j)) |>
  dplyr::filter(date_week == date_ne) |> 
  # dplyr::filter(alpha == max(alpha, na.rm = T),
  #               .by = c(i,j, date_week)) |> 
  filter(i %in% samples_id_ne,
         j %in% samples_id_ne)

ne_features <- ne_features |>
  filter(alpha>0)

ne_alphas <- ne_alphas |> 
  filter(alpha>0) |> 
  select(i,j,alpha)

ne_canny_edges <- canny_edge_detector(m = ne_alphas, 
                                      gaussian_kernel = '3x3',
                                      low_threshold = 0.3, 
                                      high_threshold = 0.7)

ne_canny_df <- data.frame(i = c(t(row(ne_canny_edges))),
                          j = c(t(col(ne_canny_edges))),
                          edge = c(t(ne_canny_edges)))

ne_canny_alphas <- inner_join(edgelist, ne_canny_df)

plot_infections_ne <- ggplot()+
  geom_sf(data = ne_hexes, 
          fill = "transparent",
          size = 0.1)+
  geom_sf(data = ne_infections |> 
            filter(date_week == date_ne),
          aes(fill = infectionsPC),
          size = 0.1)+
  theme_void()+
  scale_fill_viridis_c(option = "rocket",
                       # midpoint = 600,
                       direction = -1,
                       name = "Infections per capita",
                       breaks = seq(0,5000, 500),
                       labels = c(seq(0,4500, 500), "5000+"),
                       limits = c(0,5000),
                       oob = scales::squish,
                       guide = metR::guide_colorstrip(title.position = "top",
                                                      title.hjust = 0.5,
                                                      barwidth = grid::unit(12, "cm")))+
  theme(legend.position = "top")+
  labs(tag = date_ne)
plot_infections_ne

plot_vectors_ne <- ne_infections |> 
  ggplot()+
  geom_sf(size = 0.1)+
  geom_segment(data = ne_features,
               aes(x = start_coord_x, xend = end_coord_x,
                   y = start_coord_y, yend = end_coord_y,
                   color = alpha),
               arrow = grid::arrow(length = unit(x = 1.2, units = "mm"), type = "closed"))+
  theme_void()+
  scale_color_viridis_b(option = "rocket",
                        direction = -1,
                        name = "Alphas",
                        breaks = pretty(ne_features$alpha, n = 10),
                        # oob = scales::squish,
                        guide = metR::guide_colorstrip(title.position = "top",
                                                       title.hjust = 0.5,
                                                       barwidth = grid::unit(12, "cm")))+
  theme(legend.position = "top")
plot_vectors_ne

plot_edge_ne <- ne_infections |> 
  ggplot()+
  geom_sf(size = 0.1)+
  geom_segment(data = ne_features |> 
                 inner_join(ne_canny_alphas) |> 
                 filter(edge==T),
               aes(x = start_coord_x, xend = end_coord_x,
                   y = start_coord_y, yend = end_coord_y,
                   color = edge),
               arrow = grid::arrow(length = unit(x = 1.2, units = "mm"), type = "closed"))+
  scale_color_viridis_d(option = "rocket")+
  theme_void()+
  theme(axis.title = element_blank(), 
        legend.position = "none")+
  labs(title = "Edges")
plot_edge_ne

library(patchwork)
patchwork_edges_ne <- (plot_infections_ne| plot_vectors_ne | plot_edge_ne)
patchwork_edges_ne

LT <- seq(0.1,0.9, 0.1)
HT <- 1

ne_canny_list <- list()
ne_plot_edge_list <- list()

for (i in 1:length(LT)) {
  ne_canny_edges <- canny_edge_detector(m = ne_alphas, 
                                        gaussian_kernel = '5x5',
                                        low_threshold = LT[i], 
                                        high_threshold = HT)
  
  ne_canny_df <- data.frame(i = c(t(row(ne_canny_edges))),
                            j = c(t(col(ne_canny_edges))),
                            edge = c(t(ne_canny_edges)))
  
  ne_canny_list[[i]] <- inner_join(edgelist, ne_canny_df)
  
  ne_plot_edge_list[[i]] <- ne_infections |> 
    ggplot()+
    geom_sf(size = 0.1)+
    geom_segment(data = ne_features |> 
                   inner_join(ne_canny_list[[i]]) |> 
                   filter(edge == TRUE),
                 aes(x = start_coord_x, xend = end_coord_x,
                     y = start_coord_y, yend = end_coord_y,
                     color = edge),
                 arrow = grid::arrow(length = unit(x = 1.2, units = "mm"), type = "closed"))+
    scale_color_viridis_d(option = "rocket")+
    theme_void()+
    theme(axis.title = element_blank(), 
          legend.position = "none")+
    labs(title = paste0("Edges, LT = ", LT[i], " and HT = ", HT))
}

patchwork_LT <- ((ne_plot_edge_list[[1]] | ne_plot_edge_list[[2]] | ne_plot_edge_list[[3]])/
                   (ne_plot_edge_list[[4]] | ne_plot_edge_list[[5]] | ne_plot_edge_list[[6]])/
                   (ne_plot_edge_list[[7]] | ne_plot_edge_list[[8]] | ne_plot_edge_list[[9]]))
patchwork_LT

# |>
#   # filtering to only show alphas >0, 
#   # and the maximum value between a_ij and a_ji
#   dplyr::filter(alpha>0, alpha == pmax(alpha),
#                 .by = c(j,date_week))
# 
# ## Omicron-era
# features_omicronera <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt_omicronera.geojson")|> 
#   # filtering to only show alphas >0, 
#   # and the maximum value between a_ij and a_ji
#   dplyr::filter(alpha>0, alpha == pmax(alpha),
#                 .by = c(j,date_week))

library(tidyverse)
## Alphas

library(Matrix)
library(OpenImageR)
library(RColorBrewer)

source("scripts/canny_functions.R")

## Sample size
samples_size <- 500

samples_alpha <- alphas_ij |> 
  dplyr::filter(i %in% 1:samples_size, 
                j %in% 1:samples_size)

## Prewitt kernels
prewitt_x <- matrix(nrow = 3,
                    c(-1,-1,-1,0,0,0,1,1,1))
image(prewitt_x, col = brewer.pal(name = "PuOr", n = 11))

prewitt_y <- t(prewitt_x)
image(prewitt_y, col = brewer.pal(name = "PuOr", n = 11))

prewitt_Ix <- conv_edge(alphas_ij, k = prewitt_x, sample_size = samples_size, return.df = FALSE)
prewitt_Ix2 <- prewitt_Ix*prewitt_Ix
image(prewitt_Ix2, col = brewer.pal(name = "PuOr", n = 11))
prewitt_Iy <- conv_edge(alphas_ij, k = prewitt_y, sample_size = samples_size, return.df = FALSE)
prewitt_Iy2 <- prewitt_Iy*prewitt_Iy
image(prewitt_Iy2, col = brewer.pal(name = "PuOr", n = 11))

prewitt_magnitude <- magnitude(prewitt_Ix2, prewitt_Iy2)
image(prewitt_magnitude, col = brewer.pal(name = "PuOr", n = 11))
prewitt_magnitude_df <- data.frame(i = c(t(row(prewitt_magnitude))),
                                   j = c(t(col(prewitt_magnitude))),
                                   alpha = c(t(prewitt_magnitude)))

prewitt_direction <- atan2(prewitt_Ix, prewitt_Iy)
image(prewitt_direction, col = brewer.pal(name = "PuOr", n = 11))
prewitt_direction_df <- data.frame(i = c(t(row(prewitt_direction))),
                                   j = c(t(col(prewitt_direction))),
                                   alpha = c(t(prewitt_direction)))

prewitt_mag_plus_dir <- prewitt_magnitude + prewitt_direction
image(prewitt_mag_plus_dir, col = brewer.pal(name = "PuOr", n = 11))
prewitt_mag_plus_dir <- data.frame(i = c(t(row(prewitt_mag_plus_dir))),
                                   j = c(t(col(prewitt_mag_plus_dir))),
                                   alpha = c(t(prewitt_mag_plus_dir)))

## sobel kernels
sobel_x <- matrix(nrow = 3,
                  c(-1,-2,-1,0,0,0,1,2,1))
image(sobel_x, col = brewer.pal(name = "PuOr", n = 11))

sobel_y <- t(sobel_x)
image(sobel_y, col = brewer.pal(name = "PuOr", n = 11))

sobel_Ix <- conv_edge(alphas_ij, k = sobel_x, sample_size = samples_size, return.df = FALSE)
sobel_Ix2 <- sobel_Ix*sobel_Ix
image(sobel_Ix2, col = brewer.pal(name = "PuOr", n = 11))
sobel_Iy <- conv_edge(alphas_ij, k = sobel_y, sample_size = samples_size, return.df = FALSE)
sobel_Iy2 <- sobel_Iy*sobel_Iy
image(sobel_Iy2, col = brewer.pal(name = "PuOr", n = 11))

sobel_magnitude <- magnitude(sobel_Ix2, sobel_Iy2)
image(sobel_magnitude, col = brewer.pal(name = "PuOr", n = 11))
sobel_magnitude_df <- data.frame(i = c(t(row(sobel_magnitude))),
                                 j = c(t(col(sobel_magnitude))),
                                 alpha = c(t(sobel_magnitude)))
sobel_direction <- atan2(sobel_Ix, sobel_Iy)
image(sobel_direction, col = brewer.pal(name = "PuOr", n = 11))
sobel_direction_df <- data.frame(i = c(t(row(sobel_direction))),
                                 j = c(t(col(sobel_direction))),
                                 alpha = c(t(sobel_direction)))

sobel_mag_plus_dir <- sobel_magnitude + sobel_direction
image(sobel_mag_plus_dir, col = brewer.pal(name = "PuOr", n = 11))
sobel_mag_plus_dir <- data.frame(i = c(t(row(sobel_mag_plus_dir))),
                                 j = c(t(col(sobel_mag_plus_dir))),
                                 alpha = c(t(sobel_mag_plus_dir)))

## Gaussian
gauss <- matrix(nrow = 3,
                c(1,2,1,2,4,2,1,2,1)/16)
image(gauss, col = brewer.pal(name = "PuOr", n = 11))

conv_gauss_df <- conv_edge(alphas_ij, k = gauss, sample_size = samples_size)

## Laplacian
laplace <- matrix(nrow = 3,
                  c(0,1,0,1,4,1,0,1,0))
image(laplace, col = brewer.pal(name = "PuOr", n = 11))
conv_laplace_df <- conv_edge(alphas_ij, k = laplace, sample_size = samples_size)
laplace2 <- matrix(nrow = 3,
                   c(1,4,1,4,-20,4,1,4,1))
image(laplace2, col = brewer.pal(name = "PuOr", n = 11))
conv_laplace2_df <- conv_edge(alphas_ij, k = laplace2, sample_size = samples_size)
laplace3 <- matrix(nrow = 5,
                   c(0,0,-1,0,0,
                     0,-1,-2,-1,0,
                     -1,-2,16,-2,-1,
                     0,-1,-2,-1,0,
                     0,0,-1,0,0))
image(laplace3, col = brewer.pal(name = "PuOr", n = 11))
conv_laplace3_df <- conv_edge(alphas_ij, k = laplace2, sample_size = samples_size)

## Laplacian of Gaussian
conv_gauss <- conv_edge(alphas_ij, k = gauss, sample_size = samples_size, return.df = F)
image(conv_gauss, col = brewer.pal(name = "PuOr", n = 11))

conv_LoG_df <- conv_edge(conv_gauss_df, k = laplace)

conv_LoG2_df <- conv_edge(conv_gauss_df, k = laplace2)

conv_LoG3_df <- conv_edge(conv_gauss_df, k = laplace2)

## Canny
canny <- matrix(nrow = 5,
                c(2,4,5,4,2,
                  4,9,12,9,4,
                  5,12,15,12,5,
                  4,9,12,9,4,
                  2,4,5,4,2))
image(canny, col = brewer.pal(name = "PuOr", n = 11))

samples_size <- 1000
source("scripts/canny_functions.R")

conv_canny <- conv_edge(alphas_ij, k = canny, sample_size = samples_size, return.df = F)
image(conv_canny, col = brewer.pal(name = "PuOr", n = 11))
## Ix, Ix2
canny_Ix <- conv_edge(conv_canny, k = sobel_x, return.df = F)
image(canny_Ix, col = brewer.pal(name = "PuOr", n = 11))
canny_Ix2 <- canny_Ix*canny_Ix
image(canny_Ix2, col = brewer.pal(name = "PuOr", n = 11))
## Iy, Iy2
canny_Iy <- conv_edge(conv_canny, k = sobel_y, return.df = F)
image(canny_Iy, col = brewer.pal(name = "PuOr", n = 11))
canny_Iy2 <- canny_Iy*canny_Iy
image(canny_Iy2, col = brewer.pal(name = "PuOr", n = 11))
## Magnitude
canny_magnitude <- magnitude(canny_Ix2, canny_Iy2)
image(canny_magnitude, col = brewer.pal(name = "PuOr", n = 11))
## Direction
canny_direction <- atan2(canny_Ix, canny_Iy)
image(canny_direction, col = brewer.pal(name = "PuOr", n = 11))
## Canny Magnitude + Direction
canny_mag_plus_dir <- canny_magnitude + canny_direction
image(canny_mag_plus_dir, col = brewer.pal(name = "PuOr", n = 11))
canny_mag_plus_dir_df  <- data.frame(i = c(t(row(canny_mag_plus_dir))),
                                     j = c(t(col(canny_mag_plus_dir))),
                                     alpha = c(t(canny_mag_plus_dir)))
## Non-maxima suppression
canny_nonmaxsup <- non_maximum_suppression(canny_magnitude, canny_direction)
image(canny_nonmaxsup, col = brewer.pal(name = "PuOr", n = 11))

canny_nonmaxsup_df <- data.frame(i = c(t(row(canny_nonmaxsup))),
                                 j = c(t(col(canny_nonmaxsup))),
                                 alpha = c(t(canny_nonmaxsup)))
## Canny edge tracking
canny_edge_track <- edge_tracking_by_hysteresis(canny_nonmaxsup, low_threshold = 20, high_threshold = 50)
image(canny_edge_track, col = brewer.pal(name = "PuOr", n = 11))

canny_edge_track_df <- data.frame(i = c(t(row(canny_edge_track))),
                                  j = c(t(col(canny_edge_track))),
                                  alpha = c(t(canny_edge_track)))

plt_canny_edge <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(canny_edge_track_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_d(name = "Edge", direction = -1,
                       option = "rocket")+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Canny Edge Track")
plt_canny_edge

## Ixy
# canny_sobel_Ixy <- canny_sobel_Ix*canny_sobel_Iy
# conv_canny_Ixy <- conv_edge(conv_canny, k = canny_sobel_Ixy, return.df = F)
# image(conv_canny_Ixy, col = brewer.pal(name = "PuOr", n = 11))
## Non-maxima suppression
# nonmaxima <- 

## To-do: Ix2, Iy2, Ixy as data.frames

# canny_G <- G(canny_sobel_Ix2, canny_sobel_Iy2)
# conv_canny_G_df <- data.frame(i = c(t(row(canny_G))),
#                               j = c(t(col(canny_G))),
#                               alpha = c(t(canny_G)))
# 
# canny_Theta <- atan2(canny_sobel_Ix, canny_sobel_Iy)
# conv_canny_Theta_df <- data.frame(i = c(t(row(canny_Theta))),
#                                   j = c(t(col(canny_Theta))),
#                                   alpha = c(t(canny_Theta)))
# 
# canny_grad_plus_edge <- canny_G + canny_Theta
# conv_canny_grad_plus_edge <- data.frame(i = c(t(row(canny_grad_plus_edge))),
#                                         j = c(t(col(canny_grad_plus_edge))),
#                                         alpha = c(t(canny_grad_plus_edge)))

## Harris: (Ix2*Iy2 - Ixy2)/(Ix2 + Iy2)
# det <- conv_canny_Ix2 * conv_canny_Iy2 - conv_canny_Ixy^2
# trace <- 0.04*(canny_sobel_Ix + canny_sobel_Iy)^2
# canny_harris <- det - trace
# image(canny_harris, col = brewer.pal(name = "PuOr", n = 11))
# 
# canny_harris_df <- data.frame(i = c(t(row(canny_harris))),
#                               j = c(t(col(canny_harris))),
#                               alpha = c(t(canny_harris)))
# 
# ## Non-maxima suppression
# mx <- 



## Plotting

samples_alpha <- alphas_ij|> 
  dplyr::filter(i %in% 1:samples_size, 
                j %in% 1:samples_size)

plot_original <- hexes |> 
  # filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(alphas_ij, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "Original")
plot_original

plt_sobel_grad <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_G_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                mutate(j = as.integer(j)) |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(name = "Hexes",
                       option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(name = "Vectors",
  #                       option = "rocket",
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Sobel on gradient direction")
plt_sobel_grad

plt_sobel_angle <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_Theta_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                mutate(j = as.integer(j)) |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(name = "Hexes",
                       option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(name = "Vectors",
  #                       option = "rocket",
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Sobel on angle")
plt_sobel_angle

plt_sobel_grad_angle <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_sobel_grad_plus_edge, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                mutate(j = as.integer(j)) |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(name = "Hexes",
                       option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(name = "Vectors",
  #                       option = "rocket",
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Sobel on gradient + angle")
plt_sobel_grad_angle

patchwork_sobel <- (plot_original | (plt_sobel_angle / plt_sobel_grad) | plt_sobel_grad_angle)
patchwork_sobel

ggsave()

plt_prewitt_grad <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_prewitt_G_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                mutate(j = as.integer(j)) |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(name = "Hexes",
                       option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(name = "Vectors",
  #                       option = "rocket",
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Prewitt grad")
plt_prewitt_grad

plt_prewitt_angle <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_prewitt_Theta_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(name = "Hexes",
                       option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(name = "Vectors",
  #                       option = "rocket",
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Prewitt angle")
plt_prewitt_angle

plt_prewitt_grad_plus_angle <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_prewitt_grad_plus_edge, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |>
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(name = "Hexes",
                       option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(name = "Vectors",
  #                       option = "rocket",
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Prewitt grad + angle")
plt_prewitt_grad_plus_angle

library(patchwork)
patchwork_prewitt <- (plot_original | (plt_prewitt_angle / plt_prewitt_grad) | plt_prewitt_grad_plus_angle)
patchwork_prewitt

plt_gauss <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_gauss_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Gauss")
plt_gauss

patchwork_gauss <- (plot_original | plt_gauss)
patchwork_gauss

plt_laplace <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_laplace_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Laplace")
plt_laplace

plt_laplace2 <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_laplace2_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Laplace2")
plt_laplace2

plt_laplace3 <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_laplace3_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Laplace3")
plt_laplace3

patchwork_laplace <- (plot_original | plt_laplace | plt_laplace2 | plt_laplace3)
patchwork_laplace

plt_LoG <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_LoG_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After LoG: Laplacian of Gaussian")
plt_LoG

plt_LoG2 <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_LoG2_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After LoG2: Laplacian of Gaussian")
plt_LoG2

plt_LoG3 <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_LoG3_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", 
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After LoG3: Laplacian of Gaussian")
plt_LoG3

patchwork_LoG <- (plot_original | plt_LoG | plt_LoG2 | plt_LoG3)
patchwork_LoG

plt_canny_grad <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_canny_G_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Canny grad")
plt_canny_grad

plt_canny_angle <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(conv_canny_Theta_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Canny angle")
plt_canny_angle

plt_canny_grad_plus_angle <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(canny_mag_plus_dir_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Canny grad plus angle")
plt_canny_grad_plus_angle

patchwork_canny <- (plot_original | (plt_canny_grad / plt_canny_angle) | plt_canny_grad_plus_angle)
patchwork_canny

plt_canny_nonmaxsup <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(canny_nonmaxsup_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_c(option = "rocket", direction = -1,
                       guide = guide_colorsteps(even.steps = T))+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Canny Non-max suppression")
plt_canny_nonmaxsup

plt_canny_edge <- hexes |> 
  filter(hexid %in% 1:samples_size) |> 
  sf::st_as_sf() |> 
  mutate(hexid = as.double(hexid)) |> 
  left_join(canny_edge_track_df, by = c("hexid" = "j")) |> 
  ggplot()+
  geom_sf(aes(fill = alpha), size = 0.1)+
  # geom_segment(data = features_preomicron |> 
  #                dplyr::filter(j %in% 1:samples_size),
  #              aes(x = start_coord_x, xend = end_coord_x,
  #                  y = start_coord_y, yend = end_coord_y,
  #                  color = alpha),
  #              arrow = grid::arrow(length = unit(x = .5, units = "mm"), type = "closed"))+
  scale_fill_viridis_d(name = "Edge", direction = -1,
                       option = "rocket")+
  # scale_color_viridis_c(option = "rocket", 
  #                       guide = guide_colorsteps(even.steps = T))+
  theme_minimal()+
  theme(axis.title = element_blank())+
  labs(title = "After Canny Edge Track")
plt_canny_edge

patchwork_nonmax_edge <- (plot_original | plt_canny_nonmaxsup | plt_canny_edge)
patchwork_nonmax_edge

# linconv = function (y,K) {   K=K/sum(K)
# out=convolve(y, K, type="open")
# out=head(out,-(length(out)-length(y)) )
# return(out)
# }
# 
# linconv2 = function (y,K) {  K=K/sum(K)
# require(Matrix)
# X = as.matrix(bandSparse(length(y), 
#                          k = seq(-(length(K)-1),0,1), 
#                          diag = t(replicate(length(y), rev(K))), symm=FALSE))
# 
# out = X %*% as.matrix(y, ncol=1)
# return(out)
# }
# 
# output <- linconv(y = m_alphas, K = kernel)
# output2 <- linconv2(y = m_alphas, K = kernel)
# 
# output <- convolve(x = m_alphas, y = kernel, conj = F, type = "filter")
# 
# linconv2 = function (y,K) {  K=K/sum(K)
# require(Matrix)
# X = as.matrix(bandSparse(length(y), 
#                          k = seq(-(length(K)-1),0,1), 
#                          diag = t(replicate(length(y), rev(K))), symm=FALSE))
# 
# out = X %*% as.matrix(y, ncol=1)
# return(out)
# }
# 
# linconv2(y = m_alphas, K = kernel)
# 
# 
# ## build the M input matrix
# tr_matrix <- sapply(unique(alphas_ij$i), (function(i){
#   sapply(unique(alphas_ij$j), (function(j){
#     tmp <- alphas_ij[alphas_ij$i == i & alphas_ij$j == j, "alpha"]
#     if (length(tmp) > 0) {
#       as.numeric(tmp[1])
#     } else {
#       NA
#     }
#   }))
# }))
# tr_matrix
# 
# tr_matrix <- matrix(data = NA, nrow = 7468, ncol = 7468)
# 
# for (i in is) {
#   for (j in js) {
#     tmp <- alphas_ij[alphas_ij$i == i & alphas_ij$j == j, "alpha"]
#     if (length(tmp) > 0) {
#       as.numeric(tmp[1])
#     } else {
#       NA
#     }
#   }
# }
# 
# 
# library(tidyverse)
# library(magick)
# 
# weeks <- sort(unique(features_preomicron$date_week))
# 
# frame_files <- lapply(weeks, plt_preomicron, features_preomicron, hexes, FALSE)
# frame_files <- frame_files |> 
#   unlist()
# 
# 
# 
# image_list <- lapply(frame_files, image_read)
# 
# gif <- lapply(image_list, function(x){
#   x <- image_resize(x, 
#                     '4800x2700!') |>  
#     image_morph() |> 
#     image_animate(optimize = TRUE)
# })
# 
# convolve_list <- lapply(image_list, function(x){
#   x <- x |> 
#     image_convolve("Gaussian") |> 
#     image_negate() 
#   return(x)
# })
# 
# 
# 
