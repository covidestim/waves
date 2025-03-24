rm(list = ls())
gc()

## packages
library(sf)
library(rgeoda)
library(tidyverse)

## Loading data
hexes <- sf::st_read("data-products/geo-hexes/hexes.shp") |> 
  filter(as.integer(hexid) < 7662)

## US States border lines
us_states <- tigris::states(cb = T, resolution = "500k") |> 
  st_transform(crs = 26915) |> 
  filter(!NAME %in% c("Alaska",
                      "Hawaii",
                      "Puerto Rico", 
                      "Guam", 
                      "Commonwealth of the Northern Mariana Islands",
                      "American Samoa",
                      "United States Virgin Islands"))

## hexgrids
dataset <- "preomicron"

## Pre-Omicron
CAR_df_preomicron <- vroom::vroom(paste0("data-products/tsa_",
                                         dataset, 
                                         ".csv")) |> 
  mutate(hexid = as.character(hexid))

## hexgrids
dataset <- "immunity"

## Pre-Omicron
CAR_df_immunity <- vroom::vroom(paste0("data-products/tsa_",
                                       dataset, 
                                       ".csv")) |> 
  mutate(hexid = as.character(hexid))

## peak date
alpha_peak <- as.Date("2020-11-19")
delta_peak <- as.Date("2021-09-08")

# ## Hex-immunity
# hex_immunity <- vroom::vroom("data-products/geo-hexes/hexid-immunity.csv") |>
#   mutate(hexid = as.character(hexid))

# immune_hexgrid <- full_join(hex_immunity, hexes)

## TSA over infections and immunity
## Alpha
alpha_trend <- CAR_df_preomicron |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (alpha_peak-128),
                            to = (alpha_peak),
                            length.out = 128)) |>
  mutate(days = (alpha_peak - date),
         trend_alpha = mean,
         wave = "alpha") |>
  select(date, days, hexid, trend_alpha)

alpha_immune <- CAR_df_immunity |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (alpha_peak-128),
                            to = (alpha_peak),
                            length.out = 128)) |>
  mutate(days = (alpha_peak - date),
         immune_alpha = mean,
         wave = "alpha") |>
  select(date, days, hexid, immune_alpha)

alpha_trend_immune <- full_join(alpha_trend, alpha_immune) |> 
  select(-date)

## Delta
delta_trend <- CAR_df_preomicron |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (delta_peak-128),
                            to = (delta_peak),
                            length.out = 128)) |>
  mutate(days = (delta_peak - date),
         trend_delta = mean,
         wave = "delta") |>
  select(date, days, hexid, trend_delta)

delta_immune <- CAR_df_immunity |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (delta_peak-128),
                            to = (delta_peak),
                            length.out = 128)) |> 
  mutate(days = (delta_peak - date),
         immune_delta = mean,
         wave = "delta") |> 
  select(date, days, hexid, immune_delta)

delta_trend_immune <- full_join(delta_trend, delta_immune) |> 
  select(-date)

guerry <- hexes |> 
  filter(hexid !=6545)

queen_w <- queen_weights(guerry)

## Joined data.frame
joined_bind <- full_join(alpha_trend_immune, delta_trend_immune) |> 
  mutate(immune_alpha = 10^(immune_alpha),
         immune_delta = 10^(immune_delta))

## Bivariate color map to Figure.5
style <- "quantile"
bipalette <- "DkViolet"
variable1 <- "trend_alpha"
variable2 <- "trend_delta"
bidim <- 3

## Bivariate mutates
dataset <- list()

for (i in unique(joined_bind$days)) {
  
  ## Filtering
  data <- joined_bind |> 
    filter(days == i) |>
    biscale::bi_class(x = trend_alpha, 
                      y = trend_delta, 
                      style = style, 
                      dim = bidim) |> 
    rename(bi_trend = bi_class) |>
    biscale::bi_class(x = immune_alpha,
                      y = immune_delta,
                      style = style,
                      dim = bidim) |>
    rename(bi_immune = bi_class) |>
    biscale::bi_class(x = immune_alpha,
                      y = trend_delta,
                      style = style,
                      dim = bidim) |>
    rename(bi_immune_trend = bi_class) |>
    mutate(hexid = as.character(hexid)) |>
    left_join(hexes) |>
    st_as_sf() |> 
    st_transform(crs = 26915) |> 
    mutate(dim = bidim,
           style = style)
  
  ## Special condition to 0 days
  i <- ifelse(i == 0, 1, i)
  
  dataset[[i]] <- data
  
}

joined_biclass <- bind_rows(dataset)

## Setting the bi_class breaks outside the loop to have fixed breaks over the animation
bi_breaks <- biscale::bi_class_breaks(.data = joined_biclass,
                                      x = "trend_alpha",
                                      y = "trend_delta",
                                      style = style,
                                      split = TRUE,
                                      clean_levels = T,
                                      dig_lab = 1,
                                      dim = bidim)

variable1 <- "trend"
variable2 <- "trend"

finalPlot <- list()
tmp_file_list <- list()

for (i in unique(joined_bind$days)) {
  
  if(variable1 == "trend" && variable2 == "trend")
  {bivariable <- "bi_trend"}
  else if(variable1 == "trend" && variable2 == "immune")
  {bivariable <- "bi_trend_immune"}
  else if(variable1 == "immune" && variable2 == "trend")
  {bivariable <- "bi_immune_trend"}
  else 
  {bivariable <- "bi_immune"}
  
  # data <- dataset[[ifelse(i == 0, 1, i)]]
  data <- joined_biclass |> 
    filter(days == i)
  
  # create map
  map <- ggplot() +
    geom_sf(data = data, 
            mapping = aes(fill = !!rlang::sym(bivariable),
                          color = !!rlang::sym(bivariable)), 
            size = 0.1, 
            show.legend = FALSE) +
    biscale::bi_scale_fill(pal = bipalette, dim = bidim) +
    biscale::bi_scale_color(pal = bipalette, dim = bidim) +
    biscale::bi_theme()+
    labs(title = str_c("Days before peak: ", i))
  map
  
  legend <- biscale::bi_legend(pal = bipalette,
                               dim = bidim,
                               breaks = bi_breaks,
                               xlab = str_c("Higher Alpha ", variable1, " "),
                               ylab = str_c("Higher Delta ", variable2, " "),
                               size = 8)
  
  # combine map with legend
  plot<- cowplot::ggdraw() +
    cowplot::draw_plot(map, 0, 0, 1, 1) +
    cowplot::draw_plot(legend, 0, 0, 0.3, 0.3)
  plot
  
  ## Special condition to 0 days
  i <- ifelse(i == 0, 1, i)
  
  finalPlot[[i]] <- plot
  
  # # Save the plot as a temporary file
  tmp_file <- tempfile(fileext = ".png")
  ggsave(tmp_file, plot, width = 16, height = 9, dpi = 100)
  
  # Return the temporary file path
  tmp_file_list[[i]] <- tmp_file
}

frame_files <- tmp_file_list[1:63] |> 
  unlist() |> 
  ## to revert the order of frames
  rev()

animation <- magick::image_animate(magick::image_read(frame_files), 
                                    # delay = 1,
                                    fps = 5, ## Change to a bigger number to a higher rate of fps and vice-versa
                                    optimize = T)
animation

## Gif saving
# Specify the output file path
output_file <- "img/extra_figures/bivariate_chrolopleth4x4.gif"

# Save the GIF animation
magick::image_write(animation, output_file)

# dates <- unique(joined_bind$date)
days <- unique(joined_bind$days)

joined_list <- list()

for (i in days) {
  
  cat("Moran's I for", i, "days before peak \n")
  
  ## Filtering for a day
  filtered <- joined_bind |> 
    filter(days == i)
  
  ## Calculating the local bivariate Moran
  lisa_bivariate <- local_bimoran(queen_w,
                                  filtered[,c("immune_alpha", "trend_delta")])
  
  # lisa_bivariate <- local_moran(queen_w, filtered[,c("trend_alpha")])
  
  lisa_labels <- lisa_labels(lisa_bivariate)
  lisa_colors <- lisa_colors(lisa_bivariate)
  
  ## Retrieving interesting measure from LISA
  filtered$local_bimoran <- lisa_values(lisa_bivariate)
  # filtered$local_moran_mean <- lisa_values(lisa_mean)
  # filtered$local_moran_immune <- lisa_values(lisa_immune)
  filtered$pvalues <- lisa_pvalues(lisa_bivariate)
  filtered$clusters <- lisa_clusters(lisa_bivariate, cutoff = 0.05)
  filtered$labels <- sapply(filtered$clusters, function(x){return(lisa_labels[[x+1]])})
  filtered$colors <- sapply(filtered$clusters, function(x){return(lisa_colors[[x+1]])})
  
  p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
  
  filtered$plabels <- sapply(filtered$pvalues, function(x){
    if (x <= 0.001) return(p_labels[4])
    else if (x <= 0.01) return(p_labels[3])
    else if (x <= 0.05) return (p_labels[2])
    else return(p_labels[1])})
  
  i <- ifelse(i == 0, 1, i)
  
  joined_list[[i]] <- filtered
  
}

joined_df <- bind_rows(joined_list) |> 
  mutate(hexid = as.character(hexid)) |>
  left_join(hexes) |>
  st_as_sf() |> 
  st_transform(crs = 26915)

## Saving the data.frame for the bivariate local Moran's I
# vroom::vroom_write(x = joined_df, 
#                    file = "data-products/bivariate_moran.csv")

## US Counties
us_states <- tigris::states(cb = T) |> 
  st_transform(crs = st_crs(hexes)) |> 
  # tigris::shift_geometry() |> 
  filter(!NAME %in% c("Alaska",
                      "Hawaii",
                      "Puerto Rico", 
                      "Guam", 
                      "Commonwealth of the Northern Mariana Islands",
                      "American Samoa",
                      "United States Virgin Islands"))


## Color palette
palette <- "Purple-Yellow"

figS6 <- joined_df |> 
  # filter(!is.na(local_bimoran)) |> 
  filter(days %in% c(0, 7, 14, 21, 28, 56)) |>
  ggplot()+
  geom_sf(aes(fill = pvalues,
              color = pvalues))+
  geom_sf(data = us_states,
          color = "deeppink4",
          fill = "transparent")+
  colorspace::scale_color_continuous_sequential(name = "p-values",
                                                palette = palette)+
  colorspace::scale_fill_continuous_sequential(name = "p-values",
                                               palette = palette)+
  theme_void(base_size = 12)+
  # labs(title = "Bivariate Moran's I over TSA, by days before the national peak",
  #      subtitle = "p_values, TSA[Infections] Delta vs TSA[Immunity] Alpha")+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(2, "cm"))+
  facet_wrap(.~days)
figS6

ggsave(filename = "img/extra_figures/figS6.png",
       plot = figS6,
       width = 16,
       height = 9,
       dpi = 100)

ggsave(filename = "img/extra_figures/figS6.pdf",
       plot = figS6,
       width = 16,
       height = 9,
       dpi = 100)

figS7 <- joined_df |> 
  # filter(!is.na(local_bimoran)) |> 
  filter(days %in% c(0, 7, 14, 21, 28, 56)) |>
  # filter(plabels == "p <= 0.001") |>
  ggplot()+
  geom_sf(aes(fill = plabels,
              color = plabels))+
  geom_sf(data = us_states,
          color = "deeppink4",
          fill = "transparent")+
  colorspace::scale_color_discrete_sequential(name = "p-values categories", 
                                              palette = palette)+
  colorspace::scale_fill_discrete_sequential(name = "p-values categories", 
                                             palette = palette)+
  theme_void(base_size = 12)+
  # labs(title = "Bivariate Moran's I over TSA, by days before the national peak",
  #      subtitle = "p_labels, TSA[Infections] Delta vs TSA[Immunity] Alpha")+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  facet_wrap(.~days)
figS7

ggsave(filename = "img/extra_figures/figS7.png",
       plot = figS7,
       width = 16,
       height = 9,
       dpi = 100)

ggsave(filename = "img/extra_figures/figS7.pdf",
       plot = figS7,
       width = 16,
       height = 9,
       dpi = 100)

figS8 <- joined_df |> 
  filter(days %in% c(0, 7, 14, 21, 28, 56)) |>
  ggplot()+
  geom_sf(aes(fill = labels,
              color = labels))+
  geom_sf(data = us_states,
          color = "deeppink4",
          fill = "transparent")+
  scale_fill_manual(name = "Moran's I cluster categories",
                    values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  scale_color_manual(name = "Moran's I cluster categories",
                     values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))+
  facet_wrap(.~days)
figS8

ggsave(filename = "img/extra_figures/figS8.png",
       plot = figS8,
       width = 16,
       height = 9,
       dpi = 100)

ggsave(filename = "img/extra_figures/figS8.pdf",
       plot = figS8,
       width = 16,
       height = 9,
       dpi = 100)

## Figure.5 - draft

## Alpha
alpha_trend <- CAR_df_preomicron |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (alpha_peak-128),
                            to = (alpha_peak),
                            length.out = 128)) |>
  mutate(days = (alpha_peak - date),
         trend_alpha = mean,
         wave = "alpha") |>
  select(days, hexid, trend_alpha)

## Delta
delta_trend <- CAR_df_preomicron |>
  filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
  filter(date %in% seq.Date(from = (delta_peak-128),
                            to = (delta_peak),
                            length.out = 128)) |>
  mutate(days = (delta_peak - date),
         trend_delta = mean,
         wave = "delta") |>
  select(days, hexid, trend_delta)

joined_bind <- full_join(alpha_trend, delta_trend)

# bipalette <- c(
#   "1-1" = "#d3d3d3", # low x, low y
#   "2-1" = "#c2a0a6",
#   "3-1" = "#b16d79",
#   "4-1" = "#9e3547", # high x, low y
#   "1-2" = "#a3b5c7",
#   "2-2" = "#96899d",
#   "3-2" = "#895e72",
#   "4-2" = "#7a2d43",
#   "1-3" = "#7397bb",
#   "2-3" = "#697394",
#   "3-3" = "#604e6b",
#   "4-3" = "#56263f",
#   "1-4" = "#4279b0", # low x, high y
#   "2-4" = "#3c5c8b",
#   "3-4" = "#373f65",
#   "4-4" = "#311e3b" # high x, high y
# )

# bipalette <- c(
#   "1-1" = "#d3d3d3", # low x, low y
#   "2-1" = "#b6cdcd",
#   "3-1" = "#97c5c5",
#   "4-1" = "#75bebe",
#   "5-1" = "#52b6b6", # high x, low y
#   "1-2" = "#cab6c5",
#   "2-2" = "#aeb0bf",
#   "3-2" = "#91aab9",
#   "4-2" = "#70a4b2",
#   "5-2" = "#4e9daa",
#   "1-3" = "#c098b9",
#   "2-3" = "#a593b3",
#   "3-3" = "#898ead",
#   "4-3" = "#6b89a6",
#   "5-3" = "#4a839f",
#   "1-4" = "#b77aab",
#   "2-4" = "#9e76a6",
#   "3-4" = "#8372a0",
#   "4-4" = "#666e9a",
#   "5-4" = "#476993",
#   "1-5" = "#ad5b9c", # low x, high y
#   "2-5" = "#955898",
#   "3-5" = "#7c5592",
#   "4-5" = "#60528d",
#   "5-5" = "#434e87" # high x, high y
# )

# bipalette <- c("1-1" = "#f3f3f3", 
#                "2-1" = "#b4d3e1", 
#                "3-1" = "#509dc2", 
#                "1-2" = "#f3e6b3", 
#                "2-2" = "#b3b3b3", 
#                "3-2" = "#376387", 
#                "1-3" = "#f3b300", 
#                "2-3" = "#b36600", 
#                "3-3" = "#000000")

# bipalette <- "DkViolet"
bipalette <- "DkViolet"
bidim <- 3
style <- "quantile"

data <- joined_bind |> 
  filter(days == 0) |> 
  biscale::bi_class(x = trend_alpha,
                    y = trend_delta,
                    style = style,
                    dim = bidim) |> 
  rename(bi_trend = bi_class)

# data_cum <- joined_bind |>
#   filter(days %in% seq(0,63, 1)) |>
#   group_by(hexid) |>
#   summarise(cum_trend_alpha = sum(trend_alpha),
#             cum_trend_delta = sum(trend_alpha)) |>
#   biscale::bi_class(x = cum_trend_alpha,
#                     y = cum_trend_delta,
#                     style = style,
#                     dim = bidim) |>
#   rename(bi_trend = bi_class)

# create map
map <- ggplot() +
  geom_sf(data = data |> 
            mutate(hexid = as.character(hexid)) |> 
            left_join(hexes |> 
                        filter(hexid != 6545)) |> ## excluding the hex which generate NULL weights), 
            st_as_sf() |> 
            st_transform(crs = 26915),  
          mapping = aes(fill = bi_trend),
          color = NA,
          size = 0.1, 
          show.legend = FALSE) +
  geom_sf(data = st_transform(us_states, crs = 26915),
          color = "white",
          fill = "transparent")+
  biscale::bi_scale_fill(pal = bipalette, dim = bidim)+
  # biscale::bi_scale_color(pal = bipalette, dim = bidim)+
  theme_void()
map

## Manually building the legend
leg <- tidyr::separate(data, 
                       bi_trend, 
                       into = c("x", "y"), 
                       sep = "-")

leg <- dplyr::mutate(leg, 
                     x = as.integer(x), 
                     y = as.integer(y))

bi_fill <- biscale::bi_pal(pal = "DkViolet", dim = 3, preview = F)

leg <- leg |> 
  mutate(bi_fill = case_when(x == 1 & y == 1 ~ "#cabed0",
                             x == 2 & y == 1 ~ "#bc7c8f",
                             x == 3 & y == 1 ~ "#ae3a4e",
                             x == 1 & y == 2 ~ "#89a1c8",
                             x == 2 & y == 2 ~ "#806a8a",
                             x == 3 & y == 2 ~ "#77324c",
                             x == 1 & y == 3 ~ "#4885c1",
                             x == 2 & y == 3 ~ "#435786",
                             x == 3 & y == 3 ~ "#3f2949"))
size <- 12
pad_width <- 3
pad_color <- NA
breaks_seq <- seq(from = 0.5, to = bidim+0.5, by = 1)
bi_breaks <- biscale::bi_class_breaks(.data = data,
                                      x = "trend_alpha",
                                      y = "trend_delta",
                                      style = "quantile",
                                      split = TRUE,
                                      clean_levels = T,
                                      dig_lab = 1,
                                      dim = bidim)

legend <- ggplot2::ggplot() + 
  ggplot2::geom_tile(data = leg, 
                     mapping = ggplot2::aes(x = x, y = y, 
                                            fill = bi_fill), 
                     lwd = pad_width, 
                     col = pad_color) + 
  ggplot2::scale_fill_identity() + 
  ggplot2::scale_x_continuous(
    breaks = breaks_seq,
    labels = bi_breaks$bi_x) +
  ggplot2::scale_y_continuous(
    breaks = breaks_seq,
    labels = bi_breaks$bi_y)+
  ggplot2::labs(x = "1st wave \n on November 19th, 2020 \n (infections/100k)", 
                y = "2nd wave \n on September 08th, 2021 \n (infections/100k)") + 
  biscale::bi_theme() + 
  ggplot2::theme_light() +
  ggplot2::theme(panel.border=element_blank(), 
                 axis.title.x = element_text(vjust = -.7, hjust = .5, size = size),
                 axis.title.y = element_text(vjust = .7, hjust = .5, size = size),
                 axis.line = element_line(size = .5, 
                                          arrow = arrow(type = "closed", 
                                                        length = unit(.07, "in"))), 
                 axis.ticks = element_blank(),
                 axis.text = element_text(),
                 panel.grid = element_blank(),
                 panel.background = element_blank(),
                 plot.background = element_blank()) + 
  lemon::coord_capped_cart(bottom="both", left = "both") 
legend

# combine map with legend
fig5a <- cowplot::ggdraw() +
  cowplot::draw_plot(map, 0, 0, 1, 1) +
  cowplot::draw_plot(legend, 0, 0.05, 0.20, 0.30)
fig5a

ggsave(filename = "img/extra_figures/fig5a.png",
       plot = fig5a,
       width = 16,
       height = 9, 
       dpi = 200)

ggsave(filename = "img/extra_figures/fig5a.pdf",
       plot = fig5a,
       width = 16,
       height = 9, 
       dpi = 200)

fig5b <- joined_df |> 
  filter(days == 63) |>
  ggplot()+
  geom_sf(mapping = aes(fill = labels,
          color = labels))+
  geom_sf(data = us_states,
          color = "deeppink4",
          fill = "transparent")+
  scale_fill_manual(name = "Moran's I",
                    values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  scale_color_manual(name = "Moran's I",
                     values = c("#FF0000","#f4ada8", "#a7adf9", "#0000FF", "#eeeeee"))+
  theme_void(base_size = 12)+
  theme(legend.position = "bottom",
        legend.title = element_text(hjust = 0.5),
        legend.title.position = "top",
        legend.key.width = grid::unit(1, "cm"))
fig5b

ggsave(filename = "img/extra_figures/fig5b.png",
       plot = fig5b,
       width = 16,
       height = 9, 
       dpi = 100)

ggsave(filename = "img/extra_figures/fig5b.pdf",
       plot = fig5b,
       width = 16,
       height = 9, 
       dpi = 100)

library(patchwork)

fig5 <- (fig5a | fig5b)
fig5

ggsave(plot = fig5,
       filename = "img/fig5.png",
       width = 16, 
       height = 9, 
       dpi = 100)

ggsave(plot = fig5,
       filename = "img/fig5.pdf",
       width = 16, 
       height = 9, 
       dpi = 100)

# ## Omicron-era dataset
# ## hexgrids
# dataset <- "omicronera"
# 
# ##Omicron-era
# CAR_df_omicronera <- vroom::vroom(paste0("data-products/tsa_",
#                                          dataset, 
#                                          ".csv"))
# 
# omicronba1_peak <- as.Date("2022-01-20")
# omicronba5_peak <- as.Date("2022-07-21")
# 
# ## Omicron trends
# omicronba1_trend <- CAR_df_omicronera |> 
#   filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
#   # mutate(hexid = as.character(hexid)) |>
#   # left_join(hexes) |>
#   # dplyr::select(hexid, date, mean, sd, geometry) |>
#   # st_as_sf()|>
#   # st_transform(crs = 26915)|>
#   filter(date %in% seq.Date(from = (omicronba1_peak-128), 
#                             to = (omicronba1_peak),
#                             length.out = 128)) |>
#   mutate(days = (omicronba1_peak - date),
#          trend_omicronba1 = mean,
#          wave = "Omicron BA.1") |> 
#   select(days, hexid, trend_omicronba1)
# 
# omicronba5_trend <- CAR_df_omicronera |> 
#   filter(hexid != 6545) |>  ## excluding the hex which generate NULL weights
#   # mutate(hexid = as.character(hexid)) |>
#   # left_join(hexes) |>
#   # dplyr::select(hexid, date, mean, sd, geometry) |>
#   # st_as_sf()|>
#   # st_transform(crs = 26915)|>
#   filter(date %in% seq.Date(from = (omicronba5_peak-128), 
#                             to = (omicronba5_peak),
#                             length.out = 128)) |>
#   mutate(days = (omicronba5_peak - date),
#          trend_omicronba5 = mean,
#          wave = "Omicron BA.5") |> 
#   select(days, hexid, trend_omicronba5)
# 
# ## Joining data.frames
# joined_bind_omicron <- left_join(omicronba1_trend, 
#                                  omicronba5_trend)
# 
# ## Bivariate color map to Figure.5
# style <- "quantile"
# bipalette <- "DkViolet2"
# variable1 <- "trend_omicronba1"
# variable2 <- "trend_omicronba5"
# bidim <- 2
# 
# ## Bivariate mutates
# dataset <- list()
# 
# for (i in unique(joined_bind_omicron$days)) {
#   
#   ## Filtering
#   data <- joined_bind_omicron |> 
#     filter(days == i) |>
#     biscale::bi_class(x = trend_omicronba1, 
#                       y = trend_omicronba5, 
#                       style = style, 
#                       dim = bidim) |> 
#     rename(bi_trend = bi_class) |>
#     # biscale::bi_class(x = immune_omicronba1,
#     #                   y = immune_omicronba5,
#     #                   style = style,
#     #                   dim = bidim) |>
#     # rename(bi_immune = bi_class) |>
#     # biscale::bi_class(x = immune_alpha,
#     #                   y = trend_delta,
#     #                   style = style,
#     #                   dim = bidim) |>
#     # rename(bi_immune_trend = bi_class) |>
#     mutate(hexid = as.character(hexid)) |>
#     left_join(hexes) |>
#     st_as_sf() |> 
#     st_transform(crs = 26915) |> 
#     mutate(dim = bidim,
#            style = style)
#   
#   ## Special condition to 0 days
#   i <- ifelse(i == 0, 1, i)
#   
#   dataset[[i]] <- data
#   
# }
# 
# joined_biclass_omicron <- bind_rows(dataset)
# 
# ## Figure.5 - draft
# data <- joined_biclass_omicron |> 
#   filter(days == 0)
# 
# bipalette <- "GrPink"
# 
# bi_breaks <- biscale::bi_class_breaks(.data = joined_biclass_omicron |> 
#                                         filter(days == 0),
#                                       x = "trend_omicronba1",
#                                       y = "trend_omicronba5",
#                                       style = style,
#                                       split = TRUE,
#                                       clean_levels = T,
#                                       dig_lab = 1,
#                                       dim = bidim)
# 
# # create map
# map <- ggplot() +
#   geom_sf(data = data, 
#           mapping = aes(fill = !!rlang::sym(bivariable),
#                         color = !!rlang::sym(bivariable)), 
#           size = 0.1, 
#           show.legend = FALSE) +
#   geom_sf(data = st_transform(us_states, crs = 26915),
#           color = "deeppink4",
#           fill = "transparent")+
#   biscale::bi_scale_fill(pal = bipalette, dim = bidim) +
#   biscale::bi_scale_color(pal = bipalette, dim = bidim) +
#   theme_minimal()
# map
# 
# legend <- biscale::bi_legend(pal = bipalette,
#                              dim = bidim,
#                              breaks = bi_breaks,
#                              xlab = str_c("Higher Alpha TSA"),
#                              ylab = str_c("Higher Delta TSA"),
#                              size = 8)
# 
# # combine map with legend
# figS8 <- cowplot::ggdraw() +
#   cowplot::draw_plot(map, 0, 0, 1, 1) +
#   cowplot::draw_plot(legend, 0, 0, 0.3, 0.3)
# figS8
# 
# # joined_bind_omicron <- CAR_df_omicronera |> 
# #   filter(hexid != 6545) |> 
# #   select(date, hexid, mean)
# 
# # guerry <- hexes |> 
# #   filter(hexid !=6545)
# # queen_w <- queen_weights(guerry)
# 
# # dates <- unique(joined_bind_omicron$date)
# days <- unique(joined_bind_omicron$days)
# 
# joined_list_omicron <- list()
# 
# for (i in days) {
#   
#   cat("Moran's I for", i, "days before peak \n")
#   
#   ## Filtering for a day
#   filtered <- joined_bind_omicron |> 
#     filter(days == i)
#   
#   ## Calculating the local bivariate Moran
#   lisa <- local_bimoran(queen_w, filtered[,c("trend_omicronba1", 
#                                              "trend_omicronba5")])
#   # lisa <- local_moran(queen_w, filtered[,"mean"])
#   lisa_labels <- lisa_labels(lisa)
#   
#   ## Retrieving interesting measure from LISA
#   # filtered$local_bimoran <- lisa_values(lisa)
#   filtered$local_moran <- lisa_values(lisa)
#   filtered$pvalues <- lisa_pvalues(lisa)
#   filtered$clusters <- lisa_clusters(lisa, cutoff = 0.05)
#   filtered$labels <- sapply(filtered$clusters, function(x){return(lisa_labels[[x+1]])})
#   
#   p_labels <- c("Not significant", "p <= 0.05", "p <= 0.01", "p <= 0.001")
#   
#   filtered$plabels <- sapply(filtered$pvalues, function(x){
#     if (x <= 0.001) return(p_labels[4])
#     else if (x <= 0.01) return(p_labels[3])
#     else if (x <= 0.05) return (p_labels[2])
#     else return(p_labels[1])})
#   
#   i <- ifelse(i == 0, 1, i)
#   
#   joined_list_omicron[[i]] <- filtered
#   
# }
# 
# joined_list_omicron <- bind_rows(joined_list_omicron) |> 
#   mutate(hexid = as.character(hexid)) |>
#   left_join(hexes) |>
#   st_as_sf() |> 
#   st_transform(crs = 26915)
# 
# figS6 <- joined_list_omicron |> 
#   # filter(days %in% c(0, 21, 42, 63, 128)) |> 
#   ggplot()+
#   geom_sf(aes(fill = pvalues))+
#   scale_fill_gradient(high = "thistle1", low = "deeppink1")+
#   theme_minimal()+
#   facet_wrap(.~days)+
#   theme(legend.position = "bottom",
#         legend.title = element_blank())
# figS6
# 
# 
# figS7 <- joined_list_omicron |> 
#   filter(days %in% c(0, 21, 42, 63, 128)) |> 
#   ggplot()+
#   geom_sf(aes(fill = labels))+
#   scale_fill_manual(values = c("#FF0000","#f4ada8", "#0000FF", "#a7adf9", "#eeeeee"))+
#   theme_minimal()+
#   facet_wrap(.~days)+
#   theme(legend.position = "bottom",
#         legend.title = element_blank())
# figS7
# 
# 
# 
# p_colors <- c("#eeeeee", "#84f576", "#53c53c", "#348124")
# plot(st_geometry(guerry) |> 
#        st_transform(crs=26915), 
#      col=sapply(lisa_p, function(x){
#        if (x <= 0.001) return(p_colors[4])
#        else if (x <= 0.01) return(p_colors[3])
#        else if (x <= 0.05) return (p_colors[2])
#        else return(p_colors[1])
#      }), 
#      border = "#333333", lwd=0.2)
# title(main = "Bivariate Local Moran Trend-alpha vs Trend-delta")
# legend('bottomleft', legend = p_labels, fill = p_colors, border = "#eeeeee")
# 
# 
# lisa_colors <- lisa_colors(lisa)
# lisa_labels <- lisa_labels(lisa)
# lisa_clusters <- lisa_clusters(lisa)
# 
# filtered$clusters <- lisa_clusters
# filtered$colors <- sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])})
# filtered$labels <- sapply(lisa_clusters, function(x){return(lisa_labels[[x+1]])})
# 
# filtered <- filtered |> 
#   mutate(hexid = as.character(hexid)) |>
#   left_join(hexes) |>
#   st_as_sf()|>
#   st_transform(crs = 26915)
# 
# ggplot()+
#   geom_sf(data = filtered,
#           aes(fill = labels))+
#   scale_fill_manual(values = rev(lisa_colors))
# 
# 
# 
# plot(st_geometry(guerry) |> 
#        st_transform(crs = 26915), 
#      col=sapply(lisa_clusters, function(x){return(lisa_colors[[x+1]])}), 
#      border = "#333333", lwd=0.2)
# title(main = "Bivariate Local Moran Trend-alpha vs Trend-delta")
# legend('bottomleft', 
#        legend = lisa_labels, 
#        fill = lisa_colors, 
#        border = "#eeeeee")
# 
# immune_hex <- st_read("data-products/geo-hexes/hexid-observations_immunity.geojson") |> 
#   mutate(date = as.Date(date))
# 
# immune_alpha <- immune_hex |> 
#   filter(date %in% seq.Date(from = (alpha_peak-63), 
#                             to = (alpha_peak),
#                             length.out = 63)) |>
#   mutate(days = (alpha_peak - date),
#          wave = "alpha") |> 
#   rename(immune_alpha = immune) |> 
#   select(days, hexid, immune_alpha, days)
# 
# immune_alpha_map <- ggplot()+
#   geom_sf(data = immune_alpha |> 
#             filter(days %in% c(63, 
#                                # 42, 21, 
#                                14, 0)),
#           aes(fill = immune_alpha))+
#   scale_fill_distiller(palette = "PuOr")+
#   facet_wrap(.~days, nrow = 1)+
#   theme_minimal()
# immune_alpha_map
# 
# immune_delta <- immune_hex |> 
#   filter(date %in% seq.Date(from = (delta_peak-63), 
#                             to = (delta_peak),
#                             length.out = 63)) |>
#   mutate(days = (delta_peak - date),
#          wave = "delta") |> 
#   rename(immune_delta = immune) |> 
#   select(days, hexid, immune_delta, days)
# 
# immune_delta_map <- ggplot()+
#   geom_sf(data = immune_delta |> 
#             filter(days %in% c(63, 
#                                # 42, 21, 
#                                14, 0)),
#           aes(fill = immune_delta))+
#   scale_fill_distiller(palette = "PuOr")+
#   facet_wrap(.~days, nrow = 1)+
#   theme_minimal()
# immune_delta_map
# 
# fig_immune_maps <- (immune_alpha_map/immune_delta_map)
# fig_immune_maps
# 
# summed <- left_join(alpha_trend |>
#                       st_drop_geometry() |>
#                       select(days, hexid, trend_alpha),
#                     delta_trend |>
#                       st_drop_geometry() |>
#                       select(days, hexid, trend_delta)) |>
#   left_join(immune_alpha |>
#               st_drop_geometry()) |>
#   left_join(immune_delta |>
#               st_drop_geometry())
# 
# ## Summarising the correlation between trend surface and immunity
# summed_by_days <- summed |>
#   group_by(days) |>
#   summarise(across(trend_alpha:immune_delta, ~ mean(.x, na.rm = T)))
# 
# summed_by_days <- inner_join(summed_by_days|> 
#                                pivot_longer(cols = trend_alpha:trend_delta,
#                                             names_to = "wave",
#                                             values_to = "trend") |> 
#                                mutate(wave = str_remove(wave, "trend_")) |> 
#                                select(days, wave, trend),
#                              summed_by_days|>
#                                pivot_longer(cols = immune_alpha:immune_delta,
#                                             names_to = "wave",
#                                             values_to = "immune") |> 
#                                mutate(wave = str_remove(wave, "immune_")) |> 
#                                select(days, wave, immune)) 
# 
# 
# scaleFactor <- max(summed_by_days$trend, 
#                    na.rm = T)/max(summed_by_days$immune, 
#                                   na.rm = T)
# 
# 
# # 
# trend_immune_plt <- summed_by_days |>
#   filter(days >= 7) |>
#   ggplot()+
#   geom_line(aes(x = days, 
#                 y = trend, 
#                 color = wave))+
#   geom_line(aes(x = days, 
#                 y = immune*scaleFactor, 
#                 color = wave))+
#   theme_minimal()+
#   scale_x_reverse(name = "Days before the peak")+
#   scale_y_continuous(name = "Trend surface value",
#                      sec.axis = sec_axis(~./scaleFactor, 
#                                          labels = scales::label_percent(),
#                                          name = "Percentual \n of population immune (%)"))
# trend_immune_plt
# 
# library(patchwork)
# 
# immune_maps <-  (trend_immune_plt | 
#                    ((immune_alpha_map/immune_delta_map)))
# immune_maps
# 
# joined <- list()
# 
# for (i in 2:63) {
#   alpha <- alpha_trend |> 
#     filter(days == i) |> 
#     st_drop_geometry() |>
#     select(days, hexid, trend_alpha) |> 
#     left_join(immune_alpha |> 
#                 st_drop_geometry() |> 
#                 select(days, hexid, immune_alpha))
#   
#   delta <- delta_trend |> 
#     filter(days == i) |> 
#     st_drop_geometry() |>
#     select(days, hexid, trend_delta) |> 
#     left_join(immune_delta |> 
#                 st_drop_geometry() |> 
#                 select(days, hexid, immune_delta))
#   
#   
#   joined[[i]] <- inner_join(alpha, delta)|>
#     left_join(hexes) |> 
#     st_as_sf() |> 
#     mutate(nb = sfdep::st_contiguity(geometry),
#            wt = sfdep::st_weights(nb, allow_zero = T, style = "W"))
#   
#   
#   joined[[i]] <- joined[[i]] |> 
#     mutate(moran_bv = sfdep::local_moran_bv(trend_alpha, trend_delta, nb, wt))|> 
#     tidyr::unnest(moran_bv)
#   
#   cat(i, "days before peak done! \n")
#   
#   
#   # |>
#   #   mutate(moran_alpha = local_moran(trend_alpha, nb, wt),
#   #          moran_delta = local_moran(trend_delta, nb, wt)) |>
#   #   tidyr::unnest(moran_alpha:moran_delta, names_sep = "_")|>
#   #   mutate(moran_alpha_pysal = ifelse(moran_alpha_p_folded_sim <= 0.1, 
#   #                                     as.character(moran_alpha_pysal), 
#   #                                     NA),
#   #          moran_delta_pysal = ifelse(moran_delta_p_folded_sim <= 0.1, 
#   #                                     as.character(moran_delta_pysal), 
#   #                                     NA))
#   
# }
# 
# ## bivariate Local Moran for the trends between trend on alpha and delta
# 
# joined_bind <- bind_rows(joined)
# 
# joined_filter <- joined_bind |> 
#   filter(days %in% c(63, 42, 21, 14, 7),
#          p_sim <= 0.001) |> 
#   st_drop_geometry() |>
#   left_join(hexes) |>
#   st_as_sf() |>
#   st_transform(crs=26915) |> 
#   ggplot()+
#   geom_sf(aes(fill = Ib))+
#   facet_wrap(.~days)+
#   scale_fill_fermenter(palette = "PuOr")+
#   theme_minimal()
# joined_filter
# 
# immune_maps <- ggplot()+
#   geom_sf(data = immune_alpha |> 
#             filter(days == 63),
#           aes(fill = immune_alpha))+
#   # geom_sf(data = immune_delta |> 
#   #           filter(days == 63),
#   #         aes(fill = immune_delta))+
#   theme_minimal()+
#   scale_fill_gradient(low = "thistle1", high = "deeppink4")
# immune_maps
# 
# joined_bind$ |> 
#   group_by(days) |> 
#   summarise(across(trend_alpha:immune_delta, .~ mean(.x, na.rm = T)))
# 
# 
# figS4 <- ggplot(data = joined,
#                 aes(x = days, y = mean, color = wave, group = hexid))+
#   geom_violin(position = position_dodge())
# figS4
# 
# ## Contour plots
# hexes_boundary <- st_union(hexes |> 
#                              filter(as.integer(hexid) < 7662) |> 
#                              st_transform(crs = 26915))
# 
# hexes_filtered <- hexes |> 
#   filter(as.integer(hexid) < 7662) |> 
#   st_transform(crs = 26915)
# 
# ## Summary
# summary(CAR_df_preomicron$mean)
