rm(list = ls())
gc()

## Loading auxiliary functions
source("scripts/plot_functions.R")

## Loading databases
hexes <- sf::st_read("data-products/geo-hexes/hexes.geojson")

hexes_boundary <- sf::st_union(hexes)

features_preomicron <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt.geojson")|> 
  # filtering to only show alphas >0, 
  # and the maximum value between a_ij and a_ji
  dplyr::filter(alpha>0, alpha == pmax(alpha),
                .by = c(j,date_week))

features_omicronera <- sf::st_read("data-products/geo-hexes/vectors/vectors_weekly_regardless_rt_omicronera.geojson")|> 
  # filtering to only show alphas >0, 
  # and the maximum value between a_ij and a_ji
  dplyr::filter(alpha>0, alpha == pmax(alpha),
                .by = c(j,date_week))

library(tidyverse)
library(magick)

weeks <- sort(unique(features_preomicron$date_week))

frame_files <- lapply(weeks, plt_preomicron, features_preomicron, hexes_boundary, FALSE)
frame_files <- frame_files |> 
  unlist()

image_list <- lapply(frame_files, image_read)

gif <- lapply(image_list, function(x){
  x <- image_resize(x, 
                    '4800x2700!') |>  
    image_morph() |> 
    image_animate(optimize = TRUE)
})

convolve_list <- lapply(image_list, function(x){
  x <- x |> 
    image_convolve("Gaussian") |> 
    image_negate() 
  return(x)
})


