## Libraries
library(biscale)
library(tidyverse)
library(sf)

ImmuneHexes <- sf::st_read("data-products/geo-hexes/hexid-immunity.geojson") 

immunityTotalGeo <- sf::st_read("data-products/geo-hexes/hex_immune.geojson")

ImmuneHexes <- ImmuneHexes |> 
  select(-immunePC) |> 
  mutate(across(starts_with("immune."), ~ (.x/population)*1e5, .names = "{.col}PC")) |> 
  mutate(across(ends_with("PC"), ~ if_else(.x < 0, 0, .x)))

colnames(ImmuneHexes) <- str_remove(colnames(ImmuneHexes), pattern = ".count")

## Bivariate color map to Figure.5
style <- "quantile"
bipalette <- "DkViolet2"
variable1 <- "immune.vaccinatedPC"
variable2 <- "immune.infectedPC"
bidim <- 3

## Filtering for specific dates to make the figure
ImmuneHexesFilt <- ImmuneHexes |> 
  filter(date %in% c(alpha_peak, delta_peak))|> 
  bi_class(x = immune.infectedPC,
           y = immune.vaccinatedPC, 
           style = "fisher",
           dim = bidim,
           dig_lab = 0)

bipalette_immune <- c("1-1" = "#f3f3f3", 
               "2-1" = "#b4d3e1", 
               "3-1" = "#509dc2", 
               "1-2" = "#f3e6b3", 
               "2-2" = "#b3b3b3", 
               "3-2" = "#376387", 
               "1-3" = "#f3b300", 
               "2-3" = "#b36600", 
               "3-3" = "#000000")

data_alpha_peak <- ImmuneHexesFilt |> 
  filter(date == alpha_peak) |> 
  # bi_class(x = immune.infected,
  #          y = immune.vaccinated, 
  #          style = "quantile",
  #          dim = 4,
  #          dig_lab = 0) |> 
  sf::st_transform(crs = 26915)

data_delta_peak <- ImmuneHexesFilt |> 
  filter(date == delta_peak) |> 
  # bi_class(x = immune.infected,
  #          y = immune.vaccinated, 
  #          style = "quantile",
  #          dim = 4,
  #          dig_lab = 0) |> 
  sf::st_transform(crs = 26915)

# dataA <- ImmuneHexesFilt |>
#   filter(date == "2021-01-01") |> 
#   # bi_class(x = immune.infected,
#   #          y = immune.vaccinated, 
#   #          style = "quantile",
#   #          dim = 4,
#   #          dig_lab = 0) |> 
#   sf::st_transform(crs = 26915)
# 
# dataB <- ImmuneHexesFilt |>
#   filter(date == "2021-05-01") |> 
#   # bi_class(x = immune.infectedPC,
#   #          y = immune.vaccinatedPC, 
#   #          style = "quantile",
#   #          dim = 4,
#   #          dig_lab = 0) |> 
#   st_transform(crs = 26915)
# 
# dataC <- ImmuneHexesFilt |>
#   filter(date == "2021-09-01") |> 
#   # bi_class(x = immune.infectedPC,
#   #          y = immune.vaccinatedPC, 
#   #          style = "quantile",
#   #          dim = 4,
#   #          dig_lab = 0) |> 
#   st_transform(crs = 26915)
# 
# dataD <- ImmuneHexesFilt |>
#   filter(date == "2021-12-01") |> 
#   # bi_class(x = immune.infectedPC,
#   #          y = immune.vaccinatedPC, 
#   #          style = "quantile",
#   #          dim = 4,
#   #          dig_lab = 0) |> 
#   st_transform(crs = 26915)

## Maps
mapA <- ggplot() +
  geom_sf(data = data_alpha_peak,
          aes(fill = bi_class,
              color = bi_class),
          size = 0.1, 
          show.legend = FALSE) +
  geom_sf(data = us_states,
          color = "deeppink4",
          fill = "transparent")+
  biscale::bi_scale_fill(pal = bipalette_immune, dim = bidim) +
  biscale::bi_scale_color(pal = bipalette_immune, dim = bidim) +
  biscale::bi_theme()+
  labs(subtitle = "Immunity at peak of 1st wave")
mapA

mapB <- ggplot() +
  geom_sf(data = data_delta_peak,
          aes(fill = bi_class,
              color = bi_class),
          size = 0.1, 
          show.legend = FALSE) +
  geom_sf(data = us_states,
          color = "deeppink4",
          fill = "transparent")+
  biscale::bi_scale_fill(pal = bipalette_immune, dim = bidim) +
  biscale::bi_scale_color(pal = bipalette_immune, dim = bidim) +
  biscale::bi_theme()+
  labs(subtitle = "Immunity at peak of 2nd wave")
mapB

# mapC <- ggplot(data = dataC, 
#                mapping = aes(fill = bi_class,
#                              color = bi_class)) +
#   geom_sf(size = 0.1, 
#           show.legend = FALSE) +
#   biscale::bi_scale_fill(pal = bipalette, dim = bidim) +
#   biscale::bi_scale_color(pal = bipalette, dim = bidim) +
#   biscale::bi_theme()+
#   labs(title = "1 September 2021")
# mapC
# 
# mapD <- ggplot(data = dataD, 
#                mapping = aes(fill = bi_class,
#                              color = bi_class)) +
#   geom_sf(size = 0.1, 
#           show.legend = FALSE) +
#   biscale::bi_scale_fill(pal = bipalette, dim = bidim) +
#   biscale::bi_scale_color(pal = bipalette, dim = bidim) +
#   biscale::bi_theme()+
#   labs(title = "1 December 2021")
# mapD

## Setting the bi_class breaks outside the loop to have fixed breaks over the animation
bi_breaks <- biscale::bi_class_breaks(.data = ImmuneHexesFilt,
                                      x = "immune.infectedPC",
                                      y = "immune.vaccinatedPC",
                                      style = "fisher",
                                      split = TRUE,
                                      clean_levels = TRUE,
                                      dig_lab = 1,
                                      dim = bidim)

# bi_breaks[[1]] <- na.omit(bi_breaks[[1]])
# bi_breaks[[2]] <- na.omit(bi_breaks[[2]])

legend_immune <- biscale::bi_legend(pal = bipalette_immune,
                             dim = bidim,
                             breaks = bi_breaks,
                             # flip_axes = T,
                             xlab = "Higher Immunity by infection",
                             ylab = "Higher Immunity by vaccination",
                             size = 8)
legend_immune

## Patchwork
library(patchwork)
patchwork <- ((map/(legend | legend_immune))|(mapA/mapB))
patchwork
