suppressPackageStartupMessages( library(sf) )
suppressPackageStartupMessages( library(geojsonio) )
library(magrittr, warn.conflicts = F)
library(ggplot2,  warn.conflicts = F)
library(dplyr,    warn.conflicts = F)
library(stringr,  warn.conflicts = F)
library(docopt,   warn.conflicts = F)
library(cli,      warn.conflicts = F)
library(readr,    warn.conflicts = F)
library(purrr,    warn.conflicts = F)
library(progress, warn.conflicts = F)

## Hexgrid
hexgrid <- st_read("data-products/geo-hexes/hexes.geojson") |> 
    filter(as.integer(hexid) < 7662) |> ## Filtering out Puerto Rico hexes
    st_transform(crs = 26915) 

## New hexgrid with Meta 30m population
meta_hexgrid_pop <- st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
    filter(as.integer(hexid) < 7662) |> ## Filtering out Puerto Rico hexes
    st_transform(crs = 26915) |>
    rename(meta_pop = metapop_30m)

ggplot() + geom_sf(data=meta_hexgrid_pop, mapping=aes(fill=population))
sum(meta_hexgrid_pop$meta_pop, na.rm = TRUE) / 1e6

cbg_hexgrid_pop <- st_read("data-products/geo-hexes/cbg_population.geojson") |> 
    filter(as.integer(hexid) < 7662) |> ## Filtering out Puerto Rico hexes
    st_transform(crs = 26915) %>% 
    rename(cbg_pop = population)

ggplot() + geom_sf(data=cbg_hexgrid_pop, mapping=aes(fill=cbg_pop))
sum(cbg_hexgrid_pop$cbg_pop, na.rm = TRUE) / 1e6

### plot the difference between the two populations 

diff_hexgrid_population <- st_join(meta_hexgrid_pop, cbg_hexgrid_pop, by=c("hexid", "geometry")) %>%
                           mutate(pop_difference = cbg_pop - meta_pop, 
                                  diff0 = ifelse(pop_difference == 0, 
                                                 TRUE, FALSE), 
                                  diffTen = ifelse(abs(pop_difference) >= 10, 
                                                      TRUE, FALSE),
                                  diffHund = ifelse(abs(pop_difference) >= 1e2, 
                                                      TRUE, FALSE),
                                  diffTho = ifelse(abs(pop_difference) >= 1e3, 
                                                    TRUE, FALSE),
                                  diffTenTho = ifelse(abs(pop_difference) >= 1e4, 
                                                       TRUE, FALSE),
                                  diffHundTho = ifelse(abs(pop_difference) >= 1e5, 
                                                      TRUE, FALSE),
                                  diffHalfMil = ifelse(abs(pop_difference) >= 5e5, 
                                                    TRUE, FALSE),
                                  diff1Mil = ifelse(abs(pop_difference) >= 1e6, 
                                                    TRUE, FALSE),
                                  diff2Mil = ifelse(abs(pop_difference) >= 2e6, 
                                                    TRUE, FALSE)) 


ggplot(data=diff_hexgrid_population) + 
    geom_point(aes(y=pop_difference/1e6, x = 1:nrow(diff_hexgrid_population))) + 
    theme_minimal()


library(viridis)

ggplot() + geom_sf(data=diff_hexgrid_population, mapping=aes(fill=pop_difference/1e6)) + 
    theme_minimal() + ggtitle("cbg population - meta population")

ggplot() + geom_sf(data=diff_hexgrid_population, mapping=aes(fill=abs(pop_difference/1e6))) + 
    scale_fill_viridis_c(option = "A") +
    theme_minimal() + ggtitle("cbg population - meta population")

ggplot() + 
    geom_sf(data=diff_hexgrid_population, mapping=aes(geometry = geometry),
            fill="turquoise") + 
    geom_sf(data=diff_hexgrid_population %>% filter(diff0 ==TRUE), mapping=aes(geometry = geometry),
            fill = "grey") +
    geom_sf(data=diff_hexgrid_population %>% filter(diffTen ==TRUE), mapping=aes(geometry = geometry),
            fill = "blue") +
    geom_sf(data=diff_hexgrid_population %>% filter(diffHund ==TRUE), mapping=aes(geometry = geometry),
            fill = "forestgreen") +
    geom_sf(data=diff_hexgrid_population %>% filter(diffTho ==TRUE), mapping=aes(geometry = geometry),
            fill = "green") +
    geom_sf(data=diff_hexgrid_population %>% filter(diffTenTho ==TRUE), mapping=aes(geometry = geometry),
            fill = "yellowgreen") +   
    geom_sf(data=diff_hexgrid_population %>% filter(diffHundTho ==TRUE), mapping=aes(geometry = geometry),
            fill = "yellow") +
    geom_sf(data=diff_hexgrid_population %>% filter(diffHalfMil ==TRUE), mapping=aes(geometry = geometry),
            fill = "gold") +
    geom_sf(data=diff_hexgrid_population %>% filter(diff1Mil ==TRUE), mapping=aes(geometry = geometry),
            fill = "orange") +
    geom_sf(data=diff_hexgrid_population %>% filter(diff2Mil ==TRUE), mapping=aes(geometry = geometry),
            fill = "red") +
    theme_minimal() + ggtitle("cbg population - meta population")

###############################################################################
###############################################################################
###############################################################################

### Compare observations
observationsFips <- st_read("data-products/geo-hexes/observations_preomicron.shp") |> 
  st_cast(to = "POLYGON")

ggplot() + geom_sf(aes(fill = "infections"))
