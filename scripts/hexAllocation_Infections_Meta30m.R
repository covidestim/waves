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
hexgrid_pop <- st_read("data-products/geo-hexes/meta_population/hexgrid_meta30m_population.geojson") |> 
  filter(as.integer(hexid) < 7662) |> ## Filtering out Puerto Rico hexes
  st_transform(crs = 26915) |>
  rename(population = metapop_30m)

## Observations
observationsFips <- st_read("data-products/geo-hexes/observations_fullmodel.shp") |> 
  st_cast(to = "POLYGON")

#### Also set a test date for the plots throughout the workflow
testDate <- "2022-01-20"

### Explore with a plot
ggplot() +
  geom_sf(observationsFips |>
            filter(date == testDate),
          mapping=aes(fill=infctPC)) +
  geom_sf(observationsFips %>% 
            filter(infctns == 0,
                   date == testDate), 
          mapping = aes(), 
          fill="green")+
  theme_minimal() +
  scale_fill_viridis_c()

###############################################################################
################## ~~FIRST: AREA WEIGHTED POPULATION TO HEXES~~ ###############
################## SECOND: AREA WEIGHTED INFECTION TO HEXES ###################
##### THIRD: JOIN THE POP AND INFECTIONS ACROSS HEXES. CALCULATE ##############
########################### INFECTIONS PER CAPITA #############################
###############################################################################

###############################################################################
###############################   Population    ###############################
###############################################################################

### Validate that the population and hexgrid are compatible 
areal::ar_validate(hexgrid_pop, hexgrid, varList = "population", method = "aw", verbose = TRUE)

### Transform back to the projection of interest 
hexgrid_popTrans <- st_transform(hexgrid_pop, crs = 4269)
hexgrid_popTrans$hexid <- as.numeric(hexgrid_popTrans$hexid) ### used for filtering

### Plot the population across the hexes 
# ggplot() + 
#   geom_sf(hexgrid_popTrans, 
#                    mapping=aes(fill=log10(population))) +
#   geom_sf(hexgrid_popTrans |> 
#             filter(population == 0), 
#           mapping=aes(), fill="green")+
#   khroma::scale_fill_acton(reverse = T)+
#   theme_minimal()

###############################################################################
###############################   Infections    ###############################
###############################################################################

hexgrid_pop$interpolation_weight <- hexgrid_pop[["population"]]
weight_column <- "interpolation_weight"
to_id <- "hexid"
from_id <- "from_id"

InfPopAll <- data.frame(hexid = 0,
                        population = 0,
                        immune = 0, 
                        geometry = hexgrid_pop$geometry[1],
                        date = NA)

## Checking if CRS are equal
if(st_crs(hexgrid_pop) == st_crs(observationsFips)){
  hexgrid_pop <- st_transform(hexgrid_pop, crs = 4326)
  observationsFips <- st_transform(observationsFips, crs = 4326)
}

### Create an empty dataframe to hold the results
InfPopAll <- data.frame(hexid = 0,
                        population = 0,
                        infctns = 0,
                        geometry = hexgrid_pop$geometry[1],
                        date = NA)

# testDate <- "2020-07-26"

### Write a loop for the allocation
filtDate <- unique(na.omit(observationsFips$date))

### Write a loop for the allocation
for (i in filtDate){
  
  print(as.Date(i))
  
  observationsFilt <- observationsFips %>%
    filter(date == i) %>%
    select(infctns, geometry)
  
  observationsFilt$from_id <- as.character(1:nrow(observationsFilt))
  weight_sym <- rlang::sym(weight_column)
  from_id_sym <- rlang::sym(from_id)
  to_id_sym <- rlang::sym(to_id)
  
  observationsFilt <- st_transform(observationsFilt, 
                                   crs = 26915)
  hexgrid_pop <- st_transform(hexgrid_pop |> 
                                    filter(!is.na(interpolation_weight)), 
                                  crs = 26915)
  
  ## Creating denominators, it is the counties geometries with weight for each of them.
  denominators <- observationsFilt %>%
    ## When we join the geometries, we make it slower to produce the intersection
    sf::st_join(hexgrid_pop, left = FALSE) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!from_id_sym) %>%
    dplyr::summarize(weight_total = sum(!!weight_sym,
                                        na.rm = TRUE))
  
  ## Creating intersections, it is the left_join of infections counts on counties geometries with the denominators, same geometries. Then, we intersect the hex with populations counts. After we group by the mix of geometries, counties and hexes, 
  intersections <- observationsFilt |>
    dplyr::left_join(denominators, by = from_id) |> 
    sf::st_intersection(hexgrid_pop)%>%
    # dplyr::mutate(intersection_id = dplyr::row_number()) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(hexid) %>%
    dplyr::mutate(intersection_value = sum(!!weight_sym, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(hexid,.keep_all = TRUE) %>%
    dplyr::mutate(weight_coef = intersection_value/weight_total) %>%
    dplyr::select(!!from_id_sym, !!to_id_sym, intersection_value,
                  weight_coef)
  
  interpolated <- observationsFilt %>%
    sf::st_drop_geometry() %>%
    dplyr::left_join(intersections,  by = from_id) %>%
    ## Certify that the hexid is a character value, the next mutate will modify ALL numeric variables.
    dplyr::mutate(hexid = as.character(hexid)) |> 
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), .fns = ~(.x * weight_coef)))  %>%
    dplyr::select(-weight_coef) %>%
    dplyr::group_by(!!to_id_sym) %>%
    dplyr::summarize(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                   .fns = ~sum(.x, na.rm = TRUE))) %>%
    dplyr::select(-intersection_value)
  
  output_shapes <- hexgrid_pop %>%
    dplyr::select(!!to_id_sym, population) %>%
    ## Certify hexid are character, to be able to join with interpolated values
    dplyr::mutate(hexid = as.character(hexid)) |> 
    dplyr::left_join(interpolated, by = to_id)# %>%
  # rename(geometry = x)
  
  # ggplot() + geom_sf(output_shapes,
  #                    mapping = aes(fill = log10(infctns+1)))+
  #   theme_minimal() +
  #   scale_fill_gradient(low = "thistle1", high = "deeppink4", na.value = "green")  +
  #   geom_sf(output_shapes %>% filter(infctns == 0),
  #           mapping = aes(), fill="green")+
  #   labs(title = "With CBGs")
  
  ##### Date
  output_shapes$date <- as.Date(i, origin='1970-01-01')
  
  ##### Add to the big dataframe
  InfPopAll <- rbind(InfPopAll, output_shapes)
}

###############################################################################
##### Create infections per capita 
###############################################################################
### Need to remove the first row of the InfAreaAll dataframe because it was 
### for formatting only. 
hexObservationsAll <- InfPopAll[-1,]  |> 
  rename(infections = infctns) |> 
  mutate(infectionsPC = ifelse(population == 0, 
                               0, 
                               (infections/population)*1e5))

### Create an SF version for plots 
hexObservationsAllSF <- st_as_sf(hexObservationsAll)

testDate <- unique(na.omit(observationsFips$date))[12]

### Check if we have the same number of infections 
if(sum(observationsFips %>% 
       st_drop_geometry() %>% 
       filter(date == testDate) %>% 
       select(infctns)) - 
   sum(hexObservationsAll %>% 
       filter(date == testDate, is.na(infections)==FALSE) %>% 
       select(infections)) > 1) {
  print("signicant loss of infections in interpolation step")
}

# hexgrid$hexid <- as.numeric(hexgrid$hexid)
# hexObsLoad <- full_join(hexObsPreOm, hexgrid, by = "hexid")

### Check with a plot 
ggplot() +
  geom_sf(hexObservationsAllSF %>% filter(date == testDate, 
                                          infections>0),
          mapping=aes(fill=infections)) +
  scale_fill_gradient(low = "thistle1", high = "deeppink4") +
  geom_sf(hexObservationsAllSF %>% filter(infections == 0),
          mapping=aes(), fill="green") +
  geom_sf(observationsFips %>% filter(date == testDate, 
                                      infctns == 0), 
          mapping=aes(), color = "red")

###############################################################################
###### Save to a CSV 
###############################################################################
### Ensure the right types 
hexObservationsAll$hexid <- as.character(hexObservationsAll$hexid)
hexObservationsAll$infections <- as.numeric(hexObservationsAll$infections)

### Remove missing values for infectionsPC 
hexObservationsAllNoMissing <- hexObservationsAll %>% 
  filter(is.na(infections) == FALSE)

### Remove the unnecessary columns of geometry and population
hexObservationsAllNoMissing <- hexObservationsAllNoMissing[,] 

write_csv(hexObservationsAllNoMissing, 
          file = "data-products/geo-hexes/hexid-observations_omicronera_meta30m.csv")


hexObservationsAllNoMissingGeom <- full_join(hexObservationsAllNoMissing, 
                                             hexgrid, 
                                             by = "hexid")

hexObservationsAllNoMissingGeom <- st_as_sf(hexObservationsAllNoMissingGeom)

testDate <- as.Date("2022-01-20")

ggplot() +
  geom_sf(hexObservationsAllNoMissingGeom %>% 
            filter(date == testDate) ,
          mapping=aes(fill=log10(infections+1))) +
  scale_fill_viridis_c()
  # geom_sf(hexObservationsAllNoMissingGeom %>% filter(date == testDate,
  #                                                    infectionsPC == 0),
  #         mapping=aes(), fill="green")



###############################################################################
###### Save an SF for plots 
###############################################################################
# geojson_write(
#   hexObservationsAllNoMissingGeom,
#   geometry  = "polygon",
#   file      = paste0("data-products/geo-hexes/hexid-observations_preomicron_meta30m.geojson"),
#   crs = st_crs(hexObservationsAllNoMissingGeom),
#   overwrite = T,
# )

st_write(obj = hexObservationsAllNoMissingGeom,
         paste0("data-products/geo-hexes/hexid-observations_omicronera_meta30m.geojson"),
         delete_dsn = T)
