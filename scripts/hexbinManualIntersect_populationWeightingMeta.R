suppressPackageStartupMessages( library(sf) )
suppressPackageStartupMessages( library(geojsonio) )
library(magrittr, warn.conflicts = F)
library(ggplot2,  warn.conflicts = F)
library(dplyr,    warn.conflicts = F)
library(stringr,  warn.conflicts = F)
library(docopt,   warn.conflicts = F)
library(cli,      warn.conflicts = F)
library(readr,    warn.conflicts = F)

args <- list()
args$county_polygons <- "data-sources/county_polygons.topojson"
args$hexsize <- 25
args$lower_48 <- T
### This argument is set to control which dataset to use and names to save files
args$modelVersion <- "preomicron"

###############################################################################
########################## SETUP THE COUNTY POLYGONS ##########################
###############################################################################

# The TopoJSON file is read in, the counties layer is selected, then the data
# is converted to a `sf`. Some of these geometries are invalid because of how
# the S2 geometry backend deals with lat/long data. See discussion here:
#
# https://github.com/r-spatial/sf/issues/1649
#
# Anyway, `st_make_valid` fixes the problems but it's apparently important to
# reassign the result of this call in order to avoid a stale reference to the
# unrepaired data (or at least I think that's what's going on.
# ps("Loading county polygons from {.file {args$county_polygons}}, then reprojecting")
counties_raw <- topojson_read(args$county_polygons, layer = "counties") %>%
                st_make_valid()

# missing geometries cause hexgrid() to fail below; check for NA and print
# the number of counties removed.
counties_na <- counties_raw %>% tidyr::drop_na()  
if (dim(counties_raw)[1] > dim(counties_na)[1]) {
  diff_dim <- dim(counties_raw)[1] - dim(counties_na)[1]
  cli_progress_step(paste(diff_dim, " county(ies) dropped due to missing geometry."))
}

# Need to assert the geometry or we excede memory limit
st_crs(counties_na) <- 4326
counties <- st_make_valid(counties_na)
# remove counties with empty geometries 
# this is necessary or st_make_grid fails below
counties <- counties[!st_is_empty(counties),]
rm(counties_raw); rm(counties_na)
# pd()

if (identical(args$lower_48, T)) {
  cli_alert_warning("Removing all county polygons which fall outside the lower-48 states")
  
  # Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
  excludes = c(
    "02", "60", "03", "81", "07", "64",
    "14", "66", "84", "86", "67", "89",
    "68", "71", "76", "69", "70", "95",
    "43", "72", "74", "79", "15", "78"
  )
  
  counties <- filter(counties, !str_sub(id, 1, 2) %in% excludes)
}

###############################################################################
################## READ IN THE HEX DISTRIBUTED POPULATION #####################
###############################################################################
### Population data source is meta's 30 meter estimates. 
### This file is created in script: [INSERT SCRIPT NAME HERE]

# metapop <- read_sf(dsn = "data-products/geo-hexes/meta_pop_new/",
#                    layer = "hex_pop_meta") %>%
#            st_filter(counties, .predicate = st_intersects) %>%
#            rename(population = mtpp_30) %>% select(!popultn)


metapop <- read_sf(dsn = "data-products/geo-hexes/meta_pop_new/", 
                                layer = "hex_pop_meta_new", 
                   crs = 26915) %>%
                   rename(population = meta_pop)



###############################################################################
############# READ IN AND FORMAT COVIDESTIM INFECTION ESTIMATES ###############
###############################################################################
### Create observations at a FIPS level.
if(args$modelVersion == "preomicron"){
  observations <- read_csv(
    "data-products/covidestim-observations.csv",
    col_types = cols(
      fips         = col_character(),
      date         = col_date(),
      cases        = col_number(),
      Rt           = col_number(),
      infections   = col_number(),
      infectionsPC = col_number()
    )
  )
  #### Also set a test date for the plots throughout the workflow
  testDate <- "2020-07-26"
} else if(args$modelVersion == "omicronera"){
  observations <- vroom::vroom(
    "data-products/omicronera_observation.csv")
  
  #### Also set a test date for the plots throughout the workflow
  testDate <- "2022-01-20"
  
} else if(args$modelVersion == "immunity"){
  observations <- vroom::vroom("data-sources/fips-immunity.csv")
  
  #### Also set a test date for the plots throughout the workflow
  testDate <- "2020-07-26"
} else{ 
  observations <- vroom::vroom("data-products/observations_fullmodel.csv") |> 
    dplyr::select(date, state, infections_p50) |> 
    dplyr::rename(infectionsPC = infections_p50)
  
  #### Also set a test date for the plots throughout the workflow
  testDate <- "2022-01-22"
}

### join with county geometries from above 
if(args$modelVersion != "fullmodel"){
  counties$fips <- counties$id
  observationFips <- full_join(counties, observations, by = 'fips')
  
  observationFips <- st_as_sf(observationFips)
  observationFips <- st_transform(observationFips, crs = 26915)
  
  # ## Saving the joined object
  # sf::st_write(obj = observationFips, 
  #              dsn = paste0("data-products/fips-shape-", args$modelVersion, ".geojson"),
  #              delete_dsn = T,
  #              delete_layer = T)
  
}else{
  states <- tigris::states(cb = T) |> 
    filter(! STATEFP %in% excludes)
  
  states$state <- states$NAME
  observationFips <- full_join(states, observations, by = 'state')
  
  observationFips <- st_as_sf(observationFips)
  observationFips <- st_transform(observationFips, crs = 26915)
}

### Explore with a plot
ggplot() + geom_sf(observationFips %>% filter(date == testDate),
                   mapping=aes(fill=infections)) +
  theme_minimal() +
  scale_fill_gradient(low = "thistle1", high = "deeppink4") +
  ggtitle(paste(testDate))


## Saving the observations with geoms
# sf::st_write(dsn = paste0("data-products/geo-hexes/observations_", 
#                           args$modelVersion,
#                           ".geojson"),
#              obj = observationFips,
#              delete_dsn = T)

###############################################################################
##################  FIRST: POP WEIGHTED INFECTION TO HEXES  ###################
##### SECOND: JOIN THE POP AND INFECTIONS ACROSS HEXES. CALCULATE #############
########################### INFECTIONS PER CAPITA #############################
###############################################################################

###############################################################################
###############################   Population    ###############################
###############################################################################

### Set the population to the meta population
tempPop <- metapop 

tempPop$hexid <- as.character(tempPop$hexid) ### used for filtering

### How many zeros? 
print(paste("number of zero population hexes =", sum(tempPop$population == 0)))


### Generate a tibble version for later join
tempPopTib <- tibble(metapop)

### Validate that the hex population and observations are compatible 
areal::ar_validate(tempPop, observationFips, 
                   varList = "population", 
                   method = "aw", verbose = TRUE)

### Transform back to the projection of interest 
# observationFips <- st_transform(observationFips, st_crs(tempPop))

###############################################################################
###############################   Infections    ###############################
###############################################################################

tempPop$interpolation_weight <- tempPop[["population"]]
weight_column <- "interpolation_weight"
to_id <- "hexid"
from_id <- "from_id"

InfPopAll <- data.frame(hexid = "0",
                        population = 0,
                        infections = 0, 
                        geometry = tempPop$geometry[1],
                        date = as.Date(testDate))

### Write a loop for the allocation
for (i in 1:length(unique(na.omit(observationFips$date)))){
  # for (i in 187:197){ ### for testing

  filtDate <- sort(unique(na.omit(observationFips$date)))[i]
  print(filtDate)
  observationsFilt <- observationFips %>% 
    filter(date == filtDate) %>% 
    select(infections, geometry)
  
  observationsFilt$from_id <- as.character(1:nrow(observationsFilt))
  
  weight_sym <- rlang::sym(weight_column)
  from_id_sym <- rlang::sym(from_id)
  to_id_sym <- rlang::sym(to_id)
  
  denominators <- observationsFilt %>%
    sf::st_join(tempPop, left = FALSE) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!from_id_sym) %>%
    dplyr::summarize(weight_total = sum(!!weight_sym,
                                        na.rm = TRUE))

  intersections <- observationsFilt %>%
    dplyr::left_join(denominators, by = from_id) %>%
    sf::st_intersection(tempPop) %>%
    # dplyr::filter(sf::st_is(., c("POLYGON", "MULTIPOLYGON", "GEOMETRYCOLLECTION"))) %>%
    dplyr::mutate(intersection_id = dplyr::row_number()) %>%
    # sf::st_join(tempPop, left = TRUE) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(intersection_id) %>%
    dplyr::mutate(intersection_value = sum(!!weight_sym, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(intersection_id,.keep_all = TRUE) %>%
    dplyr::mutate(weight_coef = intersection_value/weight_total) %>%
    dplyr::select(!!from_id_sym, !!to_id_sym, intersection_value,
                  weight_coef)
  
  interpolated <- observationsFilt %>% 
    sf::st_drop_geometry() %>%
    dplyr::left_join(intersections,  by = from_id) %>% 
    dplyr::mutate(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric), .fns = ~(.x * weight_coef)))  %>%
    dplyr::select(-weight_coef) %>%
    dplyr::group_by(!!to_id_sym) %>%
    dplyr::summarize(dplyr::across(tidyselect::vars_select_helpers$where(is.numeric),
                                   .fns = ~sum(.x, na.rm = TRUE))) %>%
    dplyr::select(-intersection_value)
  
  output_shapes <- tempPop %>% 
    dplyr::select(!!to_id_sym, population) %>%
    dplyr::left_join(interpolated, by = to_id)
  
  # ggplot() + geom_sf(output_shapes, mapping = aes(fill = infections))+
  #   theme_minimal() +
  #   scale_fill_gradient(low = "thistle1", high = "deeppink4")  +
  #   geom_sf(output_shapes %>% filter(infections == 0), mapping = aes(), fill="green") + 
  #   ggtitle(paste(unique(output_shapes$date)))
  
  ##### Date 
  output_shapes$date <- as.Date(filtDate)
  
  ##### Add to the big dataframe
  InfPopAll <- rbind(InfPopAll, output_shapes)
  rm(output_shapes); rm(interpolated); rm(intersections); rm(denominators); rm(filtDate)
}

###############################################################################
###################### CREATE INFECTIONS PER CAPITA ###########################
###############################################################################
### Need to remove the first row of the InfAreaAll dataframe because it was 
### for formatting only. 
hexObservationsAll <- InfPopAll[-1,] %>% mutate(infectionsPC = (infections/population)*1e5)

## check that infections are always <= population 
if(sum(hexObservationsAll$infections > hexObservationsAll$population, na.rm = TRUE) > 0){
  print("Infections exceeding population counts. Check weighting above.")
}
### Confirmed! No infections > population. 

### Create an SF version for plots 
hexObservationsAllSF <- st_as_sf(hexObservationsAll)
# st_crs(hexObservationsAllSF)

# testDate <- unique(na.omit(observationFips$date))[12]
testDate <- as.Date("2020-07-26")

### Check if we have the same number of infections 
if(sum(observationFips %>% 
       st_drop_geometry() %>% 
       filter(date == testDate) %>% 
       select(infections)) - 
   sum(hexObservationsAll %>% 
       filter(date == testDate, is.na(infections)==FALSE) %>% 
       select(infections)) > 1) {
  print("signicant loss of infections in interpolation step")
}

# hexgrid$hexid <- as.numeric(hexgrid$hexid)
# hexObsLoad <- full_join(hexObsPreOm, hexgrid, by = "hexid")

### Check with a plot 
ggplot() +
  geom_sf(hexObservationsAllSF %>% filter(date == testDate),
          mapping=aes(fill=infections)) +
  scale_fill_gradient(low = "thistle1", high = "deeppink4") +
  geom_sf(hexObservationsAllSF %>% filter(date == testDate, infections == 0),
          mapping=aes(), fill="green") +
  # geom_sf(observationFips %>% filter(date == testDate, infections == 0), 
  #         mapping=aes(), color = "red") +
  ggtitle(paste(testDate))

###############################################################################
###### Save to a CSV 
###############################################################################
### Ensure the right types 
hexObservationsAll$hexid <- as.character(hexObservationsAll$hexid)
hexObservationsAll$infections <- as.numeric(hexObservationsAll$infections)
hexObservationsAll$infections <- as.numeric(hexObservationsAll$infections)
### Remove missing values for infectionsPC 
hexObservationsAllNoMissing <- hexObservationsAll %>% filter(is.na(infections) == FALSE)
### Remove the unnecessary columns of geometry and population
hexObservationsAllNoMissing <- hexObservationsAllNoMissing[,] 

write_csv(hexObservationsAllNoMissing, 
          file = paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, "05-31-25.csv"))

hexObservationsAllNoMissingGeom <- st_as_sf(hexObservationsAllNoMissing)

ggplot() +
  geom_sf(hexObservationsAllNoMissingGeom %>% 
            filter(date == testDate) ,
          mapping=aes(fill=infections)) +
  scale_fill_viridis_b(labels = scales::label_percent(scale = 10, accuracy = 2))+
  # geom_sf(hexObservationsAllNoMissingGeom %>% filter(date == testDate,
  #                                                    infectionsPC == 0),
  #         mapping=aes(), fill="green") + 
  geom_sf(observationFips %>% filter(date == testDate, infections == 0), 
          mapping=aes(), color = "red")

###############################################################################
###### Save an SF for plots 
###############################################################################
# geojson_write(
#   hexObservationsAllNoMissingGeom,
#   geometry  = "polygon",
#   file      = paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, "05-31-25.geojson"),
#   crs = st_crs(hexObservationsAllNoMissingGeom),
#   overwrite = T,
# )

st_write(obj = hexObservationsAllNoMissingGeom,
         paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, "06-05-25.geojson"),
         delete_dsn = T, delete_layer = TRUE)

