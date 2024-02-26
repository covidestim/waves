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
args$cbg_polygons <- "data-sources/cbg-polygons/cb_2019_us_bg_500k.shp"
args$cbg_popsize <- "data-sources/cbg_popsize.csv"
args$lower_48 <- T
### This argument is set to control which dataset to use and names to save files
args$modelVersion <- "preomicron"

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

st_crs(counties_na) <- 4326 # This TopoJSON doesn't encode the CRS maybe
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
    "43", "72", "74", "79", "15"
  )
  
  counties <- filter(counties, !str_sub(id, 1, 2) %in% excludes)
}

# ps("Loading CBG polygons from {.file {args$cbg_polygons}}")
cbgs <- read_sf(args$cbg_polygons)
dim(cbgs)

if (identical(args$lower_48, T)) {
  cli_alert_warning("Removing all cbg polygons which fall outside the lower-48 states")
  
  # Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
  excludes = c(
    "02", "60", "03", "81", "07", "64",
    "14", "66", "84", "86", "67", "89",
    "68", "71", "76", "69", "70", "95",
    "43", "72", "74", "78", "79", "15", "11"
  )
  
  cbgs <- filter(cbgs, ! STATEFP %in% excludes)
}

length(unique(cbgs$STATEFP))
dim(cbgs)


# pd()

# ps("Loading CBG popsizes from {.file {args$cbg_popsize}}")
cbg_popsize <- read_csv(
  args$cbg_popsize,
  col_types = cols(GEOID = col_character(), population = col_number())
)
# pd()

hexsize <- as.numeric(args$hexsize)
cli_alert_info("Hexsize will be ~{.val {hexsize}}mi")

# Crude formula to try and make the grid size change-able in a meaningful
# way.
miles_to_degrees <- function(miles) {
  earth_diameter <- 3960
  radians_to_degrees <- 180/pi
  (miles / earth_diameter) * radians_to_degrees
}

# ps("Creating hexgrid")
hexgrid <- st_make_grid(
  counties,
  square = F, # Means "create hexagons"
  cellsize = miles_to_degrees(hexsize) %>% rep(2) # width/height
) %>%
  st_as_sf %>%
  # Eliminate the hexes that don't intersect any counties.
  # I.e. the hexes in the Pacific Ocean etc (b/c Hawaii)
  st_filter(counties, .predicate = st_intersects) %>%
  # Assign serial ID to each hex.
  mutate(hexid = as.character(1:n()))
# pd()

# Since we'll need the hexes for graphing later on, save them to a shapefile
# if (!is.null(args$save_shp)) {
#   # ps("Writing hexgrid shapefile to {.file {args$save_shp}}")
#   st_write(hexgrid, args$save_shp, append = F) # Forces overwrite
#   # pd()
# }
# 
# # Can also save GeoJSON if user specifies. Better for Observable/d3
# if (!is.null(args$save_geojson)) {
#   # ps("Writing hexgrid GeoJSON to {.file {args$save_geojson}}")
#   geojson_write(hexgrid, geometry = "polygon", file = args$save_geojson)
#   # pd()
# }

### st_touches returns a list of neighbors for each hex 
### neighbors are defined as adjacent but not overlapping polygons
# ps("Creating neighbors dataframe")
neighbors<- as.data.frame(st_touches(hexgrid))
colnames(neighbors) <- c("i", "j") # These column names are consistent 
# with county polygons' column names
# pd()

cli_alert_info("{.val {nrow(neighbors)/2}} unique neighbor-pairs identified")

### Write this dataframe to a csv file --save-neighbors <path>
# ps("Writing hex neighbors to {.file {args$save_neighbors}}")
# write_csv(neighbors, args$save_neighbors)
# pd()

#### Join CBG pop and geometry 
cbgpop <- st_as_sf(mutate(cbg_popsize, GEOID = str_sub(GEOID, 8, 19)) %>%
                     right_join(cbgs, by = 'GEOID') %>% 
                     filter(STATEFP != "02", STATEFP != "15") %>% 
                     mutate(fips = paste0(STATEFP, COUNTYFP)) %>% 
                     select(GEOID, fips, population, geometry))

### check for nas 
sum(is.na(cbgpop))

# cbgPop01001 <- st_as_sf(cbgpop %>% filter(fips == "01001"))

# ggplot() + geom_sf(cbgPop01001, mapping=aes(fill=population))

#### Success! 
### check coordinate systems
hexgrid <- st_transform(hexgrid, crs = 26915)
cbgpop <- st_transform(cbgpop, crs = 26915)


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
} else {
  observations <- read_csv(
    "data-products/omicron_estimates.csv") %>% 
    select(fips, date, infections)
  #### Also set a test date for the plots throughout the workflow
  testDate <- "2021-11-04"
}

### join with county geometries from above 
counties$fips <- counties$id
observationFips <- left_join(counties, observations, by = 'fips')

observationFips <- st_as_sf(observationFips)
observationFips <- st_transform(observationFips, crs = 26915)

### Explore with a plot
# ggplot() + geom_sf(observationFips %>% filter(date == testDate), 
#                    mapping=aes(fill=infections)) + 
#   theme_minimal() + 
#   scale_fill_gradient(low = "thistle1", high = "deeppink4")

###############################################################################
################## FIRST: AREA WEIGHTED POPULATION TO HEXES ###################
################## SECOND: AREA WEIGHTED INFECTION TO HEXES ###################
##### THIRD: JOIN THE POP AND INFECTIONS ACROSS HEXES. CALCULATE ##############
########################### INFECTIONS PER CAPITA #############################
###############################################################################

###############################################################################
###############################   Population    ###############################
###############################################################################

### Validate that the population and hexgrid are compatible 
# areal::ar_validate(cbgpop, hexgrid, varList = "population", method = "aw", verbose = TRUE)

### Calculate the area weighted population for hexes
tempPop <- areal::aw_interpolate(
  hexgrid,
  tid = hexid,
  source = cbgpop,
  sid = GEOID,
  weight = "sum",
  output =  "sf",
  extensive = "population"
)

### Generate a tibble version for later join
tempPopTib <- areal::aw_interpolate(
  hexgrid,
  tid = hexid,
  source = cbgpop,
  sid = GEOID,
  weight = "sum",
  output =  "tibble",
  extensive = "population"
)

### Transform back to the projection of interest 
tempPopTrans <- st_transform(tempPop, crs = 4269)
tempPopTrans$hexid <- as.numeric(tempPopTrans$hexid) ### used for filtering
### Plot the population across the hexes 
# ggplot() + geom_sf(tempPopTrans, mapping=aes(fill=population)) + 
#   geom_sf(tempPopTrans %>% filter(population == 0), mapping=aes(), fill="green")

###############################################################################
###############################   Infections    ###############################
###############################################################################
tempPop <- tempPop %>% rename(geometry = x)

tempPop$interpolation_weight <- tempPop[["population"]]
weight_column <- "interpolation_weight"
to_id <- "hexid"
from_id <- "from_id"

InfPopAll <- data.frame(hexid = 0,
                        population = 0,
                         infections = 0, 
                         geometry = tempPop$geometry[1],
                         date = NA)

### Write a loop for the allocation
for (i in 1:length(unique(na.omit(observationFips$date)))){
# for (i in c(120,121)){ ### for testing
  filtDate <- unique(na.omit(observationFips$date))[i]
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
                  dplyr::left_join(denominators,by = from_id) %>%
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
                   dplyr::left_join(interpolated, by = to_id)# %>% 
                   # rename(geometry = x)
  
  ggplot() + geom_sf(output_shapes, mapping = aes(fill = infections))+ 
    theme_minimal() + 
    scale_fill_gradient(low = "thistle1", high = "deeppink4")  + 
    geom_sf(output_shapes %>% filter(infections == 0), mapping = aes(), fill="green")
  
  ##### Date 
  output_shapes$date <- as.Date(filtDate, origin='1970-01-01')

  ##### Add to the big dataframe
  InfPopAll <- rbind(InfPopAll, output_shapes)
}

###############################################################################
##### Create infections per capita 
###############################################################################
### Need to remove the first row of the InfAreaAll dataframe because it was 
### for formatting only. 
hexObservationsAll <- InfPopAll[-1,] %>% 
                      mutate(infectionsPC = ifelse(population == 0, 0, infections/population))

### Create an SF version for plots 
hexObservationsAllSF <- st_as_sf(hexObservationsAll)

# testDate <- unique(na.omit(observationFips$date))[120]

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
                   mapping=aes(fill=infectionsPC)) +
  scale_fill_gradient(low = "thistle1", high = "deeppink4") +
  geom_sf(hexObservationsAllSF %>% filter(infectionsPC == 0),
          mapping=aes(), fill="green") + 
  geom_sf(observationFips %>% filter(date == testDate, infections == 0), 
          mapping=aes(), color = "red")

###############################################################################
###### Save to a CSV 
###############################################################################
### Ensure the right types 
hexObservationsAll$hexid <- as.character(hexObservationsAll$hexid)
hexObservationsAll$infections <- as.numeric(hexObservationsAll$infections)
hexObservationsAll$infectionsPC <- as.numeric(hexObservationsAll$infectionsPC)
### Remove missing values for infectionsPC 
hexObservationsAllNoMissing <- hexObservationsAll %>% filter(is.na(infectionsPC) == FALSE)
### Remove the unnecessary columns of geometry and population
hexObservationsAllNoMissing <- hexObservationsAllNoMissing[,] 

write_csv(hexObservationsAllNoMissing, 
          file = paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, ".csv"))


hexObservationsAllNoMissingGeom <- full_join(hexObservationsAllNoMissing, hexgrid, 
                                             by = "hexid")

hexObservationsAllNoMissingGeom <- st_as_sf(hexObservationsAllNoMissingGeom)

testDate <- unique(na.omit(observationFips$date))[320]

ggplot() +
  geom_sf(hexObservationsAllNoMissingGeom %>% filter(date == testDate) ,
          mapping=aes(fill=infectionsPC)) +
  scale_fill_gradient(low = "thistle1", high = "deeppink4") +
  geom_sf(hexObservationsAllNoMissingGeom %>% filter(date == testDate,
                                                     infectionsPC == 0),
          mapping=aes(), fill="green") + 
  geom_sf(observationFips %>% filter(date == testDate, infections == 0), 
          mapping=aes(), color = "red")



###############################################################################
###### Save an SF for plots 
###############################################################################
geojson_write(
  hexObservationsAllNoMissingGeom,
  geometry  = "polygon",
  file      = paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, ".geojson"),
  overwrite = T,
)

