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
ggplot() + geom_sf(observationFips %>% filter(date == testDate), 
                   mapping=aes(fill=infections)) + 
  theme_minimal() + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4")

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
ggplot() + geom_sf(tempPopTrans, mapping=aes(fill=population)) + 
  geom_sf(tempPopTrans %>% filter(population == 0), mapping=aes(), fill="green")

###############################################################################
###############################   Infections    ###############################
###############################################################################
###############################################################################
#### First show it works for one date 
###############################################################################
### Validate that the infections and hexgrid are compatible 
hexgrid <- hexgrid %>% rename("geometry" = x)
hexgrid <- st_transform(hexgrid, crs = 26915)

areal::ar_validate(observationFips, hexgrid, 
                   varList = "infections",
                   method = "aw", verbose = TRUE)

### Calculate the area weighted infection for hexes
tempInfArea <- areal::aw_interpolate(
  hexgrid,
  tid = hexid,
  source = observationFips %>% select(id, name, fips, date, infections, geometry) %>% filter(date == testDate),
  sid = fips,
  weight = "sum",
  output =  "sf",
  extensive = "infections"
)

### Generate a tibble version for later join
tempInfAreaTib <- areal::aw_interpolate(
  hexgrid,
  tid = hexid,
  source = observationFips %>% select(id, name, fips, date, infections, geometry) %>% filter(date == testDate),
  sid = fips,
  weight = "sum",
  output =  "tibble",
  extensive = "infections"
)

### Transform back to the projection of interest for plotting
tempInfAreaTrans <- st_transform(tempInfArea, crs = 4269)

### Plot the population across the hexes 
ggplot() + geom_sf(tempInfAreaTrans, mapping=aes(fill=infections)) + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4") + 
  geom_sf(tempInfAreaTrans %>% filter(infections == 0), mapping=aes(), fill="green")


### Create infections per capita
hexObservations <- full_join(tempInfAreaTib, tempPopTib, by="hexid") %>% 
  mutate(infectionsPC = infections/population)


###############################################################################
##### Now create dataframe to hold hex infections for all dates 
###############################################################################

InfAreaAll <- data.frame(hexid = 0,
                         infections = 0, 
                         date = NA)

### Write a loop for the allocation
for (i in 1:length(unique(observationFips$date))){
  filtDate <- unique(observationFips$date)[i]
  print(filtDate)
  tempInfAreaAll <- areal::aw_interpolate(
    hexgrid,
    tid = hexid,
    source = observationFips %>% 
      select(id, name, fips, date, infections, geometry) %>% 
      filter(date == filtDate),
    sid = fips,
    weight = "sum",
    output =  "tibble",
    extensive = "infections"
  )
  ##### Reformat the date
  tempInfAreaAll$date <- as.Date(filtDate, origin='1970-01-01')
  ##### Add to the big dataframe
  InfAreaAll <- rbind(InfAreaAll, tempInfAreaAll)
}
#### Reformat the date
tempInfAreaAll$date <- as.Date(tempInfAreaAll$date, origin='1970-01-01')

###############################################################################
##### Create infections per capita 
###############################################################################
### Need to remove the first row of the InfAreaAll dataframe because it was 
### for formatting only. 
hexObservationsAll <- full_join(InfAreaAll[-1,], tempPop, by="hexid") %>% 
  mutate(infectionsPC = ifelse(population == 0, 0, infections/population), 
         date = as.Date(date, origin='1970-01-01')) %>% 
  rename(geometry = x)

### Create an SF version for plots 
hexObservationsAllSF <- st_as_sf(hexObservationsAll)

### Check with a plot 
ggplot() + geom_sf(hexObservationsAllSF %>% filter(date == testDate), 
                   mapping=aes(fill=infections)) + 
  scale_fill_gradient(low = "thistle1", high = "deeppink4") + 
  geom_sf(hexObservationsAllSF %>% filter(infections == 0), 
          mapping=aes(), fill="green")
###############################################################################
###### Save to a CSV 
###############################################################################
### Ensure the right types 
hexObsPreOm$hexid <- as.character(hexObsPreOm$hexid)
hexObsPreOm$infections <- as.numeric(hexObsPreOm$infections)
hexObsPreOm$infectionsPC <- as.numeric(hexObsPreOm$infectionsPC)
### Remove missing values for infectionsPC 
hexObsPreOmNoMissing <- hexObsPreOm %>% filter(is.na(infectionsPC) == FALSE)
### Remove the unnecessary columns of geometry and population
hexObsPreOmNoMissing <- hexObsPreOmNoMissing[,-c(4:5)] 

write_csv(hexObsPreOmNoMissing, 
          file = paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, ".csv"))
###############################################################################
###### Save an SF for plots 
###############################################################################
geojson_write(
  hexObservationsAllSF,
  geometry  = "polygon",
  file      = paste0("data-products/geo-hexes/hexid-observations_", args$modelVersion, ".geojson"),
  overwrite = T,
)
