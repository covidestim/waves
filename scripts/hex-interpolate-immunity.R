### Requires three steps:
### 1. CBG pop to county pop
### 2. CBG pop to hex pop
### 3. Calculate county immunity counts --> hex immunity counts

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

###############################################################################
##### Load in the immunity estimates & filter
###############################################################################
immunity <- vroom::vroom("data-sources/fips-immunity.csv")

#####Explore immunity estimates
# colnames(immunity)
# range(immunity$date)
# range(immunity$both.only)
# range(immunity$immune)

##### Filter to the both immunity
immunityTotal <- immunity %>% 
  select(fips, date, immune, immune.infected, immune.vaccinated) %>%
  ### create a record of original immunity
  mutate(immunityOriginal = immune) |> 
         ### update immunity for workflow below
  mutate(across(starts_with("immune"), ~ ifelse(.x > 1, 1, .x)))

###############################################################################
##### Load in counties geometries
################################################################################
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
counties$fips <- counties$id

###############################################################################
##### Load in cbg population
###############################################################################
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

cbgpop <- st_as_sf(mutate(cbg_popsize, GEOID = str_sub(GEOID, 8, 19)) %>%
                     right_join(cbgs, by = 'GEOID') %>%
                     filter(STATEFP != "02", STATEFP != "15") %>%
                     mutate(fips = paste0(STATEFP, COUNTYFP)) %>%
                     select(GEOID, fips, population, geometry))

### check for nas
sum(is.na(cbgpop))

###############################################################################
##### Create the hex grid
###############################################################################
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
  st_as_sf() %>%
  # Eliminate the hexes that don't intersect any counties.
  # I.e. the hexes in the Pacific Ocean etc (b/c Hawaii)
  st_filter(counties, .predicate = st_intersects) %>%
  # Assign serial ID to each hex.
  mutate(hexid = as.character(1:n()))

###############################################################################
##### STEP 1: CBG to county populations
###############################################################################

### Transform both datasets to same projection
cbgpop <- st_transform(cbgpop, crs = 26915)
counties <- st_transform(counties, crs = 26915)

### Calculate the area weighted population for counties
tempPopCounty <- areal::aw_interpolate(
  counties,
  tid = fips,
  source = cbgpop,
  sid = GEOID,
  weight = "sum",
  output =  "sf",
  extensive = "population"
)

### Generate a tibble version for later join
tempPopCountyTib <- areal::aw_interpolate(
  counties,
  tid = fips,
  source = cbgpop,
  sid = GEOID,
  weight = "sum",
  output =  "tibble",
  extensive = "population"
)

###############################################################################
##### STEP 2: CBG to hex populations
###############################################################################
### Transform both datasets to same projection
hexgrid <- st_transform(hexgrid, crs = 26915) ### ADDED LINE ###

### Calculate the area weighted population for hexes
tempPopHex <- areal::aw_interpolate(
  hexgrid,
  tid = hexid,
  source = cbgpop,
  sid = GEOID,
  weight = "sum",
  output =  "sf",
  extensive = "population"
)

### Generate a tibble version for later join
tempPopHexTib <- areal::aw_interpolate(
  hexgrid,
  tid = hexid,
  source = cbgpop,
  sid = GEOID,
  weight = "sum",
  output =  "tibble",
  extensive = "population"
)

### Transform back to the projection of interest
tempPopHexTrans <- st_transform(tempPopHex, crs = 4269)

tempPopHexTrans$hexid <- as.numeric(tempPopHexTrans$hexid)

###############################################################################
##### Step 3: Distribute immunity counts across hexes
###############################################################################

###############################################################################
##### Create counts of immunity
###############################################################################
### Join the immunity to the population estimates for counties
immunityTotal$fips <- as.character(immunityTotal$fips)

###join immunity to county polygons
immunityTotalGeo <- full_join(tempPopCounty, immunityTotal)
immunityTotalGeo <- st_transform(immunityTotalGeo, crs = 4269)

### Calculate the immunity counts from immunity fractions * population
immunityTotalGeo <- immunityTotalGeo %>% 
  mutate(across(starts_with("immune"), ~ .x * population, .names = "{.col}.count"))
  # mutate(immune.count = immune * population)
### Explore the range of these estimates and distribution before going to hexes
# range(is.na(immunityTotalGeo$immune.count)==FALSE)
#### This will be population weighted
tempPopHexTrans <- tempPopHexTrans %>% rename(geometry = x)
immunityTotalGeo$from_id <- immunityTotalGeo$id ### UPDATED LINE
tempPopHexTrans$interpolation_weight <- tempPopHexTrans[["population"]]
weight_column <- "interpolation_weight"
to_id <- "hexid"
from_id <- "from_id"

## Saving the files to pre-allocation to speed up the process
## Population Hex
sf::st_write(obj = tempPopHexTrans,
             dsn = "data-products/geo-hexes/hex_pop.geojson",
             delete_dsn = T,
             delete_layer = T)

sf::st_write(obj = immunityTotalGeo,
             dsn = "data-products/geo-hexes/hex_immune.geojson",
             delete_dsn = T,
             delete_layer = T)

## Reading objects, in case you won't want to run everything above
tempPopHexTrans <- sf::st_read("data-products/geo-hexes/hex_pop.geojson")

immunityTotalGeo <- sf::st_read("data-products/geo-hexes/hex_immune.geojson")

### Create an empty dataframe to hold the results
ImmunityPopAll <- data.frame(hexid = 0,
                             population = 0,
                             immune.count = 0,
                             immune.infected.count = 0,
                             immune.vaccinated.count = 0,
                             geometry = tempPopHexTrans$geometry[1],
                             date = NA)

testDate <- "2020-07-26"

### Write a loop for the allocation
filtDate <- unique(immunityTotalGeo$date)

### Write a loop for the allocation
for (i in filtDate){
  # for (i in c(120,121)){ ### for testing
  # filtDate <- testDate
  
  print(i)

  immunityFilt <- immunityTotalGeo %>%
    filter(date == i) %>%
    select(ends_with(".count"), geometry)

  immunityFilt$from_id <- as.character(1:nrow(immunityFilt))
  weight_sym <- rlang::sym(weight_column)
  from_id_sym <- rlang::sym(from_id)
  to_id_sym <- rlang::sym(to_id)
  
  immunityFilt <- st_transform(immunityFilt, crs = 26915)
  tempPopHexTrans <- st_transform(tempPopHexTrans |> 
                                    filter(!is.na(interpolation_weight)), 
                                  crs = 26915)
  
  ## Creating denominators, it is the counties geometries with weight for each of them.
  denominators <- immunityFilt %>%
    ## When we join the geometries, we make it slower to produce the intersection
    sf::st_join(tempPopHexTrans, left = FALSE) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(!!from_id_sym) %>%
    dplyr::summarize(weight_total = sum(!!weight_sym,
                                        na.rm = TRUE))
  
  ## Creating intersections, it is the left_join of immune counts on counties geometries with the denominators, same geometries. Then, we intersect the hex with populations counts. After we group by the mix of geometries, counties and hexes, 
  intersections <- immunityFilt |>
    dplyr::left_join(denominators, by = from_id) |> 
    sf::st_intersection(tempPopHexTrans)%>%
    # dplyr::mutate(intersection_id = dplyr::row_number()) %>%
    sf::st_drop_geometry() %>%
    dplyr::group_by(hexid) %>%
    dplyr::mutate(intersection_value = sum(!!weight_sym, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::distinct(hexid,.keep_all = TRUE) %>%
    dplyr::mutate(weight_coef = intersection_value/weight_total) %>%
    dplyr::select(!!from_id_sym, !!to_id_sym, intersection_value,
                  weight_coef)

  interpolated <- immunityFilt %>%
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

  output_shapes <- tempPopHexTrans %>%
    dplyr::select(!!to_id_sym, population) %>%
    ## Certify hexid are character, to be able to join with interpolated values
    dplyr::mutate(hexid = as.character(hexid)) |> 
    dplyr::left_join(interpolated, by = to_id)# %>%
  # rename(geometry = x)

  # ggplot() + geom_sf(output_shapes,
  #                    mapping = aes(fill = immune.count))+
  #   theme_minimal() +
  #   scale_fill_gradient(low = "thistle1", high = "deeppink4", na.value = "green")  +
  #   geom_sf(output_shapes %>% filter(immune.count == 0),
  #           mapping = aes(), fill="green")

  ##### Date
  output_shapes$date <- as.Date(i, origin='1970-01-01')

  ##### Add to the big dataframe
  ImmunityPopAll <- rbind(ImmunityPopAll, output_shapes)
}

  ### Remove the placeholder value
  ImmunityPopAll <- st_as_sf(ImmunityPopAll[-1,])
  ###############################################################################
  ###### Save to a CSV
  ###############################################################################
  ### Ensure the right types
  ImmunityPopAll$hexid <- as.character(ImmunityPopAll$hexid)
  ImmunityPopAll$immune.count <- as.numeric(ImmunityPopAll$immune.count)
  ImmunityPopAll$date <- as.Date(ImmunityPopAll$date)
  ImmunityPopAll <- ImmunityPopAll |> 
    ## This was added to use when doing the bivariate local Moran's I
    mutate(immunePC = (immune.count/population)*1e5)
  
  ggplot() + geom_sf(ImmunityPopAll |> 
                       filter(date == delta_peak), 
                     mapping = aes(fill = immune.vaccinated.count))+
    theme_minimal() +
    scale_fill_gradient(low = "thistle1", high = "deeppink4", na.value = "green")
  
  ### Remove missing values for infectionsPC
  ImmunityPopAllNoMissing <- ImmunityPopAll %>% filter(is.na(immune.count) == FALSE)
  
  ### Remove the unnecessary columns of geometry and population
  ImmunityPopAllNoMissing <- ImmunityPopAllNoMissing |> 
    st_drop_geometry()
  
  ## Saving with no missing no geo
  vroom::vroom_write(ImmunityPopAllNoMissing,
            file = "data-products/geo-hexes/hexid-immunity.csv")


  ImmunityPopAllNoMissingGeom <- full_join(ImmunityPopAllNoMissing,
                                               hexgrid,
                                               by = "hexid") |> 
    rename(geometry = x)

  ImmunityPopAllNoMissingGeom <- st_as_sf(ImmunityPopAllNoMissingGeom)

  testDate <- unique(na.omit(immunityTotalGeo$date))[12]
  
  ggplot() + geom_sf(ImmunityPopAll |> 
                       filter(date == testDate), 
                     mapping = aes(fill = immune.count))+
    theme_minimal() +
    scale_fill_gradient(low = "thistle1", high = "deeppink4", na.value = "green")


  ###############################################################################
  ###### Save an SF for plots
  ###############################################################################
  geojson_write(
    ImmunityPopAll,
    geometry  = "polygon",
    file      = "data-products/geo-hexes/hexid-immunity.geojson",
    crs = st_crs(ImmunityPopAll),
    overwrite = T,
  )

  sf::st_write(obj = ImmunityPopAllNoMissingGeom,
           dsn = "data-products/geo-hexes/hexid-immunity.geojson",
           delete_dsn = T)

