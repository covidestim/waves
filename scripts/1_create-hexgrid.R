# Waves project: US hex-grid generator

suppressPackageStartupMessages( library(sf) )
suppressPackageStartupMessages( library(geojsonio) )
library(magrittr, warn.conflicts = F)
library(ggplot2,  warn.conflicts = F)
library(dplyr,    warn.conflicts = F)
library(stringr,  warn.conflicts = F)
library(readr,    warn.conflicts = F)

# The TopoJSON file is read in, the counties layer is selected, then the data
# is converted to a `sf`. Some of these geometries are invalid because of how
# the S2 geometry backend deals with lat/long data. See discussion here:
#
# https://github.com/r-spatial/sf/issues/1649
#
# Anyway, `st_make_valid` fixes the problems but it's apparently important to
# reassign the result of this call in order to avoid a stale reference to the
# unrepaired data (or at least I think that's what's going on.
print("Loading county polygons then reprojecting")
counties_raw <- st_read("Data/data-sources/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")

# missing geometries cause hexgrid() to fail below; check for NA and print
# the number of counties removed.
counties_na <- counties_raw %>% tidyr::drop_na()  
if (dim(counties_raw)[1] > dim(counties_na)[1]) {
  diff_dim <- dim(counties_raw)[1] - dim(counties_na)[1]
  print(paste(diff_dim, " county(ies) dropped due to missing geometry."))
}

counties <- st_make_valid(counties_na)
# remove counties with empty geometries 
# this is necessary or st_make_grid fails below
counties <- counties[!st_is_empty(counties),]
rm(counties_raw); rm(counties_na)

print("Removing all county polygons which fall outside the lower-48 states")

# Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
excludes = c(
    "02", "60", "03", "81", "07", "64",
    "14", "66", "84", "86", "67", "89",
    "68", "71", "76", "69", "70", "95",
    "43", "72", "74", "78", "79", "15")

counties <- filter(counties, !str_sub(STATEFP, 1, 2) %in% excludes)
counties_5070 <- st_transform(counties, 5070)

### Set the hexsize in kilometers 
hexsize <- 1100
print(paste("Hexsize will be", hexsize, "km^2"))
print("Creating hexgrid")

### transform the counties to a projection compatible with meter hexgrid 
### create a hexgrid using kilometers 
hexgrid = st_make_grid(counties_5070,
                       square = F,
                       units::as_units(hexsize, "km^2"))  %>%
          st_as_sf %>%
          # Eliminate the hexes that don't intersect any counties.
          # I.e. the hexes in the Pacific Ocean etc (b/c Hawaii)
          st_filter(counties_5070, .predicate = st_intersects) %>%
          # Assign serial ID to each hex.
          mutate(hexid = as.character(1:n()))

###############################################################################
### Test that the hexgrid meets our expectations  
###############################################################################
### All hexes are the same size to the fourth decimal
if(length(unique(round(as.numeric(units::set_units(st_area(hexgrid),km^2)), 4))) > 1){
  print("Hexes are not equal sizes")
} else {
  print(paste("All hexes are", unique(round(as.numeric(units::set_units(st_area(hexgrid),km^2)), 4)),"km^2"))
}

range(as.numeric(units::set_units(st_area(hexgrid),km^2)))

### Total area approximates that of the United States
### Per wikipedia, per online ~8 million kilometer squared
paste("Total area covered by the hexgrid is", 
round(as.numeric(units::set_units(st_area(hexgrid %>% st_union()),km^2))/1e6, 3), 
"million km^2")

# ggplot() + geom_sf(data=hexgrid)

# Since we'll need the hexes for graphing later on, save them to a shapefile
print("Writing hexgrid shapefile")
st_write(hexgrid, paste0("Data/data-products/geo-hexes/hexgrid_", hexsize, "_km.shp"), append = F) # Forces overwrite

###############################################################################
### Create the intersection of counties and the hexgrid for population 
### allocation. 
###############################################################################

intersections <- st_intersection(counties_5070, hexgrid) %>% 
                 select(GEOID, hexid, geometry) %>% 
                 rename(fips = GEOID) %>%
                 filter(st_dimension(.) > 0) 

ggplot() + geom_sf(data=intersections)

st_write(intersections, here("Data/data-products/geo-hexes/intersectionPolygons.shp"))
