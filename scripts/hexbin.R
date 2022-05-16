suppressPackageStartupMessages( library(sf) )
suppressPackageStartupMessages( library(geojsonio) )
library(magrittr, warn.conflicts = F)
library(ggplot2,  warn.conflicts = F)
library(dplyr,    warn.conflicts = F)
library(stringr,  warn.conflicts = F)
library(docopt,   warn.conflicts = F)
library(cli,      warn.conflicts = F)
library(readr,    warn.conflicts = F)

'Waves project: US hex-grid generator

Usage:
  hexbin.R --save-mapping <path> [--save-shp <path>] [--save-geojson <path>] --save-neighbors <path> --county-polygons <path> --cbg-polygons <path> --cbg-popsize <path> [--hexsize <num>] [--lower-48]
  hexbin.R (-h | --help)
  hexbin.R --version

Options:
  --save-mapping <path>     Where to save CSV of [hexid,fips,proportion1,proportion2]
  --save-neighbors <path>   Where to save CSV of [i,j] neighbors
  --save-shp <path>         Where to save SHP of hexes, with fields [hexid]
  --save-geojson <path>     Where to save GeoJSON of hexes, with fields [hexid]
  --county-polygons <path>  Path to TopoJSON of EPSG4326 county boundaries
  --cbg-polygons <path>     Path to TIGER SHP of CBG polygons
  --cbg-popsize <path>      Path to CSV of CBG popsize estimates, [GEOID,population]
  --hexsize <num>           Width of each hex, unit: mi, [default: 20]
  --lower-48                Restrict binning to lower 48 states
  -h --help                 Show this screen.
  --version                 Show version.

' -> doc

ps <- cli_process_start
pd <- cli_process_done

args <- docopt(doc, version = 'hexbin.R 0.1')

# The TopoJSON file is read in, the counties layer is selected, then the data
# is converted to a `sf`. Some of these geometries are invalid because of how
# the S2 geometry backend deals with lat/long data. See discussion here:
#
# https://github.com/r-spatial/sf/issues/1649
#
# Anyway, `st_make_valid` fixes the problems but it's apparently important to
# reassign the result of this call in order to avoid a stale reference to the
# unrepaired data (or at least I think that's what's going on.
ps("Loading county polygons from {.file {args$county_polygons}}, then reprojecting")
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
rm(counties_raw); rm(counties_na)
pd()

if (identical(args$lower_48, T)) {
  cli_alert_warning("Removing all county polygons which fall outside the lower-48 states")

  # Source: https://en.wikipedia.org/wiki/Federal_Information_Processing_Standard_state_code
  excludes = c(
    "02", "60", "03", "81", "07", "64",
    "14", "66", "84", "86", "67", "89",
    "68", "71", "76", "69", "70", "95",
    "43", "72", "74", "79"
  )

  counties <- filter(counties, !str_sub(id, 1, 2) %in% excludes)
}

ps("Loading CBG polygons from {.file {args$cbg_polygons}}")
cbgs <- read_sf(args$cbg_polygons)
pd()

ps("Loading CBG popsizes from {.file {args$cbg_popsize}}")
cbg_popsize <- read_csv(
  args$cbg_popsize,
  col_types = cols(GEOID = col_character(), population = col_number())
)
pd()

hexsize <- as.numeric(args$hexsize)
cli_alert_info("Hexsize will be ~{.val {hexsize}}mi")

# Crude formula to try and make the grid size change-able in a meaningful
# way.
miles_to_degrees <- function(miles) {
  earth_diameter <- 3960
  radians_to_degrees <- 180/pi
  (miles / earth_diameter) * radians_to_degrees
}

ps("Creating hexgrid")
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
pd()

# Since we'll need the hexes for graphing later on, save them to a shapefile
if (!is.null(args$save_shp)) {
  ps("Writing hexgrid shapefile to {.file {args$save_shp}}")
  st_write(hexgrid, args$save_shp, append = F) # Forces overwrite
  pd()
}

# Can also save GeoJSON if user specifies. Better for Observable/d3
if (!is.null(args$save_geojson)) {
  ps("Writing hexgrid GeoJSON to {.file {args$save_geojson}}")
  geojson_write(hexgrid, geometry = "polygon", file = args$save_geojson)
  pd()
}

### st_touches returns a list of neighbors for each hex 
### neighbors are defined as adjacent but not overlapping polygons
ps("Creating neighbors dataframe")
neighbors<- as.data.frame(st_touches(hexgrid))
colnames(neighbors) <- c("i", "j") # These column names are consistent 
                                   # with county polygons' column names
pd()

cli_alert_info("{.val {nrow(neighbors)/2}} unique neighbor-pairs identified")

### Write this dataframe to a csv file --save-neighbors <path>
ps("Writing hex neighbors to {.file {args$save_neighbors}}")
write_csv(neighbors, args$save_neighbors)
pd()

# Join the CBG population data to the CBG polygons. This results in a loss of
# ~3000 CBGs (out of 217k). Some CBGs have 0 population.
#
# TODO: Why are we losing these polygons and how much does that matter? Are
#   some of these lost polygons concentrated in any particular part of the
#   country?
#
# The GEOIDs used in the two datasets have slightly different formats, so
# one of them is chopped to match the other.
#
# Then, the centroids of each CBG are calculated. This is done so that each
# CBG is within at most one hex - since otherwise it would be possible for the
# polygonal shape of a CBG to intersect two hexes, which would make things
# needlessly complicated.
ps("Joining population data to CBG polygons and calculating CBG centroids")
cbgpop_centroids <- mutate(cbg_popsize, GEOID = str_sub(GEOID, 8, 19)) %>%
  inner_join(cbgs, by = 'GEOID') %>%
  transmute(
    GEOID,
    fips = paste0(STATEFP, COUNTYFP),
    population,
    geometry
  ) %>%
  st_as_sf() %>%
  # Best to reproject to the counties' CRS before doing any geographic ops
  st_transform(4326) %>%
  mutate(geometry = st_centroid(geometry))
pd()

# This is where we generate the first key mapping: which CBGs are in which
# counties, and which counties are those CBGs in?
#
# Note that we conveniently already have the FIPS code for each CBG.
#
# Notes:
#
# - Using an inner join eliminates the possibility of having an unjoined
#   CBG for any reason.
#
# - `st_nearest_feature` is used over `st_within` because it avoids issues
#   that arise from the different spatial resolutions of the centroids and the
#   hexgrids, where it's possible for a centroid to not be within any 
#   hex.
ps("Joining CBG centroids to hexgrid polygons using {.code st_nearest_feature} operator")
cbgs_with_hexid <- 
  st_join(cbgpop_centroids, hexgrid, join = st_nearest_feature, left = F) %>%
  select(GEOID, fips, population, hexid) %>%
  as_tibble
pd()

# Calculate the population of each county by summming up the CBGs in each
# county. This is advantageous because we want everything to sum to 1, which
# isn't likely to happen if we rely on an external source of FIPS population
# data.
ps("Calculating county-population using CBG data")
fipspop_according_to_cbgs <- as_tibble(cbgpop_centroids) %>%
  select(-GEOID, -geometry) %>%
  group_by(fips) %>%
  summarize(fipspop = sum(population, na.rm = T)) 
pd()
cli_alert_info("U.S. population is {.val {sum(fipspop_according_to_cbgs$fipspop)}} according to CBG data")

# 1. Associate the CBGs to the population of their enclosing county
# 2. Compute each hex's population using CBG pop data
# 3. Calculate the:
#   - `proportion_from_fips`: Proportion of the hex's population that comes
#       from each intersecting FIPS.
#       => We need this to interpolate rate-based observations
#
#   - `proportion_of_fips`: Proportion of the intersecting FIPS's population
#       which lies within the hex.
#       => We need this to interpolate incidence observations, like raw cases
ps("Creating hexid-FIPS mapping + proportions")
mapping <- inner_join(cbgs_with_hexid, fipspop_according_to_cbgs, by = 'fips') %>%
  group_by(hexid) %>%
  mutate(
    hexpop = sum(population, na.rm = T),
    n_fips_involved = length(unique(fips))
  ) %>%
  ungroup %>%
  group_by(hexid, fips) %>%
  summarize(
    # If we don't know hex's population, then assume that an equal number of
    # people from each intersecting FIPS make up the population of the hex.
    proportion_from_fips = ifelse(
      first(hexpop) == 0,
      1/first(n_fips_involved),
      sum(population, na.rm = T)/first(hexpop)
    ),
    # If we don't know the fips population, then we can't calculate what
    # percentage of this FIPS's population comes from this hex, so assign it
    # 0.
    proportion_of_fips = ifelse(
      first(fipspop) == 0,
      0,
      sum(population, na.rm = T)/first(fipspop)
    ),
    .groups = 'drop'
  )
pd()
cli_alert_info("{.val {nrow(mapping)}} intersections added to mapping")
cli_alert_info("{.val {length(unique(mapping$hexid))}} hexes in the mapping")

ps("Creating hexid-FIPS mapping + proportions to {.file {args$save_mapping}}")
write_csv(mapping, args$save_mapping)
pd()
