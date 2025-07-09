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
# remove counties with empty geometries 
# this is necessary or st_make_grid fails below
counties <- counties[!st_is_empty(counties),]
rm(counties_raw); rm(counties_na)
pd()

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

