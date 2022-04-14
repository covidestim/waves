suppressPackageStartupMessages( library(sf) )
library(geojsonio, warn.conflicts = F)
library(magrittr, warn.conflicts = F)
library(ggplot2, warn.conflicts = F)
library(dplyr, warn.conflicts = F)
library(stringr, warn.conflicts = F)
library(docopt, warn.conflicts = F)
library(cli, warn.conflicts = F)
library(readr, warn.conflicts = F)


'US hex-grid generator

Usage:
  hexbin.R --save-csv <path> --save-shp <path> --county-polygons <path> --cbg-polygons <path> --cbg-popsize <path> --hexsize <num>
  hexbin.R (-h | --help)
  hexbin.R --version

Options:
  --save-csv <path>         Where to save CSV of [hexid,fips,proportion]
  --save-shp <path>         Where to save SHP of hexes, with fields [hexid]
  --county-polygons <path>  Path to TopoJSON of EPSG4326 county boundaries
  --cbg-polygons <path>     Path to TIGER SHP of CBG polygons
  --cbg-popsize <path>      Path to CSV of CBG popsize estimates, [GEOID,population]
  --hexsize <num>           Width of each hex, unit: mi, [default: 20]
  -h --help                 Show this screen.
  --version                 Show version.

' -> doc

ps <- cli_process_start
pd <- cli_process_done

args <- docopt(doc, version = 'hexbin.R 0.1')

ps("Loading county polygons from {.file {args$county_polygons}}")
counties <- topojson_read(args$county_polygons, layer = "counties")
pd()

ps("Loading CBG polygons from {.file {args$cbg_polygons}}")
cbgs <- read_sf(args$cbg_polygons)
pd()

ps("Loading CBG popsizes from {.file {args$cbg_popsize}}")
cbg_popsize <- read_csv(args$cbg_popsize, col_types = cols(GEOID = col_character(), population = col_number()))
pd()

hexsize <- as.numeric(args$hexsize)
cli_alert_info("Hexsize will be {.val {hexsize}}")

miles_to_degrees <- function(miles) {
  earth_radius <- 3960
  radians_to_degrees <- 180/pi
  (miles / earth_radius) * radians_to_degrees
}

ps("Creating hexgrid")
hexgrid <- st_make_grid(
  counties,
  square = F,
  cellsize = miles_to_degrees(hexsize) %>% rep(2)
) %>% st_as_sf %>% st_filter(counties, .predicate = st_intersects)
pd()
                        
ps("Writing hexgrid shapefile to {.file {args$save_shp}}")
st_write(hexgrid, args$save_shp, append = F)
pd()

# connecticut_counties <- counties %>%
#   filter(
#     str_detect(id, "^06")
#     # str_detect(id, "^09") |
#     # str_detect(id, "^36") |
#     # str_detect(id, "^42")
#   )
# 
# connecticut_hexes <- st_filter(hexgrid, connecticut_counties)
# 
# ggplot() +
#   geom_sf(data = connecticut_hexes) +
#   geom_sf(data = connecticut_counties, color = "red", fill = NA)
# 
