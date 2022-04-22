### Create csv of neighbors of each hex 
suppressPackageStartupMessages( library(sf) )
library(cli, warn.conflicts = F)
library(readr, warn.conflicts = F)
library(docopt, warn.conflicts = F)


'Waves project: US hex-grid generator

Usage:
  hexbin.R --hex-polygons <path> 
  hexbin.R (-h | --help)
  hexbin.R --version

Options:
  --hex-polygons <path>     Path to shapefile of hex boundaries
  -h --help                 Show this screen.
  --version                 Show version.

' -> doc

ps <- cli_process_start
pd <- cli_process_done

args <- docopt(doc, version = 'hexbin.R 0.1')

### Read in the hexes shapefile 
ps("Loading hex polygons from {.file {args$hex_polygons}}")
hexes <- read_sf(args$hex_polygons)
pd()

### st_touches returns a list of neighbors for each hex 
### neighbors are defined as adjacent but not overlapping polygons
ps("Creating neighbors dataframe}")
neighbors<- as.data.frame(st_touches(hexes))
colnames(neighbors) <- c("i", "j") # These column names are consistent 
                                   # with V1 with county polygons
pd()

### Write this dataframe to a csv file neighbors_hex.csv
ps("Writing hex neighbors to {.file neighbors_hex.csv}")
write_csv(neighbors, "neighbors_hex.csv")
pd()