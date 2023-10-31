suppressPackageStartupMessages( library(sf) )
suppressPackageStartupMessages( library(tidyverse) )
library(docopt)
library(cli)
library(glue)

# 'Alpha vector-field generator
# 
# Usage:
#   vectorfield.R -o <path> --neighbors <path> --alphas <path> --geos <path> --observations <path>
#   vectorfield.R (-h | --help)
#   vectorfield.R --version
# 
# Options:
#   -o <path>              Where to write a GeoJSON with the vector for each [hex, date]
#   --neighbors <path>     Path to a csv [i,j] of neighbors        
#   --alphas <path>        Path to a csv [i,j,date,alpha_normalized,...]
#   --geos <path>          Path to a GeoJSON containing the polygons of all hexes
#   --observations <path>  Path to a csv of observations by hex
#   -h --help              Show this screen.
#   --version              Show version.
# 
# ' -> doc
# 
# ps <- cli_process_start 
# pd <- cli_process_done 
# 
# args <- docopt(doc, version = 'vectorfield.R 0.2')

# Uncomment this line to capture the value of the `args` object...useful for
# interactive work in Rstudio
#
# saveRDS(args, 'example-vectorfield-args.RDS')

# Use this to read in a surrogate argument object for interactive work.
#
# args <- readRDS('example-vectorfield-args.RDS')

# ps("Reading {.file {args$neighbors}}")
neighbors <- read_csv(
  "data-products/geo-hexes/hexid-neighbors.csv",
  col_types = cols(i = col_character(), j = col_character())
)

# ps("Reading {.file {args$alphas}}")
alphas <- read_csv(
  "data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat.csv",
  col_types = cols_only(
    i = col_character(), j = col_character(),
    date = col_date(),
    # week_num = col_character(),
    alpha_normalized = col_number()
  )
) |> 
  rename(week = date)

# ps("Reading {.file {args$observations}}")
observations <- read_csv(
  "data-products/geo-hexes/hexid-observations.csv",
  col_types = cols(
    hexid = col_character(),
    date = col_date(),
    cases = col_number(),
    infections = col_number(),
    infectionsPC = col_number(),
    Rt = col_number()
  )
)

# ps("Summarizing observations by date")
observations_date <- observations %>% 
                        mutate("week" = lubridate::floor_date(date, "week"),
                               "j" = hexid) %>% #recode hexid as j for join with vector_mean
                        group_by(week, j) %>%
                        summarize("infectionsPC_avg" = mean(infectionsPC)) %>%
                        select(j, week, infectionsPC_avg)

# pd()

# ps("Reading {.file {args$geos}} and calculating centroids")
geos <- read_sf("data-products/geo-hexes/hexes.geojson") %>%
  mutate(geometry = st_centroid(geometry))
# pd()

# ps("Creating library of [i,j] polygons")
# This is a tibble with columns [i, i_geo, j, j_geo].
# `i_geo`, `j_geo` columns are of type `st_point`
neighbors_with_associated_geos <- 
  rename(geos, j = hexid, j_geo = geometry) %>%
  inner_join(neighbors, by = 'j') %>%
  inner_join(
    # `as_tibble` drops the `sf` class; otherwise you get an error about trying
    # to join `sf`'s together without using `st_join`
    rename(geos, i = hexid, i_geo = geometry) %>% as_tibble,
    by = 'i'
  )
# pd()

# ps("Forming intra-geo vectors")
from_j_to_i <- 
  mutate(neighbors_with_associated_geos, j_to_i = i_geo - j_geo) %>%
  select(i, j, j_to_i, j_geo)
# pd()

# ps("Joining alphas to neighbors library")
joined <- inner_join(from_j_to_i, alphas, by = c("i", "j"))
# pd()                    

# ps("Computing mean vector for every {.code i,date} combination")
vector_mean <- joined %>% as_tibble %>%
  mutate(
    j_geo_x  = st_coordinates(j_geo)[,"X"],
    j_geo_y  = st_coordinates(j_geo)[,"Y"],
    j_to_i_x = alpha_normalized * st_coordinates(j_to_i)[,"X"],
    j_to_i_y = alpha_normalized * st_coordinates(j_to_i)[,"Y"]
  ) %>% group_by(j, week) %>%
  summarize(
    start_coord_x = first(j_geo_x),
    end_coord_x   = first(j_geo_x) + mean(j_to_i_x),
    start_coord_y = first(j_geo_y),
    end_coord_y   = first(j_geo_y) + mean(j_to_i_y),
    .groups = 'drop'
  ) %>%
  filter(if_all(matches('coord'), ~!is.na(.)))
# pd()

# ps("Joining average infections per capita to mean vectors")
## Verify closer the week flooring , maybe this is cutting off some dates
joined_vector_mean <- vector_mean %>% 
  mutate(week = floor_date(week, "week")) |> 
  inner_join(observations_date |> 
               mutate(week = floor_date(week, "week")), 
             alphas, 
             by = c("j", "week"))
# pd()

# ps("Creating {.code LINESTRING} features for every mean-vector")
features_from_wkt <- joined_vector_mean %>%
  mutate(wkt = glue(
    # WKT for a line
    "LINESTRING({start_coord_x} {start_coord_y}, {end_coord_x} {end_coord_y})"
  )) %>%
  # Create simple feature from WKT
  mutate(geography = st_as_sfc(wkt)) %>%
  select(-wkt) %>%
  st_as_sf(crs = 4326) |> 
  rename(date = week)
# pd()

# ps("Writing {.file {args$o}}")
write_sf(features_from_wkt, 
         "data-products/geo-hexes/vectors/vectors_weekly_regardless_rt.geojson", 
         append = F)
# pd()
