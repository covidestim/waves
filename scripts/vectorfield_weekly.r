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
  "data-products/geo-hexes/mixedmodel/alphas_weekly_reformat_preomicron_mainNEW.csv",
  col_types = cols_only(
    i = col_character(), j = col_character(),
    date_week = col_date(),
    # week = col_number(),
    value = col_number(),
    alpha_normalized = col_number(),
    alpha = col_number()
  )
)

# ps("Reading {.file {args$observations}}")
observations <- read_csv(
  "data-products/geo-hexes/hexid-observations_preomicronNEW.csv",
  col_types = cols(
    hexid = col_character(),
    date = col_date(),
    # cases = col_number(),
    infections = col_number(),
    infectionsPC = col_number(),
    # Rt = col_number()
  )
)

# ps("Summarizing observations by date")
observations_date <- observations %>% 
  # mutate("date" = lubridate::floor_date(date, "week"),
  #        "j" = hexid) %>% #recode hexid as j for join with vector_mean
  rename(j = hexid) |> 
  group_by(date, j) %>%
  summarize("infectionsPC_avg" = mean(infectionsPC)) %>%
  select(j, date, infectionsPC_avg)

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
  select(i, j, j_to_i, i_geo, j_geo)
# pd()

# ps("Joining alphas to neighbors library")
joined <- inner_join(from_j_to_i, alphas, by = c("i", "j")) 
# |> 
#   ## mutating i and j to character
#   mutate(i = as.double(i),
#          j = as.double(j)) |> 
#   ## Writing the hexbin code as a 4-digit number
#   mutate(hex_i = sprintf("%04d", i),
#          hex_j = sprintf("%04d", j))
# pd()                    

# ps("Computing mean vector for every {.code i,date} combination")
vector_mean <- joined %>% 
  # ## filtering to only show alphas >0, and the maximum value between a_ij and a_ji
  filter(alpha>0, alpha == max(alpha),
         .by = c(j,date_week)) %>%
  as_tibble %>%
  mutate(
    j_geo_x  = st_coordinates(j_geo)[,"X"],
    j_geo_y  = st_coordinates(j_geo)[,"Y"],
    ## Try without the sqrt(2)/2, maybe this is fixing the direction
    # j_to_i_x = sqrt(2)/2 * st_coordinates(j_to_i)[,"X"],
    # j_to_i_y = sqrt(2)/2 * st_coordinates(j_to_i)[,"Y"],
    ## No sqrt(2)/2
    j_to_i_x = st_coordinates(j_to_i)[,"X"],
    j_to_i_y = st_coordinates(j_to_i)[,"Y"],
    # j_to_i_x_norm = alpha_normalized * st_coordinates(j_to_i)[,"X"],
    # j_to_i_y_norm = alpha_normalized * st_coordinates(j_to_i)[,"Y"]
  ) |> 
  # mutate(start_x = j_geo_x, start_y = j_geo_y,
  #        end_x = j_geo_x + j_to_i_x, end_y = j_geo_y + j_to_i_y) |> 
  group_by(j, date_week) %>%
  ## summairzes over the maximum value in any direction
  summarize(
    ## Keeping alphas as cut-off measure
    alpha = alpha,
    start_coord_x = j_geo_x + j_to_i_x/3,
    end_coord_x   = j_geo_x + 2*j_to_i_x/3,
    start_coord_y = j_geo_y + j_to_i_y/3,
    end_coord_y   = j_geo_y + 2*j_to_i_y/3,
    .groups = 'drop'
  ) %>%
  filter(if_all(matches('coord'), ~!is.na(.)))
# pd()

# ## Casting points geometry to start_coord and end_coords
# vector_xy_start <- vector_mean |> 
#   select(i, j, date_week, start_x = j_geo_x, start_y = j_geo_y)
#   
# vector_xy_end <- vector_mean |> 
#   select(i, j, date_week, end_x = j_geo_x, j_to_i_x, end_y = j_geo_y, j_to_i_y)|> 
#   mutate(end_x = end_x + j_to_i_x, 
#          end_y = end_y + j_to_i_y) 
# 
# vector_xy <- inner_join(vector_xy_start, vector_xy_end) 

# ps("Joining average infections per capita to mean vectors")
## Verify closer the week flooring , maybe this is cutting off some dates
joined_vector_mean <- vector_mean |> 
  # mutate(date_week = floor_date(date_week, unit = "week", week_start = "Thursday")) |>
  inner_join(observations_date |> 
               rename(date_week = date),
             # |> 
             #   mutate(date_week = floor_date(date_week, unit = "week", week_start = "Thursday")), 
             alphas,
             # |> 
             #   mutate(date_week = floor_date(date_week, unit = "week", week_start = "Thursday")), 
             by = c("j", "date_week"))
# pd()

# ps("Creating {.code LINESTRING} features for every mean-vector")
features_from_wkt <- joined_vector_mean %>%
  mutate(wkt = glue(
    # WKT for a line
    "LINESTRING({start_coord_x} {start_coord_y}, {end_coord_x} {end_coord_y})"
  )) %>%
  # Create simple feature from WKT
  mutate(geography = st_as_sfc(wkt)) %>%
  select(-wkt)  |> 
  st_as_sf(crs = 4326) |> 
  st_cast("LINESTRING")
# pd()

## Maybe we have to cast this object as LINESTRING from POINT geometry, instead of writing it manually
# all.equal(unique(alphas$date_week), unique(features_from_wkt$date_week))

# ps("Writing {.file {args$o}}")
sf::st_write(features_from_wkt,
         "data-products/geo-hexes/vectors/vectors_weekly_omicronera_mainNEW.geojson",
         delete_dsn = T)

# geojson_write(input = features_from_wkt,
#               file = "data-products/geo-hexes/vectors/vectors_weekly_regardless_rt_omicron.geojson",
#               geometry = "LINESTRING", 
#               crs = 4326)
# pd()
