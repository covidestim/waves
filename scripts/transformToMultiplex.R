suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(glue) )
suppressPackageStartupMessages( library(cli) )
library(docopt)

'Waves project: transform hex/county-specific alphas into multiplex encoding

Usage:
  transformToMultiplex.R --save-network <path> --save-monthcode-mapping <path> --save-geo-mapping <path> --key <key> --alphas-reformat <path>
  transformToMultiplex.R (-h | --help)
  transformToMultiplex.R --version

Options:
  --save-network <path>            Where to save the .net multiplex file
  --save-monthcode-mapping <path>  Where to save the monthcode mapping
  --save-geo-mapping <path>        Where to save the geo (fips/hexid) mapping
  --key <key>                      Key that uniquely identifies a geographs (fips/hexid)
  --alphas-reformat <path>         Path to reformatted alphas for each geo (fips/hexid) 
  -h --help  Show this screen.
  --version  Show version.

' -> doc

args <- docopt(doc, version = 'transformToMultiplex.R 0.1')

cli_process_start("Reading {.file {args$alphas_reformat}}")
d <- read_csv(
  args$alphas_reformat,
  col_types = cols(
    i = col_character(),
    j = col_character(),
    month = col_factor(),
    value = col_number()
  )
)
cli_process_done()

monthCodes        <- 1:length(levels(d$month))
names(monthCodes) <- sort(levels(d$month))

d <- filter(d, value >= 0)

geos  <- union(unique(d$i), unique(d$j)) %>% sort
nGeos <- length(geos)

geoCodes <- 1:nGeos
names(geoCodes) <- geos
nVertices <- length(geoCodes)

vertices_str <- glue("{geoCodes} \"{names(geoCodes)}\"")

edges <- transmute(
  d,
  layer  = monthCodes[month],
  j      = geoCodes[j],
  i      = geoCodes[i],
  weight = value
)

edges_str <- glue_data(edges, "{layer} {j} {i} {weight}")

cli_alert_success("Finished data transformations")

cli_process_start("Writing geo code mapping: {.file {args$save_geo_mapping}}")
if (args$key == "fips") {
  write_csv(
    tibble(fips=names(geoCodes), code=geoCodes),
    args$save_geo_mapping
  )
} else {
  write_csv(
    tibble(hexid=names(geoCodes), code=geoCodes),
    args$save_geo_mapping
  )
}
cli_process_done()

cli_process_start("Writing month code mapping: {.file {args$save_month_mapping}}")
write_csv(
  tibble(month=names(monthCodes), code=monthCodes),
  args$save_monthcode_mapping
)
cli_process_done()

# .net file

cli_process_start("Writing multiplex network file: {.file {args$save_network}}")
write_file(
  glue("*Vertices {nVertices}

"),
  args$save_network,
  append=T # Suspect...currently relies on makefile to delete any old files
)

write_csv(
  tibble(vertices=vertices_str),
  args$save_network,
  append=T, col_names=F, escape="none", quote="none"
)

write_file(
  "# layer node node weight
*Intra
",
  args$save_network,
  append=T
)

write_csv(
  tibble(vertices=edges_str),
  args$save_network,
  append=T, col_names=F, escape="none", quote="none"
)

cli_process_done()
