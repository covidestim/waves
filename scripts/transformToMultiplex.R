suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(glue) )
suppressPackageStartupMessages( library(cli) )
library(docopt)

'Waves project: transform hex/county-specific alphas into multiplex encoding

Usage:
  transformToMultiplex.R --save-network <path> --save-monthcode-mapping <path> --save-fips-mapping <path> --alphas-reformat <path>
  transformToMultiplex.R (-h | --help)
  transformToMultiplex.R --version

Options:
  --save-network <path>            Where to save the .net multiplex file
  --save-monthcode-mapping <path>  Where to save the monthcode mapping
  --save-fips-mapping <path>       Where to save the FIPS mapping
  --alphas-reformat <path>         Where the reformatted alphas are for each FIPS 
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

fips  <- union(unique(d$i), unique(d$j)) %>% sort
nFips <- length(fips)

fipsCodes <- 1:nFips
names(fipsCodes) <- fips
nVertices <- length(fipsCodes)

vertices_str <- glue("{fipsCodes} \"{names(fipsCodes)}\"")

edges <- transmute(
  d,
  layer  = monthCodes[month],
  j      = fipsCodes[j],
  i      = fipsCodes[i],
  weight = value
)

edges_str <- glue_data(edges, "{layer} {j} {i} {weight}")

cli_alert_success("Finished data transformations")

cli_process_start("Writing FIPS code mapping: {.file {args$save_fips_mapping}}")
write_csv(
  tibble(fips=names(fipsCodes), code=fipsCodes),
  args$save_fips_mapping
)
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
