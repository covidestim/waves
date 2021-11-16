suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(glue) )
suppressPackageStartupMessages( library(cli) )

cli_process_start("Reading {.file alphas_reformat.csv}")
d <- read_csv(
  "alphas_reformat.csv",
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

fips  <- intersect(unique(d$i), unique(d$j)) %>% sort
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

cli_process_start("Writing {.file fips-code-mapping.csv}")
write_csv(
  tibble(fips=names(fipsCodes), code=fipsCodes),
  "fips-code-mapping.csv"
)
cli_process_done()

# .net file

cli_process_start("Writing {.file network.net}")
write_file(
  glue("*Vertices {nVertices}

"),
  "network.net",
  append=T
)

write_csv(
  tibble(vertices=vertices_str),
  "network.net",
  append=T, col_names=F, escape="none", quote="none"
)

write_file(
  "# layer node node weight
*Intra
",
  "network.net",
  append=T
)

write_csv(
  tibble(vertices=edges_str),
  "network.net",
  append=T, col_names=F, escape="none", quote="none"
)

cli_process_done()
