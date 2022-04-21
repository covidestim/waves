suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(glue) )
library(docopt)

'Waves project: transform Julia mixed-model output to machine-readable format

Usage:
  transformResults.R -o <path> --alphas <path> 
  transformResults.R (-h | --help)
  transformResults.R --version

Options:
  -o <path>        Where to write the CSV of alphas to [i, j, month, value]
  --alphas <path>  Path to a CSV from Julia, [interactionTerm, (Intercept)]
  -h --help        Show this screen.
  --version        Show version.

' -> doc

args <- docopt(doc, version = 'transformResults.R 0.1')

d <- read_csv(
  args$alphas,
  col_types = cols(
    interactionTerm = col_character(),
    `(Intercept)` = col_number()
  )
)

d1 <- rename(d, alpha=interactionTerm, value=`(Intercept)`) %>%
  separate(alpha, into = c("i", "j", "year", "month"), sep = "-") %>%
  transmute(
    i, j,
    month = glue("{year}-{month}-01") %>% as.Date,
    value
  )

write_csv(d1, args$o)
