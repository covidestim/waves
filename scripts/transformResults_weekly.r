suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(glue) )
library(docopt)

# 'Waves project: transform Julia mixed-model output to machine-readable format
# 
# Usage:
#   transformResults_weekly.R -o <path> --alphas <path>
#   transformResults.R (-h | --help)
#   transformResults.R --version
# 
# Options:
#   -o <path>        Where to write the CSV of alphas to [i, j, month, value]
#   --alphas <path>  Path to a CSV from Julia, [interactionTerm, (Intercept)]
#   -h --help        Show this screen.
#   --version        Show version.
# 
# ' -> doc
# 
# args <- docopt(doc, version = 'transformResults_weekly.R 0.1')

d <- read_csv(
  "data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt.csv",
  col_types = cols(
    interactionTerm = col_character(),
    `(Intercept)` = col_number()
  )
)

d1 <- rename(d, alpha=interactionTerm, value=`(Intercept)`) |> 
  separate(alpha, into = c("i", "j", "year", "month", "week"), sep = "-") |> 
  transmute(
    i, j,
    date = glue("{year}-{month}-01") |> as.Date(),
    week = glue("{week}") |> as.integer(),
    alpha = value,
    value # backwards-compatibility, for now
  ) |> 
  # Creating a date using the month and week
  mutate(date_week = ymd(date) + (week - week(date))*7)

d1_with_normalized <- d1 |>  
  ## Should be grouped by i, not by j
  group_by(j, date_week) |>
  mutate(alpha_normalized = alpha / mean(abs(alpha)))

write_csv(d1_with_normalized, 
          "data-products/geo-hexes/mixedmodel/alphas_weekly_regardless_rt-reformat.csv")
  