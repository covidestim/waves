'Wave project: interpolate hex observations from county-level Covidestim data

Usage:
  hex-interpolate.R --save-observations <path> [--save-excluded <path>] --hex-mapping <path> --observations <path> [--exclude-threshold <pct>] [--debug-missingness]
  hex-interpolate.R (-h | --help)
  hex-interpolate.R --version

Options:
  --save-observations <path>  Where to save a CSV of observations for each hex [cols, ...]
  --save-excluded <path>      Where to save a CSV detailing hexes which were excluded
  --hex-mapping <path>        Path to the CSV containing the FIPS/hex mapping [cols, ...]
  --observations <path>       Path to CSV containing Covidestim output
  --exclude-threshold <pct>   Exclude hexes where > <pct>% of population has no data [default: 10]
  --debug-missingness         Print a list of hexes which are missing data pre-exclude
  -h --help  Show this screen.
  --version  Show version.

' -> doc

library(docopt)
library(cli)
suppressPackageStartupMessages( library(tidyverse) )

args <- docopt(doc, version = 'hex-interpolate.R 0.1')
args$exclude_threshold <- as.numeric(args$exclude_threshold)

ps <- cli_process_start
pd <- cli_process_done

ps("Reading {.file {args$hex_mapping}}")
mapping <- read_csv(
  args$hex_mapping,
  col_types = cols(
    hexid                = col_character(),
    fips                 = col_character(),
    proportion_from_fips = col_number(),
    proportion_of_fips   = col_number(),
  )
)
pd()

ps("Reading {.file {args$observations}}")
observations <- read_csv(
  args$observations,
  col_types = cols(
    fips = col_character(),
    date = col_date(),
    cases = col_number(),
    Rt = col_number(),
    infections = col_number()
  )
)
pd()

n_hexes_at_start <- length(unique(mapping$hexid))

counties_present_in_covidestim <- unique(observations$fips)
counties_present_in_mapping    <- unique(mapping$fips)

counties_present <- union(
  counties_present_in_covidestim,
  counties_present_in_mapping
)

local({
  counties_not_in_covidestim <- setdiff(
    counties_present_in_mapping,
    counties_present_in_covidestim
  )

  counties_not_in_mapping <- setdiff(
    counties_present_in_covidestim,
    counties_present_in_mapping
  )

  frac_unusable_counties <-
    (length(counties_not_in_covidestim) + length(counties_not_in_mapping)) /
      length(counties_present)

  frac_unusable_counties_pct <-
    scales::label_percent(accuracy = 0.1)(frac_unusable_counties)

  cli_alert_warning("{.emph {frac_unusable_counties_pct}} counties unusable:")
  cli_ul()
  cli_li("{.emph {length(counties_not_in_covidestim)}} counties are in hex-mapping but not in Covidestim data")
  cli_li("{.emph {length(counties_not_in_mapping)}} counties are in Covidestim data but not in hex-mapping")
  cli_end()
})

ps("Calculating per-hex data missingness weighted by population")
missingness_by_hex <- left_join(
  mapping,
  tibble(fips = counties_present_in_covidestim, present = T),
  by = 'fips'
) %>%
  replace_na(list(present = F)) %>% # Now we know which FIPS are missing data
  group_by(hexid) %>%
  summarize(
    # Are any of this hex's FIPS missing data?
    any_missing_fips = any(present == F), 

    # How many are missing?
    n_missing = sum(present == F),

    # What proportion of the hex's population has no Covidestim observations?
    proportion_of_hex_missing = sum(
      proportion_from_fips[which(present == F)]
    )
  )
pd()

# If --debug-missingness, print which hexes have missing data
if (args$debug_missingness) {
  cli_alert_info("Missingness by hex:")
  print(missingness_by_hex %>% filter(any_missing_fips), n = 1000)
}

ps("Filtering out hexes with greater than {.emph {args$exclude_threshold}%} population missing in Covidestim data")
approved_hexes <- filter(
  missingness_by_hex,
  proportion_of_hex_missing < I(args$exclude_threshold)/100
) %>% pull(hexid)
n_hexes_discarded <- nrow(missingness_by_hex) - length(approved_hexes)
pd()
cli_alert_warning("Discarded {.val {n_hexes_discarded}} / {.val {n_hexes_at_start}} hexes")

result_uninterpolated <- mapping %>% filter(hexid %in% approved_hexes) %>%
  inner_join(observations, by = 'fips')

ultimately_included_fips   <- unique(result_uninterpolated$fips)
ultimately_included_hexids <- unique(result_uninterpolated$hexid)

ps("Interpolating hex observations from Covidestim observations")
result <- result_uninterpolated %>%
  group_by(hexid, date) %>%
  summarize(
    cases      = sum(proportion_of_fips   * cases),
    infections = sum(proportion_of_fips   * infections),
    Rt         = sum(proportion_from_fips * Rt),
    .groups = 'drop'
  )
pd()

cli_alert_info("Interpolated data: missingness report (all should be FALSE):")
print(
  result %>%
    summarize(
      across(
        all_of(c("cases", "infections", "Rt")),
        ~any(is.na(.))
      )
    )
)

ultimately_excluded_fips <-
  setdiff(counties_present, ultimately_included_fips)
ultimately_excluded_hexids <-
  setdiff(unique(mapping$fips), ultimately_included_hexids)

if (!is.null(args$save_excluded)) {
  ps("Saving excluded fips+hexids report to {.file {args$save_excluded}}")
  write_csv(
    tibble(
      geography = c(ultimately_excluded_hexids, ultimately_excluded_fips),
      type = c(
        rep("hexid", length(ultimately_excluded_hexids)),
        rep("fips",  length(ultimately_excluded_fips))
      )
    ),
    args$save_excluded
  )
  pd()
}

ps("Saving interpolated observations to {.file {args$save_observations}}" )
write_csv(result, args$save_observations)
pd()
