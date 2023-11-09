suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(lubridate) )
library(glue)
library(cli)
library(docopt)

'Waves project: pull input data from Covidestim API

Usage:
  pullInputData.R -o <path> --rundate <YYYY-MM-DD> --clip-final-months <int>
  pullInputData.R (-h | --help)
  pullInputData.R --version

Options:
  -o <path>                  Where to save CSV of observations [cols]
  --rundate <YYYY-MM-DD>     Rundate for Covidestim data
  --clip-final-months <int>  How many months to exclude from the end of the timeseries, floored to the month
  -h --help                  Show this screen.
  --version                  Show version.

' -> doc

library(docopt)
args <- docopt(doc, version = 'pullInputData.R 0.1')

cli_process_start("Pulling {.val {args$rundate}} input data from API")
d1 <- read_csv(
  url(
    glue("https://api.covidestim.org/inputs?rundate=eq.{args$rundate}&select=fips,date,cases"),
    headers = c("Accept" = "text/csv")
  ),
  col_types = cols(
    fips = col_character(),
    date = col_date(),
    cases = col_number()
  )
)
cli_process_done()

cli_process_start("Pulling {.val {args$rundate}} results from API")
d2 <- local({
  endpoint <- "https://api.covidestim.org/results"
  outcomes <- c("fips", "date", "Rt", "infections", "infectionsPC")

  query <- glue(
    '"run.date"=eq.{rundate}&select={cols}',
    rundate = args$rundate,
    cols = paste(outcomes, collapse = ',')
  )

  read_csv(
    url(
      glue('{endpoint}?{query}'),
      headers = c("Accept" = "text/csv")
    ),
    col_types = cols(
      fips         = col_character(),
      date         = col_date(),
      Rt           = col_number(),
      infections   = col_number(),
      infectionsPC = col_number()
    )
  )
})
cli_process_done()

maxDate <- (max(d1$date) - months(as.numeric(args$clip_final_month))) %>% floor_date(unit = 'month')

cli_alert_info("All dates will be less than {.code {maxDate}}")

d2 <- filter(d2, date < maxDate)

cli_alert_info("Inner-joining model results to input data")

d3 <- inner_join(d1, d2, by=c("fips", "date"))

cli_process_start("Writing to {.file {args$o}}")
write_csv(d3, args$o)
cli_process_done()
