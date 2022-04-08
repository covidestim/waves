suppressPackageStartupMessages( library(tidyverse) )
library(cli)
suppressPackageStartupMessages( library(lubridate) )

cli_process_start("Pulling 2021-12-02 input data from API")
d1 <- read_csv(
  url(
    "https://api.covidestim.org/inputs?rundate=eq.2021-12-02&select=fips,date,cases",
    headers = c("Accept" = "text/csv")
  ),
  col_types = cols(
    fips = col_character(),
    date = col_date(),
    cases = col_number(),
  )
)
cli_process_done()

cli_process_start("Pulling 2021-12-02 results from API")
d2 <- read_csv(
  url(
    'https://api.covidestim.org/results?"run.date"=eq.2021-12-02&select=fips,date,Rt,infections',
    headers = c("Accept" = "text/csv")
  ),
  col_types = cols(
    fips = col_character(),
    date = col_date(),
    Rt = col_number(),
    infections = col_number()
  )
)
cli_process_done()


maxDate <- (max(d1$date) - months(1)) %>% floor_date(unit = 'month')

cli_alert_info("All dates will be less than {.code {maxDate}}")

d2 <- filter(d2, date < maxDate)

cli_alert_info("Inner-joining model results to input data")

d3 <- inner_join(d1, d2, by=c("fips", "date"))

cli_process_start("Writing to {.file results.csv}")
write_csv(d3, "results.csv")
cli_process_done()
