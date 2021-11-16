suppressPackageStartupMessages( library(tidyverse) )
library(cli)
suppressPackageStartupMessages( library(lubridate) )

cli_process_start("Pulling latest results from API")
d <- read_csv(
  url(
    "https://api.covidestim.org/latest_results?select=fips,date,Rt,infectionsPC",
    headers = c("Accept" = "text/csv")
  ),
  col_types = cols(
    fips = col_character(),
    date = col_date(),
    Rt = col_number(),
    infectionsPC = col_number()
  )
)
cli_process_done()

maxDate <- (max(d$date) - months(1)) %>% floor_date(unit = 'month')

cli_alert_info("All dates will be less than {.code {maxDate}}")

d <- filter(d, date < maxDate)

cli_process_start("Writing to {.file results.csv}")
write_csv(d, "results.csv")
cli_process_done()
