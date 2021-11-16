suppressPackageStartupMessages( library(tidyverse) )
suppressPackageStartupMessages( library(glue) )

d <- read_csv(
  "alphas.csv",
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

write_csv(d1, "alphas_reformat.csv")
