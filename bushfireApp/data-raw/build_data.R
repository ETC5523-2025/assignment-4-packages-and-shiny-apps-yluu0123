## data-raw/build_data.R
##
## Build the cleaned dataset `temp_data` for the bushfireApp package.


library(readr)
library(dplyr)
library(stringr)

# 1. Read the raw temperature anomaly CSV
raw <- read_csv(
  "data-raw/temperature-anomaly.csv",
  show_col_types = FALSE
)


clean_data <- raw %>%
  transmute(
    country       = .[[1]],
    code          = .[[2]],
    year          = .[[3]],
    temp_anomaly  = .[[4]]
  )

# 2. Clean types and order
temp_data <- clean_data %>%
  mutate(
    country = str_trim(as.character(country)),
    year = as.integer(year),
    temp_anomaly = as.numeric(temp_anomaly)
  ) %>%
  arrange(country, year)


# 3. Save as package data

usethis::use_data(temp_data, overwrite = TRUE)
