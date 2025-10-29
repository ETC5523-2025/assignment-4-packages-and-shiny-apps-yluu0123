# data-raw/build_data.R
# Prepare the OWID temperature-anomaly dataset for the package.

library(readr)
library(dplyr)
library(janitor)

# 1. read csv from package's data-raw/
raw <- read_csv("data-raw/temperature-anomaly.csv", show_col_types = FALSE)

# 2. clean/select/rename to a minimal, tidy schema
data <- raw |>
  clean_names() |>
  rename(country = entity, year = year) |>
  mutate(temp_anomaly = `global_average_temperature_anomaly_relative_to_1861_1890`) |>
  select(country, year, temp_anomaly)

# keep Australia & World if available
if (any(data$country %in% c("Australia", "World"))) {
  data <- dplyr::filter(data, country %in% c("Australia", "World"))
}

# 3. save as package data object
temp_data <- data
usethis::use_data(temp_data, overwrite = TRUE)

