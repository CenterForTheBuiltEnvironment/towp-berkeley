# Title: Get temperature percentiles
# Description: Fetch ASHRAE thermal comfort database and calculate temperature
#              percentiles for US office buildings

library(tidyverse)

meta_data <- read_csv("https://raw.githubusercontent.com/CenterForTheBuiltEnvironment/ashrae-db-II/refs/heads/master/v2.1.0/db_metadata.csv")

measurement_data <- read_csv("https://raw.githubusercontent.com/CenterForTheBuiltEnvironment/ashrae-db-II/refs/heads/master/v2.1.0/db_measurements_v2.1.0.csv.gz")

ashrae_database <- dplyr::left_join(measurement_data, meta_data)

ashrae_database_usa <- ashrae_database %>%
  filter(country == "usa" & building_type == "office" & between(thermal_sensation, -1.5, 1.5))

percentiles <- ashrae_database_usa %>%
  summarise(
    P10 = quantile(ta, probs = 0.10, na.rm = TRUE),
    P25 = quantile(ta, probs = 0.25, na.rm = TRUE),
    P50 = quantile(ta, probs = 0.50, na.rm = TRUE),
    P75 = quantile(ta, probs = 0.75, na.rm = TRUE),
    P90 = quantile(ta, probs = 0.90, na.rm = TRUE)
  )

quantiles <- list(
  Q10 = as.numeric(percentiles$P10),
  Q25 = as.numeric(percentiles$P25),
  Q50 = as.numeric(percentiles$P50),
  Q75 = as.numeric(percentiles$P75),
  Q90 = as.numeric(percentiles$P90)
)