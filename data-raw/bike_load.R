library(tidyverse)

# Read file
bike_sharing <- read_csv(here::here("data-raw", "bike_sharing.csv"))

# Clean columns
bike_sharing <- bike_sharing |>
  select(
    date = dteday,
    season,
    year = yr,
    month = mnth,
    holiday,
    weekday,
    workingday,
    weather = weathersit,
    temperature = temp,
    humidity = hum,
    windspeed,
    users_casual = casual,
    users_registered = registered,
    rental_number = cnt
  )

# Use data
usethis::use_data(bike_sharing, overwrite = TRUE)
