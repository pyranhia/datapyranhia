library(tidyverse)

## Read data
fires <- read_csv("data-raw/forestfires.csv", show_col_types = FALSE)
summary(fires)

## Colnames
colnames <- c(
  "x",
  "y",
  "mois",
  "jour",
  "ffmc",
  "dmc",
  "dc",
  "isi",
  "temperature",
  "humidite",
  "vent",
  "pluie",
  "surface_brulee"
)

colnames(fires) <- colnames
fires

## Use data
usethis::use_data(fires, overwrite = TRUE)
