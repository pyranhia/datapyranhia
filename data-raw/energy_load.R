library(tidyverse)
library(readxl)

## Read data
energy <- read_excel("data-raw/ENB2012_data.xlsx")


## Colnames
colnames_energy <- c(
  "compacite_relative",       # X1: Relative Compactness
  "surface_sol",              # X2: Surface Area
  "surface_murs",             # X3: Wall Area
  "surface_toit",             # X4: Roof Area
  "hauteur_totale",           # X5: Overall Height
  "orientation",              # X6: Orientation
  "surface_vitrage",          # X7: Glazing Area
  "distribution_vitrage",     # X8: Glazing Area Distribution
  "charge_chauffage",         # y1: Heating Load
  "charge_climatisation"      # y2: Cooling Load
)

colnames(energy) <- colnames_energy

## Use data
usethis::use_data(energy, overwrite = TRUE)
