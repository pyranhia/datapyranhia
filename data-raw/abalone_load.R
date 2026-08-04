library(tidyverse)

# Read
abalone_cols <- c(
  "sex",
  "length",
  "diameter",
  "height",
  "whole_weight",
  "shucked_weight",
  "viscera_weight",
  "shell_weight",
  "rings"
)

abalone <- read_csv(
  here::here("data-raw", "abalone.data"),
  col_names = abalone_cols,
  show_col_types = FALSE
)

# Recode sex as factor
abalone <- abalone |>
  mutate(
    sex = factor(
      sex,
      levels = c("M", "F", "I"),
      labels = c("male", "female", "infant")
    )
  )

# Use data
usethis::use_data(abalone, overwrite = TRUE)
