library(tidyverse)
library(rsample)

## Générer les données
set.seed(2)
n <- 500 # Nombre de points

# Générer toutes les données
data <- tibble(
  heures = rnorm(n, mean = 10, sd = 3),
  experience = rnorm(n, mean = 5, sd = 2)
) |>
  mutate(
    score = 50 +
      2 * heures +
      10 * experience +
      0.5 * heures * experience +
      0.3 * heures^2 -
      0.2 * experience^2 +
      rnorm(n, sd = 5)
  )

## Division des données (à faire avant la première visualisation)
data_split <- initial_split(data, prop = 0.8)
train_tbl  <- training(data_split)
test_tbl   <- testing(data_split)

## On remet tout ensemble
sportifs <- bind_rows(
  train_tbl |> mutate(set = "Train"),
  test_tbl |> mutate(set = "Test")
)

## Use data
usethis::use_data(sportifs, overwrite = TRUE)
