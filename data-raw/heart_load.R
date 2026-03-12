library(tidyverse)

## Read data
heart <- read_table(
  "data-raw/cleve.mod",
  col_names = FALSE,
  col_types = cols(X6 = col_character()),
  skip = 19
) |>
  select(-X15)

## Rename columns
colnames <- c(
  "age",
  "sexe",
  "type_douleur_poitrine",
  "pression_arterielle_repos",
  "cholesterol",
  "glycemie_jeun_sup_120",
  "ecg_repos",
  "frequence_cardiaque_max",
  "angine_induite_exercice",
  "depression_st",
  "pente_st",
  "nb_vaisseaux_colores",
  "thalassemie",
  "maladie_cardiaque"
)
colnames(heart) <- colnames

## Fix columns types
# Booleans to booleans
# maladie_cardique as factor
# nb_vaisseaux_colores as numeric

heart <- heart |>
  mutate(
    across(
    c(glycemie_jeun_sup_120, angine_induite_exercice),
    ~ ifelse(.x == "true", "TRUE", "FALSE") |> as.logical()
   ),
   maladie_cardiaque = ifelse(maladie_cardiaque == "buff", 0, 1) |> as.factor(),
   nb_vaisseaux_colores = as.numeric(nb_vaisseaux_colores)
  )

## Use data
usethis::use_data(heart, overwrite = TRUE)
