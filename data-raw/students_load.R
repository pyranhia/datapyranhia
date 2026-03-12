library(tidyverse)

## Read data
students <- read_delim(
  "data-raw/student-mat.csv",
  delim = ";",
  escape_double = FALSE,
  trim_ws = TRUE,
  show_col_types = FALSE
)


# Sélectionner les variables d'intérêt et renommer en français
students <- students %>%
  select(
    school, sex, age, Medu, Fedu, schoolsup, famsup, paid,
    studytime, failures, absences, G1, G2, G3
  ) %>%
  rename(
    ecole = school,
    sexe = sex,
    education_mere = Medu,
    education_pere = Fedu,
    soutien_ecole = schoolsup,
    soutien_famille = famsup,
    cours_payants = paid,
    temps_etude = studytime,
    echecs_precedents = failures,
    note_G1 = G1,
    note_G2 = G2,
    note_finale = G3
  ) %>%
  mutate(
    # Convertir les binaires "yes"/"no" en logical
    soutien_ecole = soutien_ecole == "yes",
    soutien_famille = soutien_famille == "yes",
    cours_payants = cours_payants == "yes"
  )


## Use data
usethis::use_data(students)
