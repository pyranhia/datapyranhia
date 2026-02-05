library(tidyverse)

## Read data
# Column names
colnames <- read_lines(here::here("data-raw", "adult.names"), skip = 96) |> str_extract(pattern = "^[^:]+")
colnames <- c(colnames, "income")

# Read both tables
df1 <- read_csv(here::here("data-raw", "adult.data"), col_names = colnames, show_col_types = FALSE, na = "?")
df2 <- read_csv(here::here("data-raw", "adult.test"), col_names = colnames, show_col_types = FALSE, na = "?", skip = 1) |>
  mutate(income = str_remove_all(income, "\\."))

# Assemble tables
adult <- bind_rows(df1, df2)

# Clean column names
adult <- janitor::clean_names(adult)

# Drop unwanted columns
adult <- adult |> select(-c(fnlwgt, education_num))

## Use data
usethis::use_data(adult, overwrite = TRUE)
