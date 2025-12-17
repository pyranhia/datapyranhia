library(tidyverse)
library(janitor)

# Read
wine_raw <- readr::read_csv(here::here("data-raw", "wine.csv"), show_col_types = FALSE)
wine_raw

# Clean names
wine <- clean_names(wine_raw)

# Remove id column
wine <- wine |> select(-id)

# Use data
usethis::use_data(wine, overwrite = TRUE)
