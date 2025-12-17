# Read data
housing <- readr::read_csv(here::here("data-raw", "housing.csv"), show_col_types = FALSE)

# Use data
usethis::use_data(housing, overwrite = TRUE)
