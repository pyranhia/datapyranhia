# Read data
titanic <- readr::read_csv(here::here("data-raw", "titanic.csv"), show_col_types = FALSE)

# Use data
usethis::use_data(titanic, overwrite = TRUE)
