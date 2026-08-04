library(tidyverse)

# Read
cmc_cols <- c(
  "wife_age",
  "wife_edu",
  "husband_edu",
  "num_children",
  "wife_religion",
  "wife_working",
  "husband_occupation",
  "standard_of_living_index",
  "media_exposure",
  "contraceptive_method"
)

cmc <- read_csv(
  here::here("data-raw", "cmc.data"),
  col_names = cmc_cols,
  col_types = "iiiiiiiiii"
)

# Recode categorical/binary columns as factors
cmc <- cmc |>
  mutate(
    wife_edu = factor(wife_edu, levels = 1:4, labels = c("low", "medium_low", "medium_high", "high"), ordered = TRUE),
    husband_edu = factor(husband_edu, levels = 1:4, labels = c("low", "medium_low", "medium_high", "high"), ordered = TRUE),
    wife_religion = factor(wife_religion, levels = c(0, 1), labels = c("non_islam", "islam")),
    wife_working = factor(wife_working, levels = c(0, 1), labels = c("yes", "no")),
    husband_occupation = factor(husband_occupation, levels = 1:4, labels = c("cat1", "cat2", "cat3", "cat4")),
    standard_of_living_index = factor(standard_of_living_index, levels = 1:4, labels = c("low", "medium_low", "medium_high", "high"), ordered = TRUE),
    media_exposure = factor(media_exposure, levels = c(0, 1), labels = c("good", "not_good")),
    contraceptive_method = factor(
      contraceptive_method,
      levels = 1:3,
      labels = c("no_use", "long_term", "short_term")
    )
  )

# Use data
usethis::use_data(cmc, overwrite = TRUE)
