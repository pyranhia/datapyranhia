library(tidyverse)
library(readxl)

## Read data
# First row contains generic names (X1, ..., Y), actual names are on the second row
credit <- read_excel(
  here::here("data-raw", "default of credit card clients.xls"),
  skip = 1
)

# Clean column names
credit <- credit |>
  rename(default = `default payment next month`) |>
  rename_with(str_to_lower)

## Clean columns
# Drop ID and rename PAY_0 to pay_1 for consistency with bill_amt1 and pay_amt1
credit <- credit |>
  select(-id) |>
  rename(pay_1 = pay_0)

# Recode categorical columns as factors
# Undocumented codes (0, 5, 6 for education, 0 for marriage) are grouped with "other"
credit <- credit |>
  mutate(
    sex = factor(sex, levels = 1:2, labels = c("male", "female")),
    education = case_when(
      education == 1 ~ "graduate_school",
      education == 2 ~ "university",
      education == 3 ~ "high_school",
      .default = "other"
    ) |>
      factor(
        levels = c("graduate_school", "university", "high_school", "other")
      ),
    marriage = case_when(
      marriage == 1 ~ "married",
      marriage == 2 ~ "single",
      .default = "other"
    ) |>
      factor(levels = c("married", "single", "other")),
    default = factor(default, levels = c(0, 1), labels = c("no", "yes"))
  )

## Keep only the most recent month (September 2005)
# months_late: -2, -1 and 0 (no delay) are mapped to 0, giving a numeric,
# monotonic variable that reads directly as "number of months late"
credit <- credit |>
  transmute(
    limit_bal, sex, education, marriage, age,
    months_late = pmax(pay_1, 0),
    bill_amt    = bill_amt1,
    paid_amt    = pay_amt1,
    default
  )

## Use data
usethis::use_data(credit, overwrite = TRUE)
