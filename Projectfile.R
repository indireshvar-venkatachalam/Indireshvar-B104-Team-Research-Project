library(tidyverse)

modern_renewable_energy_consumption <- readr::read_csv(
  "modern-renewable-energy-consumption.csv"
)

str(modern_renewable_energy_consumption)

print(colSums(is.na(modern_renewable_energy_consumption)))

numeric_cols <- modern_renewable_energy_consumption %>%
  select(where(is.numeric)) %>%
  names()
numeric_cols

data_clean <- modern_renewable_energy_consumption %>%
  group_by(Entity) %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ tidyr::replace_na(.x, median(.x, na.rm = TRUE))
    )
  ) %>%
  ungroup()

cat("\nMissing values per column:\n")
print(colSums(is.na(data_clean)))

data_clean <- data_clean %>%
  mutate(
    across(
      all_of(numeric_cols),
      ~ tidyr::replace_na(.x, median(.x, na.rm = TRUE))
    )
  )

cat("\nMissing values per column after data cleaning\n")
print(colSums(is.na(data_clean)))