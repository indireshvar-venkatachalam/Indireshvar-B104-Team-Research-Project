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

