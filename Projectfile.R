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

hydro_data <-
  data_clean %>%
  select(
    Country = Entity,
    Year,
    `Hydro Power Generation` = `Hydropower (terawatt-hours)`
  ) %>%
  filter(Country %in% c("Brazil", "India"))

print(hydro_data %>% head())

hydro_wide <-
  hydro_data %>%
  spread(key = Country, value = `Hydro Power Generation`) %>%
  arrange(Year)

colnames(hydro_wide)[colnames(hydro_wide) == "Brazil"] <- "Brazil_Hydro" 
colnames(hydro_wide)[colnames(hydro_wide) == "India"] <- "India_Hydro"

print(hydro_wide %>% head())

cat("\nBasic Summary: Brazil\n")
print(summary(hydro_wide$Brazil_Hydro))

cat("\nBasic Summary: India\n")
print(summary(hydro_wide$India_Hydro))

summary_stats <- function(x) {
  data.frame(
    Min       = min(x, na.rm = TRUE),
    Q1        = quantile(x, 0.25, na.rm = TRUE),
    Median    = median(x, na.rm = TRUE),
    Mean      = mean(x, na.rm = TRUE),
    Q3        = quantile(x, 0.75, na.rm = TRUE),
    Max       = max(x, na.rm = TRUE),
    SD        = sd(x, na.rm = TRUE),
    Range     = max(x, na.rm = TRUE) - min(x, na.rm = TRUE)
  )
}

Brazil_summary <- summary_stats(hydro_wide$Brazil_Hydro)
India_summary  <- summary_stats(hydro_wide$India_Hydro)

cat("\nDescriptive Statistics: Brazil\n")
print(Brazil_summary)

cat("\nDescriptive Statistics: India\n")
print(India_summary)

x <- data_clean$`Hydropower (terawatt-hours)`
x <- x[x <= 500]
hist(x,
     freq = TRUE,
     main = "Overall Hydropower Generation",
     xlab = "Hydropower generation (TWh)",
     ylab = "Frequency",
     col = "skyblue",
     border = "black",
     ylim = c(0, 500),
     xlim = c(0, 500),
     breaks = seq(0, 500, 50),)

x <- hydro_wide$Brazil_Hydro
y <- hydro_wide$India_Hydro

hist(x,
     freq = FALSE,
     main = "Brazil Hydropower generation with Normal Curve",
     xlab = "Hydropower generation (TWh)",
     col  = "skyblue")
curve(dnorm(x, mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)),
      add = TRUE, lwd = 2)

hist(y,
     freq = FALSE,
     main = "India Hydropower generation with Normal Curve",
     xlab = "Hydropower generation (TWh)",
     col  = "skyblue")
curve(dnorm(x, mean = mean(y, na.rm = TRUE), sd = sd(y, na.rm = TRUE)),
      add = TRUE, lwd = 2)

wtest <- wilcox.test(x, y, paired = TRUE)
wtest
wtest$statistic   # W value
wtest$p.value     # p-value
cat("\n Wilcoxon Signed-Rank Test: W =", wtest$statistic,
    "p-value =", wtest$p.value, "\n")

boxplot(hydro_wide$Brazil_Hydro,
        hydro_wide$India_Hydro,
        names = c("Brazil", "India"),
        ylab  = "Hydropower generation (TWh)",
        main  = "Hydropower generation : Brazil vs India",
        col = c("deepskyblue2", "yellow"))