# Import libraries and datasets
library(tidyverse)
library(here)
library(countrycode)
library(janitor)

matmor0 <- read.csv(here("data", "raw", "maternal_mortality.csv"), header = TRUE)
infmor0 <- read.csv(here("data", "raw", "infant_mortality.csv"), header = TRUE)
neomor0 <- read.csv(here("data", "raw", "neonatal_mortality.csv"), header = TRUE)
un5mor0 <- read.csv(here("data", "raw", "under5_mortality.csv"), header = TRUE)

# Examine mortality datasets
summary(matmor0)
summary(infmor0)
summary(neomor0)
summary(un5mor0)

# Write a function that converts the data to long format
make_long <- function(dfname, varname){
  dfname |>
    dplyr::select(iso, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year)
}

matmor <- make_long(dfname = matmor0, varname = "maternal_mortality")
infmor <- make_long(dfname = infmor0, varname = "infant_mortality")
neomor <- make_long(dfname = neomor0, varname = "neonatal_mortality")
un5mor <- make_long(dfname = un5mor0, varname = "under5_mortality")

# Prepare disaster data
disaster_events <- read.csv(
  here("data", "raw", "disaster.csv"),
  header = TRUE
) %>%
  clean_names() %>%
  filter(
    between(year, 2000, 2019),
    disaster_type %in% c("Earthquake", "Drought")
  ) %>%
  transmute(
    iso,
    year = as.integer(year),
    earthquake = as.integer(disaster_type == "Earthquake"),
    drought = as.integer(disaster_type == "Drought")
  ) %>%
  group_by(iso, year) %>%
  summarise(
    earthquake = max(earthquake),
    drought = max(drought),
    .groups = "drop"
  )

disaster <- matmor %>%
  distinct(iso, year) %>%
  left_join(disaster_events, by = c("iso", "year")) %>%
  mutate(
    earthquake = coalesce(earthquake, 0L),
    drought = coalesce(drought, 0L)
  )

# Prepare conflict data
conflict <- read.csv(here("data", "raw", "conflict.csv"), header = TRUE)

conflict_sum <- conflict %>%
  group_by(iso, year) %>%
  summarise(
    deaths_sum = if (all(is.na(best))) NA_real_ else sum(best, na.rm = TRUE),
    .groups = "drop"
  )

conflict_sum <- matmor %>%
  distinct(iso, year) %>%
  left_join(
    conflict_sum %>%
      transmute(
        iso,
        year = year + 1L,
        deaths_sum
      ),
    by = c("iso", "year")
  ) %>%
  mutate(
    armed_conflict = if_else(
      is.na(deaths_sum),
      NA_integer_,
      as.integer(deaths_sum >= 25)
    )
  ) %>%
  filter(between(year, 2000, 2019)) %>%
  select(iso, year, armed_conflict)

# Import covariates
covariates <- read.csv(here("data", "raw", "covariates.csv"), header = TRUE)

conflict_merged <- list(conflict_sum, disaster, matmor, infmor, neomor, un5mor, covariates)

conflict_merged <- conflict_merged %>%
  reduce(full_join, by = c('iso', 'year'))
conflict_merged$year <-  as.integer(conflict_merged$year)

# Export merged dataset
write.csv(conflict_merged, here("data", "processed", "final_data.csv"), row.names = FALSE)
