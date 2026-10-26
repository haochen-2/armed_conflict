# Load packages
library(tidyverse)
library(here)

# Read the wide datasets
matmor0 <- read.csv(here("data", "raw", "maternal_mortality.csv"))
infmor0 <- read.csv(here("data", "raw", "infant_mortality.csv"))
neomor0 <- read.csv(here("data", "raw", "neonatal_mortality.csv"))
un5mor0 <- read.csv(here("data", "raw", "under5_mortality.csv"))

# Convert a wide dataset with X<year> columns to long format
make_long <- function(data, value_name) {
  data |>
    select(iso, matches("^X[0-9]{4}$")) |>
    pivot_longer(
      cols = matches("^X[0-9]{4}$"),
      names_to = "year",
      names_prefix = "X",
      names_transform = list(year = as.integer),
      values_to = value_name
    ) |>
    arrange(iso, year)
}

# Create long versions of each dataset
matmor <- make_long(matmor0, "maternal_mortality")
infmor <- make_long(infmor0, "infant_mortality")
neomor <- make_long(neomor0, "neonatal_mortality")
un5mor <- make_long(un5mor0, "under5_mortality")

# Read and standardize disaster data
disaster_events <- read.csv(here("data", "raw", "disaster.csv")) |>
  janitor::clean_names() |>
  filter(
    between(year, 2000, 2019),
    disaster_type %in% c("Drought", "Earthquake")
  ) |>
  transmute(
    iso,
    year = as.integer(year),
    drought = as.integer(disaster_type == "Drought"),
    earthquake = as.integer(disaster_type == "Earthquake")
  ) |>
  group_by(iso, year) |>
  summarise(
    drought = max(drought),
    earthquake = max(earthquake),
    .groups = "drop"
  )

# Give every country-year in the mortality data a 0/1 disaster indicator
disaster <- matmor |>
  distinct(iso, year) |>
  left_join(disaster_events, by = c("iso", "year")) |>
  mutate(
    drought = coalesce(drought, 0L),
    earthquake = coalesce(earthquake, 0L)
  )

# Read conflict records and total the best estimate for each country-year
conflict_totals <- read.csv(here("data", "raw", "conflict.csv")) |>
  group_by(iso, year) |>
  summarise(best_total = sum(best, na.rm = TRUE), .groups = "drop")

# A country has conflict_presence = 1 when its prior year's total is at least 25
conflict <- matmor |>
  distinct(iso, year) |>
  left_join(
    conflict_totals |>
      transmute(
        iso,
        year = year + 1L,
        conflict_presence = as.integer(best_total >= 25)
      ),
    by = c("iso", "year")
  ) |>
  mutate(conflict_presence = coalesce(conflict_presence, 0L))

# Read country-year covariates
covariates <- read.csv(here("data", "raw", "covariates.csv"))

# Merge mortality outcomes, disaster indicators, conflict, and covariates
merged_data <- list(
  matmor,
  infmor,
  neomor,
  un5mor,
  disaster,
  conflict,
  covariates
) |>
  reduce(full_join, by = c("iso", "year")) |>
  arrange(iso, year)

# Export the final country-year dataset
write.csv(
  merged_data,
  here("data", "processed", "final_data_codex.csv"),
  row.names = FALSE
)
