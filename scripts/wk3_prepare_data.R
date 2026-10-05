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

# Derive country names from ISO codes
get_name <- function(x) {
  x$Country.Name <- countrycode(x$iso,
  origin = "iso3c",
  destination = "country.name")
  x <- x |>
  dplyr::mutate(country = Country.Name) |>
  dplyr::select(iso, country, indicator, X2000:X2019)
}

matmor_wide <- get_name(matmor0)
infmor_wide <- get_name(infmor0)
neomor_wide <- get_name(neomor0)
un5mor_wide <- get_name(un5mor0)

# Write a function that converts the data to long format
make_long <- function(dfname, varname){
  dfname |>
    dplyr::select(country, iso, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(country, iso, year)
}

matmor <- make_long(dfname = matmor_wide, varname = "maternal_mortality")
infmor <- make_long(dfname = infmor_wide, varname = "infant_mortality")
neomor <- make_long(dfname = neomor_wide, varname = "neonatal_mortality")
un5mor <- make_long(dfname = un5mor_wide, varname = "under5_mortality")

# Prepare disaster data
disaster <- read.csv(here("data", "raw", "disaster.csv"), header = TRUE)
disaster <- clean_names(disaster)

disaster <- disaster %>%
  filter(between(year, 2000, 2019) & disaster_type %in% c("Earthquake", "Drought")) %>%
  select(year, iso, disaster_type)

disaster <- disaster %>%
  mutate(earthquake = if_else(disaster_type == "Earthquake", 1, 0))
disaster$earthquake <-  as.integer(disaster$earthquake)

disaster <- disaster %>%
  mutate(drought = if_else(disaster_type == "Drought", 1, 0))
disaster$drought <-  as.integer(disaster$drought)

disaster <- disaster %>%
  select(-disaster_type)

# Prepare conflict data
conflict <- read.csv(here("data", "raw", "conflict.csv"), header = TRUE)

conflict_sum <- conflict %>%
  group_by(iso, year) %>%
  summarise(deaths_sum = sum(best, na.rm = TRUE))

conflict_sum <- conflict_sum %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(conflict_presence = if_else(lag(deaths_sum) >= 25, 1, 0))
conflict_sum$conflict_presence <-  as.integer(conflict_sum$conflict_presence)
conflict_sum <- conflict_sum %>%
  select(-deaths_sum)

# Merge all datasets
conflict_sum$country <- countrycode(conflict_sum$iso,
  origin = "iso3c",
  destination = "country.name")

conflict_merged <- list(conflict_sum, disaster, matmor, infmor, neomor, un5mor)

conflict_merged <- conflict_merged %>%
  reduce(full_join, by = c('iso', 'year'))
conflict_merged <- conflict_merged %>%
  select(-country, -country.y, -country.x.x, -country.y.y) %>%
  rename(country = country.x)
conflict_merged$year <-  as.integer(conflict_merged$year)
conflict_merged <- conflict_merged %>%
  select(iso, country, year, conflict_presence, earthquake, drought, maternal_mortality, infant_mortality, neonatal_mortality, under5_mortality)

# Export merged dataset
write.csv(conflict_merged, here("data", "processed", "merged_conflict_data.csv"), row.names = FALSE)
