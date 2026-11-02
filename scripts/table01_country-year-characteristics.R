# Import data and libraries ####
library(tidyverse)
library(here)
library(table1)

df <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Prepare data for Table 1 ####
df$armed_conflict <-
  factor(df$armed_conflict, levels = c(1,0),
  labels = c("Armed conflict present", "Armed conflict absent"))

df <- df %>%
  mutate(
    armed_conflict = as.character(armed_conflict),
    armed_conflict = ifelse(
      is.na(armed_conflict),
      "Missing",
      armed_conflict
    )
  )

df$drought <-
  factor(df$drought, levels = c(1,0),
  labels = c("Present", "Absent"))
df$earthquake <-
  factor(df$earthquake, levels = c(1,0),
  labels = c("Present", "Absent"))
df$oecd <-
  factor(df$oecd, levels = c(1,0),
  labels = c("Member", "Non-member"))

label(df$drought) <- "Any drought"
label(df$earthquake) <- "Any earthquake"
label(df$oecd) <- "OECD membership status"
label(df$gdp_1000) <- "GDP per capita"
label(df$pop_dens) <- "Population density"
label(df$urban) <- "Urban residence"
label(df$age_dep) <- "Age dependency ratio"
label(df$male_edu) <- "Male education"
label(df$temp) <- "Temperature"
label(df$rainfall_1000) <- "Rainfall "

units(df$gdp_1000) <- "$USD"
units(df$age_dep) <- "per 100 working-age individuals"
units(df$male_edu) <- "years per capita"
units(df$temp) <- "Celsius"
units(df$rainfall_1000) <- "mm per year"

caption  <- "Table 1. Characteristics of Each Country-Year Observation from 2000-2019"
footnote <- "GDP = gross domestic product, mm = millimitres, OECD = Organisation for Econoomic Co-operation and Development. 
<br>Armed conflict was defined as ≥25 conflict-related deaths in a calendar year. 
GDP per capita was scaled up by a factor of 1,000. 
Population density represents the proportion of a population living with a density of at least 1,000 people/km². 
Age dependency ratio represents the number of dependents (individuals under 15 years or over 64 years of age) per 100 working-age individuals. 
Male education represents age-standardised years per capita. 
Rainfall was scaled down by a factor of 1,000."

table1(~ drought + earthquake + oecd + gdp_1000 + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 | armed_conflict, data = df,
    overall = c(left = "Overall"), caption = caption, footnote = footnote)
