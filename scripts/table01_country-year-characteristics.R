# Import data and libraries ####
library(tidyverse)
library(here)
library(table1)

df <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Prepare data for Table 1 ####
df <- df %>% filter(!is.na(armed_conflict))

df$armed_conflict <-
  factor(df$armed_conflict, levels = c(1,0),
  labels = c("Armed Conflict Presentᵃ", "Armed Conflict Absent"))
df$drought <-
  factor(df$drought, levels = c(1,0),
  labels = c("Present", "Absent"))
df$earthquake <-
  factor(df$earthquake, levels = c(1,0),
  labels = c("Present", "Absent"))
df$oecd <-
  factor(df$oecd, levels = c(1,0),
  labels = c("Member", "Non-member"))

label(df$drought) <- "Drought"
label(df$earthquake) <- "Earthquake"
label(df$oecd) <- "OECD Membership Status"
label(df$gdp_1000) <- "GDP per Capita"
label(df$pop_dens) <- "Population Density"
label(df$urban) <- "Urban Residence"
label(df$age_dep) <- "Age Dependency Ratio"
label(df$male_edu) <- "Male Education"
label(df$temp) <- "Temperature"
label(df$rainfall_1000) <- "Rainfall "

units(df$gdp_1000) <- "$USD, scaled up by 1,000x"
units(df$pop_dens) <- "%"
units(df$urban) <- "%"
units(df$age_dep) <- "per 100 working-age individuals"
units(df$male_edu) <- "years per capita"
units(df$temp) <- "Celsius"
units(df$rainfall_1000) <- "mm per year, scaled down by 1,000x"

caption  <- "Table 1. Characteristics of Countries in a Given Year from 2000-2018"
footnote <- "ᵃ Armed conflict was defined as ≥25 conflict-related deaths in a calendar year"

table1(~ drought + earthquake + oecd + gdp_1000 + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 | armed_conflict, data = df,
    overall = c(left = "Total"), caption = caption, footnote = footnote)
