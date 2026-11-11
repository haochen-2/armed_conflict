# Import data and libraries ####
library(tidyverse)
library(here)
library(table1)
library(webshot2)
library(htmltools)

df <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Prepare data for Table 1 ####
df$armed_conflict <-
  factor(df$armed_conflict, levels = c(1,0),
  labels = c("Armed conflict present", "Armed conflict absent"))

df$drought <-
  factor(df$drought, levels = c(1,0),
  labels = c("Present", "Absent"))
df$earthquake <-
  factor(df$earthquake, levels = c(1,0),
  labels = c("Present", "Absent"))
df$oecd <-
  factor(df$oecd, levels = c(1,0),
  labels = c("Member", "Non-member"))

label(df$drought) <- "<b>Any drought</b>"
label(df$earthquake) <- "<b>Any earthquake</b>"
label(df$oecd) <- "<b>OECD membership status</b>"
label(df$gdp_1000) <- "<b>GDP per capita</b>"
label(df$pop_dens) <- "<b>Population density</b>"
label(df$urban) <- "<b>Urban residence</b>"
label(df$age_dep) <- "<b>Age dependency ratio</b>"
label(df$male_edu) <- "<b>Male education</b>"
label(df$temp) <- "<b>Temperature</b>"
label(df$rainfall_1000) <- "<b>Rainfall</b>"

units(df$gdp_1000) <- "<b>$USD</b>"
units(df$male_edu) <- "<b>years per capita</b>"
units(df$temp) <- "<b>Celsius</b>"
units(df$rainfall_1000) <- "<b>mm per year</b>"

caption  <- "Table 1. Characteristics of Each Country-Year Observation from 2000-2019"
footnote <- "GDP = gross domestic product, mm = millimitres, OECD = Organisation for Econoomic Co-operation and Development. 
<br>Armed conflict was defined as ≥25 conflict-related deaths in a calendar year. 
GDP per capita was scaled up by a factor of 1,000. 
Population density represents the proportion of a population living with a density of at least 1,000 people/km². 
Age dependency ratio represents the number of dependents (individuals under 15 years or over 64 years of age) per 100 working-age individuals. 
Male education represents age-standardised years per capita. 
Rainfall was scaled down by a factor of 1,000."

tab1 <- table1(~ drought + earthquake + oecd + gdp_1000 + pop_dens + urban + age_dep + male_edu + temp + rainfall_1000 | armed_conflict, data = df,
    overall = c(left = "Overall"), caption = caption, footnote = footnote)
print(tab1)

htmltools::save_html(
  tab1,
  file = here("outputs", "table01_country-year-characteristics.html")
)
webshot(
  here("outputs", "table01_country-year-characteristics.html"),
  file = here("outputs", "table01_country-year-characteristics.pdf")
)
