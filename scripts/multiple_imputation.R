library(tidyverse)
library(mice)
library(here)
library(modelsummary)
library(flextable)

source(here("scripts", "regression.R"))

midata <- finaldata |>
  mutate(iso_num = as.numeric(as.factor(finaldata$iso))) |>
  select(-iso)

mice0  <- mice(midata, seed = 100, m = 5, maxit = 0, print = F)

meth <- mice0$method
meth[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality")] <- "2l.lmer"

pred <- mice0$predictorMatrix
pred[c("urban", "male_edu", "temp", "rainfall_1000", "loggdp", "pop_dens",
       "maternal_mortality", "infant_mortality", "neonatal_mortality", "under5_mortality"), "iso_num"] <- -2

mice.multi.out  <- mice(midata, seed = 100, m = 10, maxit = 20,
                        method = meth,
                        predictorMatrix = pred)

plot(mice.multi.out)

save(mice.multi.out, file = "outputs//mice_out.RData")

fit.mi.matmor <- with(mice.multi.out, 
                      lm(maternal_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.un5mor <- with(mice.multi.out, 
                      lm(under5_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.infmor <- with(mice.multi.out, 
                      lm(infant_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))
fit.mi.neomor <- with(mice.multi.out, 
                      lm(neonatal_mortality ~ -1 + armed_conflict + loggdp + oecd + pop_dens + urban + 
                           age_dep + male_edu + temp + rainfall_1000 + earthquake + drought + 
                           as.factor(iso_num) + as.factor(year)))

out.mi.matmor <- pool(fit.mi.matmor)
out.mi.un5mor <- pool(fit.mi.un5mor)
out.mi.infmor <- pool(fit.mi.infmor)
out.mi.neomor <- pool(fit.mi.neomor)

summary(out.mi.matmor)
summary(out.mi.un5mor)
summary(out.mi.infmor)
summary(out.mi.neomor)

models <- list(
  "Maternal mortality ratio per 100,000 live births" = plm_maternal,
  "Under 5 mortality rate per 1,000 live births" = plm_under5,
  "Infant mortality rate per 1,000 live births" = plm_infant,
  "Neonatal mortality rate per 1,000 live births" = plm_neonatal,
  "Maternal mortality ratio per 100,000 live births" = out.mi.matmor,
  "Under 5 mortality rate per 1,000 live births" = out.mi.un5mor,
  "Infant mortality rate per 1,000 live births" = out.mi.infmor,
  "Neonatal mortality rate per 1,000 live births" = out.mi.neomor
)

tab <- modelsummary(
  models,
  coef_map = c(
     "armed_conflict" = "Armed conflict, yes vs. no",
    "loggdp" = "GDP per capita (log)",
    "oecd" = "OECD membership, yes vs. no",
    "pop_dens" = "Population density",
    "urban" = "Urban residence",
    "age_dep" = "Age dependency ratio",
    "male_edu" = "Male education",
    "temp" = "Temperature",
    "rainfall_1000" = "Rainfall",
    "earthquake" = "Earthquake, yes vs. no",
    "drought" = "Drought, yes vs. no"
  ),
  stars = TRUE,
  statistic = "conf.int",
  fmt = 3,
  title = "Table 3. Linear Regression Coefficients for Mortality Outcomes Based on Complete-Case Analysis and Multiple Imputation Models, Beta Coefficient [95% Confidence Interval]",
  output = "flextable"
)
tab <- add_header_row(
  tab,
  values = c(
    " ",
    "Complete-case analysis models",
    "Multiple imputation models"
  ),
  colwidths = c(1, 4, 4)
)

tab <- add_footer_lines(
  tab,
  values = "GDP = gross domestic product, mm = millimitres, OECD = Organisation for Econoomic Co-operation and Development. Armed conflict was defined as at least 25 conflict-related deaths in a calendar year. GDP per capita was scaled up by a factor of 1,000. Population density represents the proportion of a population living with a density of at least 1,000 people/square km. Age dependency ratio represents the number of dependents (individuals under 15 years or over 64 years of age) per 100 working-age individuals. Male education represents age-standardised years per capita. Rainfall was scaled down by a factor of 1,000."
)

tab <- font(
  tab,
  fontname = "Times New Roman",
  part = "all"
)

save_as_html(
  tab,
  path = here("outputs", "table03_linear-regression-coefficients-cca-mi.html")
)
