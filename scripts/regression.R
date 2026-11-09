library(plm)
library(here)
library(stargazer)

finaldata <- read.csv(here("data", "processed", "final_data.csv"))

finaldata$loggdp <- log(finaldata$gdp_1000)

preds <- as.formula(" ~ armed_conflict + loggdp + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

plm_maternal <- plm(update.formula(preds, maternal_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
summary(plm_maternal)

plm_under5 <- plm(update.formula(preds, under5_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
summary(plm_under5)

plm_infant <- plm(update.formula(preds, infant_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
summary(plm_infant)

plm_neonatal <- plm(update.formula(preds, neonatal_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
summary(plm_neonatal)


stargazer(plm_maternal, plm_under5, plm_infant, plm_neonatal, type = "text",
          title = "Table 2. Linear Regression Coefficients for Mortality Outcomes",
          dep.var.caption = "Mortality outcomes",
          dep.var.labels = " ",
          model.numbers = FALSE,
          column.labels = c("Maternal mortality ratio per 100,000 live births", "Under 5 mortality rate per 1,000 live births", "Infant mortality rate per 1,000 live births", "Neonatal mortality rate per 1,000 live births"),
          covariate.labels=c("Armed conflict, yes vs. no","GDP per capita (log)","OECD membership, yes vs. no",
                             "Population density","Urban residence", "Age dependency ratio", "Male education",
                             "Temperature", "Rainfall", "Earthquake, yes vs. no", "Drought, yes vs. no"),
          notes = "GDP = gross domestic product, mm = millimitres, OECD = Organisation for Econoomic Co-operation and Development. Armed conflict was defined as at least 25 conflict-related deaths in a calendar year. GDP per capita was scaled up by a factor of 1,000. Population density represents the proportion of a population living with a density of at least 1,000 people/square km. Age dependency ratio represents the number of dependents (individuals under 15 years or over 64 years of age) per 100 working-age individuals. Male education represents age-standardised years per capita. Rainfall was scaled down by a factor of 1,000.",
          out = here("outputs", "table2_linear-regression-coefficients.html"))
