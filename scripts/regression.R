library(plm)
library(here)
finaldata <- read.csv(here("data", "processed", "final_data.csv"))

finaldata$loggdp <- log(finaldata$gdp_1000)

preds <- as.formula(" ~ armed_conflict + loggdp + oecd + pop_dens + urban +
                  age_dep + male_edu + temp + rainfall_1000 + earthquake + drought")

plm_maternal <- plm(update.formula(preds, maternal_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
plm_under5 <- plm(update.formula(preds, under5_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
plm_infant <- plm(update.formula(preds, infant_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
plm_neonatal <- plm(update.formula(preds, neonatal_mortality ~ .),
              index = c("iso", "year"),
              effect = "twoways",
              model = "within",
              data = finaldata)
