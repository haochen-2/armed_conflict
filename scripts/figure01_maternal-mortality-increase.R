# Import data and libraries ####
library(tidyverse)
library(here)
library(table1)

df <- read.csv(here("data", "processed", "final_data.csv"), header = TRUE)

# Calculate annual change in maternal mortality
df <- df %>%
  select(iso, year, maternal_mortality) %>%
  filter(year < 2018) %>%
  arrange(iso, year) %>%
  group_by(iso) %>%
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L]) %>%
  filter(first(year) == 2000,
        last(year) == 2017,
        last(diff_matmor) > 0)

ggplot(data = df, aes(x = year, y = maternal_mortality, colour = iso)) +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Figure 1. Line Graph for Countries with an Increase in Maternal Mortality\nfrom 2000 to 2017",
    y = "Maternal mortality ratio per 100,000 live births (log[10] scale)",
    x = "Year",
    colour = "Country code",
  ) + theme(
    plot.title = element_text(hjust = 0.5)
  ) + scale_y_log10()

