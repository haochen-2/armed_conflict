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
  mutate(diff_matmor = maternal_mortality - maternal_mortality[1L]) 

df <- df %>%
  filter(year == 2000 | year == 2017)

df <- df %>%
  filter(diff_matmor >= 0)

ggplot(data = df, aes(x = year, y = diff_matmor, color = iso, group = iso)) +
  geom_line()
