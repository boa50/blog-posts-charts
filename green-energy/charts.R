library(ggplot2)
library(dplyr)
library(tidyr)
library(janitor)
library(purrr)
library(stringr)

# Data from https://ourworldindata.org/global-energy-200-years
df <- read.csv("green-energy/global-energy-substitution.csv", 
               check.names = FALSE) %>% 
  clean_names() %>% 
  filter(year >= 2011) %>% 
  set_names(~ str_replace(., "_t_wh_substituted_energy", "")) %>% 
  select(-c(entity, code)) %>% 
  pivot_longer(!year, names_to = "energy_type", values_to = "consumption") %>% 
  mutate(energy_type = ifelse(
    energy_type %in% c("other_renewables", "wind", "solar", "hydropower"),
    "renewable",
    "non renewable"
  ))