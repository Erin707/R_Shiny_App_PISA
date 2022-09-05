## Introduction ----
## This is a global file for data generation and processing
## Author: Erin_707
library(here)

packages <- c("shiny", "shinydashboard", "tidyverse", "plotly",
              "ggplot2", "DT", "scales", "here", "learningtower",
              "readxl", "janitor", "corrr", "psych", "sp", "sf")

for(i in seq_along(packages)) {
  require_package(packages[[i]])}

## Load relevant packages ----
source(here("r_scripts/utility.R"))
source(here("r_scripts/process.R"))

## Set relevant file paths ----
## Important to have the correct file struer in the working dir
data_path <- here("data")
rds_path <- here("intermediate_data/")


## Country variables

all_iso3 <- data_pisa_long %>% distinct(iso3)
concord_ctr_name_iso3 <- all_iso3 %>% 
  left_join(world_spdf@data %>% select(iso3, name), by = "iso3" ) %>% 
  filter(!is.na(name))

all_ctr_name <- concord_ctr_name_iso3 %>% 
  distinct(name) %>% 
  pull()
