
#### clean working space ####
rm(list = ls())

#### load in packages ####
library(tidyverse)
library(rnaturalearth)
library(sf)
library(maps)
library(rworldmap)


#### read in raw data ####
data_orig <- read.csv("data/data_original.csv")

country_location <- read.csv("data/country_location.csv")

### clean data ####

data <- data_orig %>%
  janitor::clean_names() %>%
  drop_na(x) %>%
  select(-x) %>%
  rename(full_author = full_author_list,
         closure_numbers = number_of_closures_studied_in_case_paper_if_paper_is_one_case,
         actors_overall_orig = actors_involved_with_closure_types_of_actors_organization_names,
         governance_type_overall = goverance_type_overall_bottom_up_comgmt_top_down,
         closure_names = short_type_of_closure_name,
         actors_design_orig = types_of_stakeholders_involved_in_design_number_of_stakeholders_if_available_approx_ok,
         governance_type_design = governance_type_design_bottom_up_comgmt_top_down,
         actors_type_design = design_types_of_actors,
         closure_size_orig = size_of_closure,
         closure_size_std = size_of_closure_standardized_to_ha_1_km2_100_ha,
         closure_range_closed_orig = length_of_time_closed_range,
         closure_range_closed_std = length_of_time_closed_range_standardized_to_years,
         closure_range_closed_bins = length_of_time_closed_bins,
         closure_range_open_orig = length_of_time_open_range,
         closure_range_open_std = length_of_time_open_range_standardized_to_days,
         closure_range_open_bins = length_of_time_open_bins,
         biomass_change_orig = biomass_of_target_fishes_report_changes_through_time_if_available,
         biomass_change_cleaned = biomass_summary_of_closed_area_relative_to_open_increase_no_change_decrease,
         abundance_change_orig = abundance_of_target_fishes,
         abundance_change_cleaned = abundance_summary_increase_no_change_decrease,
         compliance_orig = perceived_compliance_through_time_if_available,
         compliance_cleaned = compliance_high_medium_low,
         success_orig = is_the_closure_a_success_or_not_why,
         success_cleaned = success_categorized_yes_no_to_paper)

# clean for location data
data_location <- data %>%
  select(ref_id, doi, first_author, country, continent, closure_numbers, closure_names) %>%
  mutate(country = case_when(country == "Papua New Guinea "~"Papua New Guinea",
                             country == "Indonesia "~"Indonesia",
                             country == "Madagascar "~"Madagascar",
                             country == "Chile"~"Chile(Easter Islands)",
                             country == "United States"~"United States(Hawaii)",
                             .default = as.character(country))) %>%
  mutate(closure_names = case_when(closure_names == "Periodically Harvested Closure"~"Periodic Closure",
                                   closure_names == "Sasi (Periodically Harvested Closure)"~"Periodic Closure",
                                   closure_names == "Rāhui (Spatial Temporary Closure)"~"Rāhui",
                                   closure_names == "Tapu"~"Seasonal Closure",
                                   closure_names == "Locally Managed Marine Area (LMMA) / Temporary Closure"~"Temporary Closure(Others)",
                                   closure_names == "Temporary Fisheries Closure"~"Temporary Closure(Others)",
                                   .default = as.character(closure_names))) %>%
  mutate(closure_numbers = ifelse(is.na(closure_numbers), 1, closure_numbers)) %>%
  left_join(country_location, by = "country")

plain <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5)
)

world <- map_data("world") %>%
  filter(region)



  



  
