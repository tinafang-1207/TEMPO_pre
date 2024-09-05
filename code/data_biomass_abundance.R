

#### clean working space ####
rm(list = ls())

#### load in packages ####
library(tidyverse)


#### read in raw data ####
data_orig <- read.csv("data/data_original.csv")

#### Directories
plotdir <- "figure"

### clean data ####

data <- data_orig %>%
  janitor::clean_names() %>%
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


data_biomass <- data %>%
  select(ref_id, doi, first_author, country, biomass_change_orig, biomass_change_cleaned) %>%
  mutate(biomass_change_final = case_when(biomass_change_cleaned == "N/A"~"Unknown",
                                          biomass_change_cleaned == "Increase (even after harvest)"|biomass_change_cleaned =="Increase (decrease after harvest)"|biomass_change_cleaned=="Increase (return to baseline 7-10 days after harvest)"~"Increase",
                                          biomass_change_cleaned == "Lower"~"Decrease",
                                          .default = biomass_change_cleaned
                                          ))

data_biomass_count <- data_biomass %>%
  group_by(biomass_change_final) %>%
  summarize(biomass_change_count = n()) %>%
  mutate(biomass_change_percentage = biomass_change_count/sum(biomass_change_count)) %>%
  mutate(biomass_change_converted = biomass_change_percentage * 100) %>%
  mutate(biomass_change_converted = round(biomass_change_converted,0))%>%
  mutate(ymax = cumsum(biomass_change_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(biomass_change_final, "\n ", biomass_change_converted, "%"))

category_biomass <- c("Unknown" = "#ed7d31", "Mixed" = "#ffc000", "Increase" = "#69b647", "No change" = "#3d85c6", "Decrease" = "#bf2525")

g_biomass <- ggplot(data_biomass_count, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = biomass_change_final)) +
  geom_rect() +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "", values = category_biomass) +
  coord_polar(theta = "y") +
  labs(title = "Biomass Change") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_biomass

ggsave(g_biomass, filename = file.path(plotdir, "Fig4a_Biomass_change.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)


data_abundance <- data %>%
  select(ref_id, doi, first_author, country, abundance_change_orig, abundance_change_cleaned) %>%
  mutate(abundance_change_cleaned = case_when(abundance_change_cleaned=="N/A"~"Unknown",
                                              abundance_change_cleaned== "No change"~"Same",
                                              .default = abundance_change_cleaned))


data_abundance_count <- data_abundance %>%
  group_by(abundance_change_cleaned) %>%
  summarize(abundance_change_count = n()) %>%
  mutate(abundance_change_percentage = abundance_change_count/sum(abundance_change_count)) %>%
  mutate(abundance_change_converted = abundance_change_percentage * 100) %>%
  mutate(abundance_change_converted = round(abundance_change_converted,0))%>%
  mutate(ymax = cumsum(abundance_change_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(abundance_change_cleaned, "\n ", abundance_change_converted, "%"))



category_abundance <- c("Unknown" = "#ed7d31", "Increase" = "#69b647", "Same" = "#3d85c6", "Decrease" = "#bf2525")

g_abundance <- ggplot(data_abundance_count, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = abundance_change_cleaned)) +
  geom_rect() +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "", values = category_abundance) +
  coord_polar(theta = "y") +
  labs(title = "Abundance Change") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_abundance

ggsave(g_abundance, filename = file.path(plotdir, "Fig4b_abundance_change.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)


         