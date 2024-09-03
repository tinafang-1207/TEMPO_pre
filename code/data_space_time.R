

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

data_space <- data %>%
  select(ref_id, doi, first_author, country, closure_names, closure_size_orig, closure_size_std) %>%
  mutate(closure_size_orig = ifelse(closure_size_orig == "N/a", NA, closure_size_orig)) %>%
  mutate_all(na_if, "") %>%
  na.omit() %>%
  mutate(closure_size_std = case_when(closure_size_std == "1,550"~"1550",
                                      .default = closure_size_std)) %>%
  mutate(closure_size_std = as.numeric(closure_size_std)) %>%
  mutate(closure_size_bin = case_when(closure_size_std<=100~"0-100ha",
                                      closure_size_std>100 & closure_size_std<=200~"100-200ha",
                                      closure_size_std>200 & closure_size_std<=500~"200-500ha",
                                      closure_size_std>500~">500ha"))

data_space_count <- data_space %>%
  group_by(closure_size_bin) %>%
  summarize(closure_size_count = n()) %>%
  mutate(closure_size_percentage = closure_size_count/sum(closure_size_count)) %>%
  mutate(closure_size_converted = closure_size_percentage * 100) %>%
  mutate(closure_size_converted = round(closure_size_converted,0))%>%
  mutate(ymax = cumsum(closure_size_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(closure_size_bin, "\n ", closure_size_converted, "%"))

type_color <- c("0-100ha" = "#ed7d31", "100-200ha" = "#ffc000", "200-500ha" = "#69b647", ">500ha" = "#3d85c6")

g_closure_size <- ggplot(data_space_count, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = closure_size_bin)) +
  geom_rect() +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "Governance Type", values = type_color) +
  coord_polar(theta = "y") +
  labs(title = "Closure size", tag = "A") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_closure_size

ggsave(g_closure_size, filename = file.path(plotdir, "Fig3a_Closure_Size.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)

