
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


# clean for governance data

data_governance <- data %>%
  select(ref_id, doi, first_author, country, actors_overall_orig, governance_type_overall, actors_design_orig, governance_type_design, actors_type_design) %>%
  #Assign NA to blank or unknown space
  mutate(actors_overall_orig = ifelse(actors_overall_orig == "N/a", NA, actors_overall_orig)) %>%
  mutate(actors_design_orig = ifelse(actors_design_orig == "N/a", NA, actors_design_orig)) %>%
  # Assign unknown to summarized governance type
  mutate(governance_type_overall = ifelse(is.na(actors_overall_orig), "Unknown", governance_type_overall)) %>%
  mutate(governance_type_design = ifelse(is.na(actors_design_orig), "Unknown", governance_type_design)) %>%
  mutate(actors_type_design = ifelse(is.na(actors_design_orig), "Unknown", actors_type_design)) %>%
  # recategorize actors type in design
  mutate(actors_type_design_new = case_when(actors_type_design == "Community leaders, villagers"~"Community leaders & Locals",
                                            actors_type_design == "Villagers, external advisors"~"Locals & Others",
                                            actors_type_design == "Community leaders, NGO"~"Community leaders & NGO",
                                            actors_type_design == "Community leaders, NGO, regional government"~"Community leaders & NGO & Government",
                                            actors_type_design == "Villagers, NGO"~"Locals & NGO",
                                            actors_type_design == "Community leaders, villagers, elementary school"~"Community leaders & Locals & Others",
                                            actors_type_design == "Community leaders, villagers, NGOs, government, researchers"~"Community leaders & Locals & NGO & Government & Others",
                                            actors_type_design == "Community leaders, environmental activists, government, researchers"~"Community leaders & Government & Others",
                                            actors_type_design == "Villagers, government"~"Locals & Government",
                                            actors_type_design == "Villagers, NGOs, government"~"Locals & NGO & Government",
                                            actors_type_design == "Villagers"~"Locals",
                                            actors_type_design == "Local committee"~"Locals",
                                            .default = actors_type_design)) %>%
  separate(col = actors_type_design_new, sep = "&", into = c("actor_type_1", "actor_type_2", "actor_type_3", "actor_type_4", "actor_type_5")) %>%
  mutate(actor_type_1 = case_when(actor_type_1 == "Community leaders "~"Community leaders",
                                  actor_type_1 == "Locals "~"Locals",
                                  .default = actor_type_1)) %>%
  mutate(actor_type_2 = case_when(actor_type_2 == " NGO"~"NGO",
                                  actor_type_2 == " NGO " ~"NGO",
                                  actor_type_2 == " Locals "~"Locals",
                                  actor_type_2 == " Locals"~"Locals",
                                  actor_type_2 == " Government "~"Government",
                                  actor_type_2 == " Government"~"Government",
                                  actor_type_2 == " Others"~"Others",
                                  .default = actor_type_2)) %>%
  mutate(actor_type_3 = case_when(actor_type_3 == " Government"~"Government",
                                  actor_type_3 == " Others"~"Others",
                                  actor_type_3 ==  " NGO " ~"NGO",
                                  .default = actor_type_3)) %>%
  mutate(actor_type_4 = case_when(actor_type_4 == " Government "~"Government",
                                  .default = actor_type_4)) %>%
  mutate(actor_type_5 = case_when(actor_type_5 ==  " Others"~"Others",
                                  .default = actor_type_5))


############# make donut figure for governance overall #########

data_governance_type_overall <- data_governance %>%
  select(ref_id, first_author, country,governance_type_overall) %>%
  group_by(governance_type_overall) %>%
  # count the numbers in each group
  summarize(governance_type_count = n()) %>%
  mutate(governance_type_percentage = governance_type_count/sum(governance_type_count)) %>%
  mutate(governance_type_converted = governance_type_percentage * 100) %>%
  mutate(governance_type_converted = round(governance_type_converted,0))%>%
  mutate(ymax = cumsum(governance_type_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(governance_type_overall, "\n ", governance_type_converted, "%"))

type_color <- c("Unknown" = "#ed7d31", "Bottom-up" = "#ffc000", "Co-management" = "#69b647", "Top-down" = "#3d85c6")

g_governance_overall <- ggplot(data_governance_type_overall, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = governance_type_overall)) +
  geom_rect() +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "Governance Type", values = type_color) +
  coord_polar(theta = "y") +
  labs(title = "Governance Type (Overall)", tag = "A") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")


g_governance_overall

############# make donut figure for governance overall (design) #########


data_governance_type_design <- data_governance %>%
  select(ref_id, first_author, country,governance_type_design) %>%
  group_by(governance_type_design) %>%
  # count the numbers in each group
  summarize(governance_design_count = n()) %>%
  mutate(governance_design_percentage = governance_design_count/sum(governance_design_count)) %>%
  mutate(governance_design_converted = governance_design_percentage * 100) %>%
  mutate(governance_design_converted = round(governance_design_converted,0))%>%
  mutate(ymax = cumsum(governance_design_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(governance_type_design, "\n ", governance_design_converted, "%"))

g_governance_design <- ggplot(data_governance_type_design, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = governance_type_design)) +
  geom_rect() +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "Governance Type", values = type_color) +
  coord_polar(theta = "y") +
  labs(title = "Governance Type (Design)", tag = "B") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_governance_design


############# make donut figure for actors involved (design) #########

data_actors_design_1 <- data_governance %>%
  select(ref_id, doi, first_author, country, actor_type_1, actor_type_2, actor_type_3, actor_type_4, actor_type_5) %>%
  group_by(actor_type_1) %>%
  summarize(actor_type_count = n()) %>%
  rename(actor_type = actor_type_1) %>%
  mutate(source = "actors_type_1")
  

data_actors_design_2 <- data_governance %>%
  select(ref_id, doi, first_author, country, actor_type_1, actor_type_2, actor_type_3, actor_type_4, actor_type_5) %>%
  group_by(actor_type_2) %>%
  summarize(actor_type_count = n()) %>%
  rename(actor_type = actor_type_2) %>%
  mutate(source = "actors_type_2")


data_actors_design_3 <- data_governance %>%
  select(ref_id, doi, first_author, country, actor_type_1, actor_type_2, actor_type_3, actor_type_4, actor_type_5) %>%
  group_by(actor_type_3) %>%
  summarize(actor_type_count = n()) %>%
  rename(actor_type = actor_type_3) %>%
  mutate(source = "actors_type_3")

data_actors_design_4 <- data_governance %>%
  select(ref_id, doi, first_author, country, actor_type_1, actor_type_2, actor_type_3, actor_type_4, actor_type_5) %>%
  group_by(actor_type_4) %>%
  summarize(actor_type_count = n()) %>%
  rename(actor_type = actor_type_4) %>%
  mutate(source = "actors_type_4")

data_actors_design_5 <- data_governance %>%
  select(ref_id, doi, first_author, country, actor_type_1, actor_type_2, actor_type_3, actor_type_4, actor_type_5) %>%
  group_by(actor_type_5) %>%
  summarize(actor_type_count = n()) %>%
  rename(actor_type = actor_type_5) %>%
  mutate(source = "actors_type_5")

data_actors_overall <-bind_rows(data_actors_design_1, data_actors_design_2, data_actors_design_3, data_actors_design_4, data_actors_design_5) %>%
  na.omit() %>%
  group_by(actor_type) %>%
  summarize(actor_type_overall = sum(actor_type_count)) %>%
  mutate(actor_design_percentage = actor_type_overall/sum(actor_type_overall)) %>%
  mutate(actor_design_converted = actor_design_percentage * 100) %>%
  mutate(actor_design_converted = round(actor_design_converted,0))%>%
  mutate(ymax = cumsum(actor_design_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(actor_type, "\n ", actor_design_converted, "%"))


actor_color <- c("Unknown" = "#ed7d31", "Government" = "#ffc000", "Community leaders" = "#69b647", "NGO" = "#3d85c6", "Locals" = "#bf2525", "Others" = "#ff7cb8")

g_actor_design <- ggplot(data_actors_overall, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = actor_type)) +
  geom_rect() +
  geom_text(x = 4.4, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "Actor Type", values = actor_color) +
  coord_polar(theta = "y") +
  labs(title = "Actor Type (Design)", tag = "C") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_actor_design

########## save figures above ###########

ggsave(g_governance_overall, filename = file.path(plotdir, "Fig2a_Governance_Type_Overall.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)

ggsave(g_governance_design, filename = file.path(plotdir, "Fig2b_Governance_Type_Design.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)

ggsave(g_actor_design, filename = file.path(plotdir, "Fig2c_Actor_Type_Design.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)
