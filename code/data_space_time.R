

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

##### clean data and make figure for closure size #####

data_space <- data %>%
  select(ref_id, doi, first_author, country, closure_names, closure_size_orig, closure_size_std) %>%
  mutate(closure_size_orig = ifelse(closure_size_orig == "N/a"|closure_size_orig == "N/A", NA, closure_size_orig)) %>%
  mutate_all(na_if, "") %>%
  na.omit() %>%
  mutate(closure_size_std = case_when(closure_size_std == "1,550"~"1550",
                                      closure_size_std == "300,000"~"300000",
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
  mutate(label = paste0(closure_size_bin, "\n ", closure_size_converted, "%")) %>%
  mutate(hsize = 2)

type_color <- c("0-100ha" = "#7AD151FF", "100-200ha" = "#22A884FF", "200-500ha" = "#2A788EFF", ">500ha" = "#414487FF")

g_closure_size <- ggplot(data_space_count, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = closure_size_bin)) +
  geom_rect() +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "", values = type_color) +
  coord_polar(theta = "y") +
  labs(title = "Closure size") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_closure_size


g_closure_size_trail <- ggplot(data_space_count, aes(x = hsize, y = closure_size_converted, fill = closure_size_bin)) +
  geom_col() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(name = "", values = type_color) +
  coord_polar(theta = "y") +
  xlim(c(0.2, 2 + 0.5)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_closure_size_trail



ggsave(g_closure_size_trail, filename = file.path(plotdir, "Fig3a_Closure_Size.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)

##### clean data and make figure for closure time (closed) #####

data_time_closed <- data %>%
  select(ref_id, doi, first_author, country, closure_range_closed_orig, closure_range_closed_std, closure_range_closed_bins) %>%
  mutate(closure_range_closed_std = ifelse(closure_range_closed_orig == "N/a", NA, closure_range_closed_std)) %>%
  mutate(closure_range_closed_bins = ifelse(is.na(closure_range_closed_std)|closure_range_closed_std == "infinite", "Unknown", closure_range_closed_bins)) %>%
  mutate(closure_range_closed_bins = case_when(closure_range_closed_std>=1 & closure_range_closed_std<=5~"1-5 years",
                                               closure_range_closed_std>5~">5 years",
                                               .default = closure_range_closed_bins)) %>%
  mutate(closure_range_closed_bins = case_when(closure_range_closed_std == "infinite"~"Unknown",
                                               closure_range_closed_std == "1-9"~">5 years",
                                               closure_range_closed_std == "4-6, indefinite"~">5 years",
                                               closure_range_closed_std == "2-6"~">5 years",
                                               closure_range_closed_std == "0.5-2"~"1-5 years",
                                               .default = closure_range_closed_bins ))


data_time_closed_count<-data_time_closed %>%
  group_by(closure_range_closed_bins) %>%
  summarize(closure_range_closed_count = n()) %>%
  mutate(closed_range_percentage = closure_range_closed_count/sum(closure_range_closed_count)) %>%
  mutate(closed_range_converted = closed_range_percentage * 100) %>%
  mutate(closed_range_converted = round(closed_range_converted,0))%>%
  mutate(ymax = cumsum(closed_range_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(closure_range_closed_bins, "\n ", closed_range_converted, "%")) %>%
  mutate(closure_range_closed_bins = factor(closure_range_closed_bins, levels = c("Unknown", "<1 month", "2-11 months", "1-5 years", ">5 years"))) %>%
  mutate(hsize = 2)

closed_color <- c("Unknown" = "#cfe020ff", "<1 month" = "#7AD151FF", "2-11 months" = "#22a884ff", "1-5 years" = "#2a788eff", ">5 years" = "#365d8dff")

g_time_closed <- ggplot(data_time_closed_count, aes(ymax = ymax, ymin = ymin, xmax=4, xmin=3, fill = closure_range_closed_bins)) +
  geom_rect(stat = "identity") +
  geom_text(x = 4.5, aes(y = labelposition, label = label), color = "black", size = 3) +
  scale_fill_manual(name = "", values = closed_color) +
  coord_polar(theta = "y") +
  labs(title = "Closure Time Range") +
  xlim(c(1,4)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_time_closed

g_time_close_trail <- ggplot(data_time_closed_count, aes(x = hsize, y = closed_range_converted, fill = closure_range_closed_bins )) +
  geom_col() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4, color = "white", fontface = "bold") +
  scale_fill_manual(name = "", values = closed_color) +
  coord_polar(theta = "y") +
  xlim(c(0.2, 2 + 0.5)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")

g_time_close_trail

ggsave(g_time_close_trail, filename = file.path(plotdir, "Fig3b_Closure_Time_Closed.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)


##### clean data and make figure for closure time (open) #####

data_time_open <- data %>%
  select(ref_id, doi, first_author, country, closure_range_open_orig, closure_range_open_std, closure_range_open_bins) %>%
  mutate(closure_range_open_std = ifelse(is.na(closure_range_open_orig), NA, closure_range_open_std)) %>%
  mutate(closure_range_open_bins = ifelse(is.na(closure_range_open_std)|closure_range_open_std == "N/A", "Unknown", closure_range_open_bins)) %>%
  mutate(closure_range_open_bins = case_when(closure_range_open_bins == "rest of time"|closure_range_open_bins == "0 (never open)"~"Unknown",
                                             .default = closure_range_open_bins)) %>%
  mutate(closure_range_open_bins = case_when(closure_range_open_std == "6-18 months"~"1-2 years",
                                             .default = closure_range_open_bins))

data_time_open_count<-data_time_open %>%
  group_by(closure_range_open_bins) %>%
  summarize(closure_range_open_count = n()) %>%
  mutate(open_range_percentage = closure_range_open_count/sum(closure_range_open_count)) %>%
  mutate(open_range_converted = open_range_percentage * 100) %>%
  mutate(open_range_converted = round(open_range_converted,0))%>%
  mutate(ymax = cumsum(open_range_percentage)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  mutate(labelposition = ((ymax + ymin)/2)) %>%
  mutate(label = paste0(closure_range_open_bins, "\n ", open_range_converted, "%")) %>%
  mutate(closure_range_open_bins = factor(closure_range_open_bins, levels = c("Unknown", "1 day", "2-5 days", "1-2 weeks", "3-5 weeks", "2-9 months", "1-2 years"))) %>%
  mutate(hsize = 2)



open_color <- c("Unknown" = "#cfe020ff", "1 day" = "#7AD151FF", "2-5 days" = "#22a884ff", "1-2 weeks" = "#2a788eff", "3-5 weeks" = "#365d8dff", "2-9 months" = "#443a83ff", "1-2 years" = "#440154ff")


g_time_open_trail <- ggplot(data_time_open_count, aes(x = hsize, y = open_range_converted, fill = closure_range_open_bins )) +
  geom_col() +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3, color = "white", fontface = "bold") +
  scale_fill_manual(name = "", values = open_color) +
  coord_polar(theta = "y") +
  xlim(c(0.2, 2 + 0.5)) +
  theme_void() +
  theme(plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")
  

g_time_open_trail

ggsave(g_time_open_trail, filename = file.path(plotdir, "Fig3c_Closure_Time_Open.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)

