
#### clean working space ####
rm(list = ls())

#### load in packages ####
library(tidyverse)
library(sf)
library(maps)


#### read in raw data ####
data_orig <- read.csv("data/data_original.csv")

country_location <- read.csv("data/country_location.csv")

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

# clean for location data
data_location <- data %>%
  select(ref_id, doi, first_author, country, continent, closure_numbers, closure_names) %>%
  mutate(country = case_when(country == "Papua New Guinea "~"Papua New Guinea",
                             country == "Indonesia "~"Indonesia",
                             country == "Madagascar "~"Madagascar",
                             country == "Chile"~"Chile(Easter Island)",
                             country == "United States"~"U.S.(Hawaii)",
                             country == "French Polynesia "~"French Polynesia",
                             country == "United States (U.S.)"~"U.S.",
                             .default = as.character(country))) %>%
  mutate(closure_names = case_when(closure_names == "Periodically Harvested Closure"~"Periodic Closure",
                                   closure_names == "Locally Managed Marine Area (LMMA) / Temporary Closure"~"Temporary Closure(Others)",
                                   closure_names == "Temporary Fisheries Closure"~"Temporary Closure(Others)",
                                   closure_names == "Temporary Closure"~"Temporary Closure(Others)",
                                   closure_names == "Temporary Closure/Dynamic Area Management (DAM)"~"Dynamic Closure",
                                   .default = as.character(closure_names))) %>%
  mutate(closure_numbers = ifelse(closure_numbers == "N/A", 1, closure_numbers)) %>%
  mutate(closure_numbers = as.numeric(closure_numbers)) %>%
  mutate(continent = factor(continent, levels = c("Oceania", "Africa", "Asia", "North America", "South America"))) %>%
  left_join(country_location, by = "country")

data_location_unique <- data_location %>%
  distinct(country, .keep_all = TRUE)


world <- map_data("world") %>%
  filter(region != "Antarctica")


# Color scheme
type_color <- c("Rotational Closure" = "#bf2525", "Periodic Closure" = "#ed7d31", "Traditional Closure" = "#ffc000", "Temporary Closure(Others)" = "#69b647", "Dynamic Closure" = "#3d85c6")


world_theme <- theme(
  axis.text = element_blank(),
  axis.line = element_blank(),
  axis.ticks = element_blank(),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.title = element_blank(),
  panel.background = element_rect(fill = "white"),
  plot.title = element_text(hjust = 0.5),
  legend.key = element_rect(fill = NA, color=NA),
  legend.background = element_rect(fill=alpha('blue', 0)),
  legend.title = element_text(size = 5),
  legend.text = element_text(size = 5)
)

worldplot <- ggplot() +
  geom_polygon(data = world, aes(x = long, y = lat, group = group), fill = "gray") +
  geom_point(data = data_location_unique, aes(x = country_long, y = country_lat), color = "#3d85c6") +
  geom_text(data = data_location_unique, mapping = aes(x = country_long, y = country_lat, label = country, hjust = hjust, vjust = vjust), size = 2) +
  coord_fixed(1.3) +
  world_theme + theme(legend.position = "none")



worldplot

bar_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

bar <- ggplot(data = data_location, aes(x = continent, y = closure_numbers, fill = closure_names)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(name = "Closure Types", values = type_color) +
  labs(x = "Continent", y = "Closure Cases") +
  guides(fill = guide_legend(reverse=TRUE)) +
  theme_bw() + bar_theme +
  theme(axis.title.x=element_blank(),
        legend.position = c(0.85, 0.8),
        legend.key.size = unit(0.18, "cm"))

bar


g <- gridExtra::grid.arrange(worldplot, bar, nrow = 2)
g

ggsave(g, filename = file.path(plotdir, "Fig1_Case_Distribution.png"),
       width = 6.5, height = 6.5, units = "in", dpi = 600)



