#### Drafting Answers to Research Questions Script ####
### Freya Innes
### 22/09/2026

# Open Packages
library(tidyverse)

#### READING IN DATA ####
setwd("~/Desktop/School/Graduate/BIMBY_2026_Field_Season")

butterflies_clean <- read_csv("Clean Data/butterflies_clean.csv")
flowers_clean <- read_csv("Clean Data/flowers_clean.csv")
nectar_clean <- read_csv("Clean Data/nectar_clean.csv")



#### Can floral resource richness and abundance predict butterfly richness and abundance? ####
# The idea behind this one is a four paneled grid that has the associations

# Making a usable butterfly summary
butterfly_summary <- butterflies_clean %>%
  mutate(date = make_date(year = 2026,
                          month = as.integer(month), 
                          day = as.integer(day)),
         butterfly_species = paste(genus, species)) %>% # Combining the date
  filter(butterfly_species != "NA NA") %>% # Removes rows with no species but the name "NA NA"
  group_by(transect, date) %>%
  summarise(butterfly_abundance = sum(number, na.rm = TRUE), # Making a column for butterfly abundance
            butterfly_richness = n_distinct(butterfly_species), # Making a column for species richness
            .groups = "drop")

# Making floral summary with native and non-native cover and richness in case I want to use it later
flower_summary <- flowers_clean %>%
  filter(!is.na(genus)) %>%  # drop placeholder "no plant in this quadrat" rows
  mutate(date = make_date(year = 2026,
                          month = as.integer(month),
                          day = as.integer(day)),
         plant_species = paste(genus, species)) %>%
  distinct(transect, date, quadrat, plant_species, origin, cover_pct) %>%  # Guard against duplicate entries
  group_by(transect, date) %>%
  complete(quadrat = 1:5, nesting(plant_species, origin), # Keeps species and their origin together instead of duplicating
           fill = list(cover_pct = 0)) %>%
  ungroup() %>%
  group_by(transect, date, origin, plant_species) %>%
  summarise(mean_cover = mean(cover_pct), .groups = "drop") %>%
  group_by(transect, date, origin) %>%
  summarise(floral_richness = n_distinct(plant_species),
            floral_cover = sum(mean_cover),
            .groups = "drop")

# Making a summary table to plot more easily
question1_data <- butterfly_summary %>%
  left_join(flower_summary, by = c("transect", "date"))

# Making the data array for the figure
figure_data <- bind_rows(
  question1_data %>%
    transmute(transect, date, origin,
              x = floral_richness,
              y = butterfly_richness,
              x_variable = "Floral richness",
              y_variable = "Butterfly richness"),
  question1_data %>%
    transmute(transect, date, origin,
              x = floral_cover,
              y = butterfly_richness,
              x_variable = "Floral % cover",
              y_variable = "Butterfly richness"),
  question1_data %>%
    transmute(transect, date, origin,
              x = floral_richness,
              y = butterfly_abundance,
              x_variable = "Floral richness",
              y_variable = "Butterfly abundance"),
  question1_data %>%
    transmute(transect, date, origin,
              x = floral_cover,
              y = butterfly_abundance,
              x_variable = "Floral % cover",
              y_variable = "Butterfly abundance"))

ggplot(figure_data, aes(x = x, y = y, color = origin, fill = origin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(y_variable ~ x_variable, scales = "free") +
  labs(x = NULL, y = NULL, color = "Origin", fill = "Origin") +
  theme_bw()


#### Which floral resources are disproportionately used by butterflies relative to their availability?####
# FLOWER DATA #

#Ensuring NAs are removed (can be removed once data is properyl cleaned)
flowers_clean <- flowers_clean %>%
  filter(!is.na(transect), !is.na(month), !is.na(day)) %>%
  mutate(date = make_date(year = 2026,
                          month = as.integer(month),
                          day = as.integer(day)),
         plant_species = paste(genus, species))

# Calculate mean floral cover for each plant species
flower_availability <- flowers_clean %>%
  filter(!is.na(quadrat)) %>% # Removes quadrats that are NA values
  group_by(transect, date, quadrat, plant_species) %>%
  summarise(cover = sum(`cover_%`, na.rm = TRUE),
            .groups = "drop") %>%
  complete(transect, date, 
           quadrat = 1:5,  # specifically calculates for 5 quadrats, not more (NAs)
           plant_species, fill = list(cover = 0)) %>%
  group_by(transect, date, plant_species) %>%
  summarise(mean_cover = mean(cover),
            n_quadrats = n(),
            .groups = "drop")

# Calculate total floral cover for each transect on a certain date
flower_totals <- flower_availability %>%
  group_by(transect, date) %>%
  summarise(total_floral_cover = sum(mean_cover, na.rm = TRUE),
            .groups = "drop")

# Calculate each plant's proportion of floral availability
flower_availability <- flower_availability %>%
  left_join(flower_totals, by = c("transect", "date")) %>%
  mutate(availability_proportion = mean_cover / total_floral_cover)


# NECTARING DATA #

# Filtering the dataset (can be removed once data is completed)
nectar_clean <- nectar_clean %>%
  filter(!is.na(transect), !is.na(month), !is.na(day)) %>%
  mutate(date = make_date(year = 2026,
                          month = as.integer(month),
                          day = as.integer(day)),
         plant_species = paste(flower_genus, flower_species))

# Counting nectar observations for each plant
nectaring_summary <- nectar_clean %>%
  group_by(transect, date, plant_species) %>%
  summarise(nectar_visits = n(), 
            .groups = "drop")

# Calculating the total nectar observations per transect on a certain date
nectar_totals <- nectaring_summary %>%
  group_by(transect, date) %>%
  summarise(total_nectar_visits = sum(nectar_visits, na.rm = TRUE),
            .groups = "drop")

# Calculate if nectar use was proportional 
nectaring_summary <- nectaring_summary %>%
  left_join(nectar_totals, by = c("transect", "date")) %>%
  mutate(use_proportion = nectar_visits / total_nectar_visits)


# COMBINING DATASETS #
question2_data <- flower_availability %>%
  select(transect, date, plant_species, mean_cover, availability_proportion) %>%
  full_join(nectaring_summary %>%
              select(transect, date, plant_species, nectar_visits, use_proportion),
            by = c("transect", "date", "plant_species")) %>%
  mutate(mean_cover = replace_na(mean_cover, 0),
         availability_proportion = replace_na(availability_proportion, 0),
         nectar_visits = replace_na(nectar_visits, 0), 
         use_proportion = replace_na(use_proportion, 0))

# Calculate selection ratio
#
# > 1 = used more than expected based on availability
# = 1 = used in proportion to availability
# < 1 = used less than expected

question2_data <- question2_data %>%
  mutate(selection_ratio = case_when(availability_proportion > 0 ~ use_proportion / availability_proportion,
                                     TRUE ~ NA_real_))

#  Plotting data
# Each point represents a plant species at a particular transect on a particular date
ggplot(question2_data, aes(x = availability_proportion, y = use_proportion)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line = use proportional to availability
  geom_point(alpha = 0.6) +
  labs(x = "Proportion of floral cover available",
       y = "Proportion of nectar observations",
       title = "Butterfly nectar use relative to floral availability") +
  theme_bw()

# Plant-level summary for a second figure
plant_summary <- question2_data %>%
  group_by(plant_species) %>%
  summarise(mean_availability = mean(availability_proportion, na.rm = TRUE),
            mean_use = mean(use_proportion, na.rm = TRUE),
            total_visits = sum(nectar_visits, na.rm = TRUE),
            .groups = "drop")

# Plant-level availability vs. use figure
ggplot(plant_summary, aes(x = mean_availability, y = mean_use, label = plant_species)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # 1:1 line = use proportional to availability
  geom_point(alpha = 0.6) +
  geom_text(nudge_y = 0.01, check_overlap = TRUE) +
  labs(x = "Mean proportional floral cover",
       y = "Mean proportional nectar use") +
  theme_bw()


#### Does butterfly use of native vs. non-native nectar plants change with seasonal variation in native floral availability?####

# FLORAL DATA #
flower_species_cover <- flowers_clean %>%
  group_by(transect, date, plant_species, origin) %>%
  summarise(mean_cover = mean(`cover_%`, na.rm = TRUE),
            .groups = "drop")

# Total floral cover by transect on a date
flower_totals <- flower_species_cover %>%
  group_by(transect, date) %>%
  summarise(total_floral_cover = sum(mean_cover, na.rm = TRUE),
            .groups = "drop")

# Calculating native floral availability
native_flower_availability <- flower_species_cover %>%
  group_by(transect, date) %>%
  summarise(native_floral_cover = sum(mean_cover[origin == "Native"], na.rm = TRUE),
            total_floral_cover = sum(mean_cover, na.rm = TRUE), # Mean cover of all flowers
            .groups = "drop") %>%
  mutate(proportion_native_floral_cover = native_floral_cover / total_floral_cover) # Proportion of floral cover that is native

# NECTARING DATA #
# Summary of native nectar visits
nectar_native_summary <- nectar_clean %>%
  group_by(transect, date, flower_origin) %>%
  summarise(nectar_visits = n(),
            .groups = "drop")

# Calculating total nectar observations
nectar_totals <- nectar_native_summary %>%
  group_by(transect, date) %>%
  summarise(total_nectar_visits = sum(nectar_visits, na.rm = TRUE),
            .groups = "drop")

# Calculating the proportion of nectar visits to native plants
native_nectar_use <- nectar_native_summary %>%
  filter(flower_origin == "Native") %>%
  group_by(transect, date) %>%
  summarise(native_nectar_visits = sum(nectar_visits, na.rm = TRUE),
            .groups = "drop") %>%
  left_join(nectar_totals, by = c("transect", "date")) %>%
  mutate(proportion_native_nectar = native_nectar_visits / total_nectar_visits)

# Combining the floral availability and nectar use
question3_data <- native_flower_availability %>%
  left_join(native_nectar_use, by = c("transect", "date"))

# PLOTTING #
# Plotting native floral availability through the season
ggplot(question3_data, aes(x = date, y = proportion_native_floral_cover)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Date", 
       y = "Proportion of floral cover that is native", 
       title = "Seasonal availability of native floral resources") +
  theme_bw()

# Plotting native nectar use through the season
ggplot(question3_data, aes(x = date, y = proportion_native_nectar)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(x = "Date",
       y = "Proportion of nectar observations on native plants",
       title = "Seasonal use of native flowers") +
  ylim(0, 1) +
  theme_bw()

# Plotting native floral availability vs. native nectar use
ggplot(question3_data, aes(x = proportion_native_floral_cover, y = proportion_native_nectar)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + # Adds the 1:1 use ratio
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "Proportion of floral cover that is native",
       y = "Proportion of nectar observations on native plants",
       title = "Native nectar use as a function of native floral availability") +
  theme_bw()






#### MAKING OTHER RANDOM PLOTS THAT I HAD THE CODE FOR ####
# Transect Locations
# Adding region to butterfly data
butterflies <- butterflies_clean %>%
  mutate(region = case_when(
    transect %in% c("GBH", "CCM", "BGP", "PSP") ~ "Lower Mainland",
    transect %in% c("CHP", "RBG", "BHT", "HRP") ~ "Vancouver Island",
    transect %in% c("MGT", "FLT") ~ "High Elevation Alpine",
    transect %in% c("MMF", "KPP", "PSL") ~ "Thompson-Nicola",
    transect %in% c("OHR", "OHRb", "PBM", "ABR", "JMS",
                    "KSP", "OKF", "KWL", "RTT", "PML") ~ "Okanagan-Similkameen",
    transect %in% c("UBC", "KMP", "RRP" ) ~ "Thompson-Okanagan"))

# Base map
canada_prov <- ne_states(country = "canada", returnclass = "sf")

# Load BC boundary
bc_boundary <- canada_prov %>%
  filter(gn_name == "British Columbia")

# Convert to sf structure to plot on map 
transects_sf <- st_as_sf(butterflies, coords = c("lon", "lat"), crs = 4326)

# Clip to BC
sf_bc <- st_join(transects_sf, bc_boundary, join = st_within, left = FALSE)

# Plot
ggplot() +
  geom_sf(data = bc_boundary, fill = "grey95") +
  geom_sf(data = sf_bc, aes(color = region.x)) +
  geom_sf_text(data = sf_bc, aes(label = transect),
               nudge_y = 0.05,
               size = 2) +
  coord_sf(xlim = c(-126.0, -119.0),
           ylim = c(48.0, 51)) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  labs(title = "BIMBY Transects in British Columbia",
       x = "Longitude",
       y = "Latitude",
       color = "Sampling Region")



# SAMPLING SUMMARY
# Binding together sampling summary
sampling_summary <- bind_rows(
  flowers_clean %>%
    distinct(transect, month, day, week) %>%
    count(transect) %>%
    mutate(dataset = "Flowers"),
  
  nectar_clean %>%
    distinct(transect, month, day) %>%
    count(transect) %>%
    mutate(dataset = "Nectar"),
  
  butterflies_clean %>%
    distinct(transect, month, day) %>%
    count(transect) %>%
    mutate(dataset = "Butterflies"))

# Plots the number of times floral surveys, butterfly surveys, and nectar observations have been recorded at each site
ggplot(sampling_summary, aes(x = reorder(transect, n), y = n, fill = dataset)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(x = "Transect", 
       y = "Number of sampling occasions", 
       fill = "Dataset") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())



# NATIVE VS. NON-NATIVE FLORAL COVER
# Averaging floral cover across five quadrats per transect
cover_transect <- flowers_clean %>%
  filter(!is.na(origin)) %>%
  group_by(month, day, week, transect, quadrat, origin) %>%
  summarise(cover = sum(`cover_%`, na.rm = TRUE), 
            .groups = "drop" ) %>%
  group_by(month, day, week, transect, origin) %>%
  summarise(mean_cover = mean(cover, na.rm = TRUE), 
            se = sd(cover, na.rm = TRUE) / sqrt(n()),
            .groups = "drop")

# Creating day-of-year
cover_transect <- cover_transect %>%
  mutate(date = as.Date(paste(2026, month, day, sep = "-")),
         julian = as.integer(format(date, "%j")),
         region = case_when(
           transect %in% c("GBH", "CCM", "BGP", "PSP") ~ "Lower Mainland",
           transect %in% c("CHP", "RBG", "BHT", "HRP") ~ "Vancouver Island",
           transect %in% c("MGT", "FLT") ~ "High Elevation Alpine",
           transect %in% c("MMF", "KPP", "PSL") ~ "Thompson-Nicola",
           transect %in% c("OHR", "OHRb", "PBM", "ABR", "JMS",
                           "KSP", "OKF", "KWL", "RTT", "PML") ~ "Okanagan-Similkameen",
           transect %in% c("UBC", "KMP", "RRP" ) ~ "Thompson-Okanagan"))

# Plotting mean % floral cover vs day of year
ggplot(cover_transect, aes(x = week, y = mean_cover, colour = origin)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) + # Change if changed below
  facet_wrap(~ region) + # Change if wanting season total, region, or individual transects
  scale_colour_manual(values = c("Native" = "forestgreen", "Non-native" = "orange")) +
  scale_x_continuous(breaks = seq(min(cover_transect$week), max(cover_transect$week), by = 1)) +
  labs(x = "Week",
       y = "Mean floral cover (%)",
       colour = "Plant origin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# NECTAR-USE AT TRANSECTS 
# Simple summary of nectar-use
nectar_summary <- nectar_clean %>%
  count(transect, flower_origin)

# Plots nectaring observations at each transect and whether they were on native or non-native plants
ggplot(nectar_summary, aes(x = reorder(transect, n), y = n, fill = flower_origin)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(
    values = c("Native" = "forestgreen", "Non-native" = "orange")) +
  labs(x = "Transect",
       y = "Number of nectaring observations",
       fill = "Flower origin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

# NECTAR-USE THROUGHOUT THE SEASON
# Adding julian date to nectaring data and summing the observations
nectar_day <- nectar_clean %>%
  mutate(date = as.Date(paste(2026, month, day, sep = "-")),
         julian = as.integer(format(date, "%j"))) %>%
  count(julian, flower_origin)

ggplot(nectar_day, aes(x = julian, y = n, colour = flower_origin)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = FALSE) + # Change for error bars if wanted
  scale_colour_manual(values = c("Native" = "forestgreen","Non-native" = "orange")) +
  labs(x = "Day of year",
       y = "Nectaring observations",
       colour = "Flower origin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#FLORAL AVAILABILITY VS NECTAR-USE 
# Calculating native floral cover for each transect and week
floral_availability <- flowers_clean %>%
  filter(!is.na(origin)) %>%
  group_by(week, transect, quadrat, origin) %>%
  summarise(cover = sum(`cover_%`, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(week, transect, origin) %>%
  summarise(mean_cover = mean(cover, na.rm = TRUE),
            .groups = "drop") %>%
  rename(flower_origin = origin)

# Nectar observations based on week, transect, and flower origin 
nectar_use <- nectar_clean %>%
  count(week, transect, flower_origin, name = "nectar_obs")

# Connecting datasets
nectar_availability <- floral_availability %>%
  left_join(nectar_use, by = c("week", "transect", "flower_origin")) %>%
  mutate(nectar_obs = replace_na(nectar_obs, 0))

# Plotting 
ggplot(nectar_availability, aes(x = mean_cover, y = nectar_obs, colour = flower_origin)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ flower_origin) +
  scale_colour_manual(values = c("Native" = "forestgreen", "Non-native" = "orange")) +
  labs(x = "Mean floral cover (%)",
       y = "Nectaring observations",
       colour = "Flower origin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# BUTTERFLY ABUNDANCE THROUGHOUT THE SEASON 
# Abundance at each transect during each week
butterfly_day <- butterflies_clean %>%
  group_by(month, day, week, transect) %>%
  summarise(butterfly_abundance = sum(number, na.rm = TRUE),
            species_richness = n_distinct(paste(genus, species),na.rm = TRUE), .groups = "drop") %>%
  mutate(date = as.Date(paste(2026, month, day, sep = "-")),
         julian = as.integer(format(date, "%j")),
         region = case_when(
           transect %in% c("GBH", "CCM", "BGP", "PSP") ~ "Lower Mainland",
           transect %in% c("CHP", "RBG", "BHT", "HRP") ~ "Vancouver Island",
           transect %in% c("MGT", "FLT") ~ "High Elevation Alpine",
           transect %in% c("MMF", "KPP", "PSL") ~ "Thompson-Nicola",
           transect %in% c("OHR", "OHRb", "PBM", "ABR", "JMS",
                           "KSP", "OKF", "KWL", "RTT", "PML") ~ "Okanagan-Similkameen",
           transect %in% c("UBC", "KMP", "RRP" ) ~ "Thompson-Okanagan"))

# Plots abundance (something wonky with the x-axis)
ggplot(butterfly_day, aes(x = julian, y = butterfly_abundance)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess",se = TRUE) +
  facet_wrap(~ region) +
  labs(x = "Day of year", 
       y = "Butterfly abundance") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Plots richness (still wonky)
ggplot(butterfly_day, aes(x = julian,y = species_richness)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE) +
  #facet_wrap(~ region) +
  labs(x = "Day of year",
       y = "Butterfly species richness") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())











# NOT READY YET
availability_prop <- floral_availability %>%
  group_by(week, transect) %>%
  mutate(availability_prop = mean_cover / sum(mean_cover)) %>%
  ungroup()

use_prop <- nectar_use %>%
  group_by(week, transect) %>%
  mutate(use_prop = nectar_obs / sum(nectar_obs)) %>%
  ungroup()

comparison <- availability_prop %>%
  left_join(use_prop, by = c("week", "transect", "flower_origin")) %>%
  mutate(use_prop = replace_na(use_prop, 0))

ggplot(comparison, aes(x = availability_prop, y = use_prop, colour = flower_origin)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_point(size = 3, alpha = 0.7) +
  scale_colour_manual(values = c("Native" = "forestgreen", "Non-native" = "orange")) +
  labs(x = "Proportion of floral availability",
       y = "Proportion of nectar use",
       colour = "Flower origin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# One point is a flower origin at a transect during a certain week















floral_availability <- flowers_clean %>%
  filter(!is.na(origin)) %>%
  group_by(week, transect, quadrat, origin) %>%
  summarise(cover = sum(`cover_%`, na.rm = TRUE), .groups = "drop") %>%
  group_by(week, transect, origin) %>% # Average the quadrats to get transect-level 
  summarise(mean_cover = mean(cover, na.rm = TRUE), .groups = "drop") %>%
  rename(flower_origin = origin)

nectar_use <- nectar_clean %>%
  filter(activity == "nectaring") %>%
  count(week, transect, flower_origin, name = "nectar_obs")

selection_data <- floral_availability %>%
  left_join(nectar_use, by = c("week", "transect", "flower_origin")) %>% # Add nectar observations
  mutate(nectar_obs = replace_na(nectar_obs, 0)) # No observations on that flower origin = zero observations

selection_data <- selection_data %>%
  group_by(week, transect) %>%
  mutate(availability_prop = mean_cover / sum(mean_cover),
         use_prop = nectar_obs / sum(nectar_obs)) %>%
  ungroup()

selection_data <- selection_data %>%
  filter(!is.na(use_prop), !is.na(availability_prop), availability_prop > 0)

selection_data <- selection_data %>%
  mutate(selection_ratio = use_prop/availability_prop)

selection_data %>%
  select(week, transect, flower_origin, availability_prop, use_prop, selection_ratio) %>%
  arrange(transect, week, flower_origin)


ggplot(selection_data, aes(x = flower_origin, y = selection_ratio, colour = flower_origin)) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_boxplot() +
  geom_jitter(size = 2.5, alpha = 0.5) +
  scale_colour_manual(values = c("Native" = "forestgreen",
                                 "Non-native" = "orange")) +
  labs(x = "Flower origin",
       y = "Selection ratio",
       colour = "Flower origin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# Summary of most common nectar plants visited
nectaring_summary_plot <- nectaring_summary %>%
  group_by(plant_species) %>%
  summarise(nectar_visits = sum(nectar_visits, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(desc(nectar_visits)) %>%
  mutate(plant_species = factor(plant_species,
                           levels = plant_species)) %>%
  filter(nectar_visits > 5)


ggplot(nectaring_summary_plot, aes(x = plant_species, y = nectar_visits)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x = "Flower",
       y = "Number of nectar observations",
       title = "Butterfly nectar plant use") 
