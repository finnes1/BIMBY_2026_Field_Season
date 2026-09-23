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

#### Can floral resource richness and abundance predict butterfly richness and abundance?####
# The idea behind this one is a four panelled grid that has the associations

butterfly_summary <- butterflies_clean %>%
  mutate(date = make_date(year = 2026,
                          month = as.integer(month), 
                          day = as.integer(day))) %>%
  group_by(transect, date) %>%
  summarise(butterfly_abundance = sum(number, na.rm = TRUE),
            butterfly_richness = n_distinct(paste(genus, species),na.rm = TRUE),
            .groups = "drop")

flowers_clean <- flowers_clean %>%
  filter(!is.na(transect), !is.na(month), !is.na(day)) %>%
  mutate(date = make_date(year = 2026,
                          month = as.integer(month),
                          day = as.integer(day)))


flower_summary <- flowers_clean %>%
  group_by(transect, date, genus, species) %>%
  summarise(mean_cover = mean(`cover_%`, na.rm = TRUE),
            .groups = "drop") %>%
  group_by(transect, date) %>%
  summarise(floral_richness = n_distinct(paste(genus, species)),
            floral_abundance = sum(mean_cover, na.rm = TRUE),
            .groups = "drop")

question1_data <- butterfly_summary %>%
  left_join(flower_summary, by = c("transect", "date"))


# Create data for all four relationships
figure_data <- bind_rows( question1_data %>%
                            transmute(transect, date, 
                                      x = floral_richness,
                                      y = butterfly_richness, 
                                      x_variable = "Floral richness", 
                                      y_variable = "Butterfly richness"),
                          question1_data %>%
                            transmute(transect, date,
                                      x = floral_abundance,
                                      y = butterfly_richness,
                                      x_variable = "Floral % cover",
                                      y_variable = "Butterfly richness"),
                          question1_data %>%
                            transmute(transect, date,
                                      x = floral_richness,
                                      y = butterfly_abundance,
                                      x_variable = "Floral richness",
                                      y_variable = "Butterfly abundance"),
                          question1_data %>%
                            transmute(transect, date,
                                      x = floral_abundance,
                                      y = butterfly_abundance,
                                      x_variable = "Floral % cover",
                                      y_variable = "Butterfly abundance"))

ggplot(figure_data, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(y_variable ~ x_variable, scales = "free") +
  labs(x = NULL, y = NULL) +
  theme_bw()


# Checking how many of each transect was surveyed
question1_data %>%
  count(transect, name = "n_surveys") %>%
  arrange(desc(n_surveys))

# Checking distributions
ggplot(question1_data, aes(x = butterfly_abundance)) +
  geom_histogram(binwidth = 1) +
  theme_classic()

ggplot(question1_data, aes(x = butterfly_richness)) +
  geom_histogram(binwidth = 1) +
  theme_classic()




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
