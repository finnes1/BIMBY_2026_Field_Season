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


#### Create data for all four relationships####
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






