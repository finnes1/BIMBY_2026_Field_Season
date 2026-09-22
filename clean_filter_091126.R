#### Filter and Cleaning Script ####
### Freya Innes
### 11/09/2026

# Open Packages
library(tidyverse)
#library()

#### READING IN DATA ####
setwd("~/Desktop/School/Graduate/BIMBY_2026_Field_Season")

butterflies_raw <- read_csv("Raw Data/BIMBY-field-work-butterfly-2026.csv")
flowers_raw <- read_csv("Raw Data/BIMBY-field-work-flowers-2026.csv")
nectar_raw <- read_csv("Raw Data/BIMBY-field-work-nectaring-2026.csv")

#### FILTER AND DOUBLE CHECK BUTTERFLY SURVEYS ####
butterflies_filtered <- butterflies_raw %>% # Making a filtered dataset
  select(-starts_with("...")) %>% # Removes junk columns that come with download from Google Sheets
  mutate(temp_avg = (temp_beg + temp_end)/2,
         wind_avg = (wind_beg + wind_end)/2,
         duration_s = time_end - time_beg) %>% # Creates average temperature, average wind, and duration columns
  filter(wind_avg < 4,
         temp_avg >= 15,
         !(weather == "cloudy" & temp_avg < 18)) %>% # Filters for standardised butterfly conditions
  select(month, day, week, observer, transect, lat, lon, elevation_m, time_beg, duration_s,
         temp_avg, wind_avg, air_quality, weather, family, genus, species, number) 

ggplot(butterflies_filtered, aes(x = transect, y = duration_s, colour = week)) +
  geom_point() +
  theme_bw() # checking time data is correctly inputted





