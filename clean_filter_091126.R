#### Filter and Cleaning Script ####
### Freya Innes
### 11/09/2026

# Open Packages
library(tidyverse)
library()

#### READING IN DATA ####
setwd("~/Desktop/School/Graduate/BIMBY 2026 Field Season Code/BIMBY 2026 Code") # set wd

butterflies_raw <- read_csv("BIMBY-field-work-butterfly-2026.csv")
flowers_raw <- read_csv("BIMBY-field-work-flowers-2026.csv")
nectar_raw <- read_csv("BIMBY-field-work-nectaring-2026.csv")
