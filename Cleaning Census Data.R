#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from cesR package.
# Author: Minhui Yu
# Data: December 21st 2020
# Contact:minhui.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to download the gss_cleaning R Script and run in your computer to get a csv called gss.csv.
# - Don't forget to gitignore it!

# Setup workspace.
library(haven)
setwd("/Users/helen/Documents/Courses/STA/STA304H1/Final Project")

# install.packages("tidyverse")
library(tidyverse)

# Clean the data
gss = read.csv("/Users/helen/Documents/Courses/STA/STA304H1/Assignment 2/gss.csv")

# Select column we want and drop NA
reduced_census <- gss %>% 
  select(age, sex, education, province) %>% 
  na.omit()

# Filter the census data
filtered_census <- reduced_census %>% 
  filter(age >= 18.0)

# Mutate age_group
mutate_census <- filtered_census %>% 
  mutate(age_group = case_when(age <  20.0 ~ "Under 20",
                               age >= 20.0 & age < 40.0 ~ "20 to 39",
                               age >= 40.0 & age < 60.0 ~"40 to 59",
                               age >= 60.0 & age < 80.0 ~  "60 to 79",
                               age >= 80.0 ~ "80 & Above 80")) 

# Mutate education
mutate_census <- mutate_census %>% 
  mutate(education = case_when(education == "Less than high school diploma or its equivalent" ~ "less than high school",
                               education == "High school diploma or a high school equivalency certificate" ~ "high school",
                               education == "Trade certificate or diploma" ~ "less than non-university",
                               education == "College, CEGEP or other non-university certificate or di..." ~ "non-university",
                               education == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ "Bachelor's degree",
                               education == "University certificate or diploma below the bachelor's level" ~ "less than university",
                               education == "University certificate, diploma or degree above the bach..." ~ "above bachelor"))

# Drop NA
mutate_census<-na.omit(mutate_census)

# Select final column
final_census <- 
  mutate_census %>%
  count(age_group, sex, education, province) %>%
  group_by(age_group, sex, education, province)

# Saving the census data as a csv file in my working directory
write_csv(final_census, "census_data.csv")

