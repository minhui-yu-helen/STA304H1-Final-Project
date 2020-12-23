#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from cesR package.
# Author: Minhui Yu
# Data: December 21st 2020
# Contact:minhui.yu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to uncomment line 17.
# - Don't forget to gitignore it!

# Setup workspace.
library(haven)
library(tidyverse)
setwd("/Users/helen/Documents/Courses/STA/STA304H1/Final Project")

# install cesR package
# devtools::install_github("hodgettsp/cesR")

# load cesR package and labelled package
library(cesR)
library(labelled)

# call 2019 CES online survey
get_ces("ces2019_web")

# convert values to factor type
ces2019_web <- to_factor(ces2019_web)

# Just keep some variables
reduced_survey <- 
  ces2019_web %>% 
  select(cps19_votechoice,
         cps19_citizenship,
         cps19_province,
         cps19_education,
         cps19_gender,
         cps19_yob)

# filter survey data
filtered_survey <- 
  reduced_survey %>% 
  filter(cps19_citizenship != "cps19_citizenship") %>%
  filter(cps19_votechoice != "Don't know/ Prefer not to answer") %>%
  filter(cps19_votechoice != "NA") %>%
  filter(cps19_province != "Yukon") %>%
  filter(cps19_gender != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)") %>%
  filter(cps19_education != "Don't know/ Prefer not to answer")

# Mutate vote_liberal
mutate_survey <- reduced_survey %>% 
  mutate(vote_liberal = ifelse(cps19_votechoice == "Liberal Party", 1, 0))

# Mutate vote_conservative
mutate_survey <- mutate_survey %>% 
  mutate(vote_conservative = ifelse(cps19_votechoice == "Conservative Party", 1, 0))

# Mutate vote_ndp
mutate_survey <- mutate_survey %>% 
  mutate(vote_ndp = ifelse(cps19_votechoice == "ndp", 1, 0))

# Mutate vote_bq
mutate_survey <- mutate_survey %>% 
  mutate(vote_bq = ifelse(cps19_votechoice == "Bloc Québécois", 1, 0))

# Mutate vote_green                    
mutate_survey <- mutate_survey %>% 
  mutate(vote_green = ifelse(cps19_votechoice == "Green Party", 1, 0))

# Mutate vote_people
mutate_survey <- mutate_survey %>% 
  mutate(vote_people = ifelse(cps19_votechoice == "People's Party", 1, 0))

# Mutate province
mutate_survey <- mutate_survey %>% 
  mutate(province = case_when(cps19_province == "Alberta" ~ "Alberta",
                              cps19_province == "British Columbia" ~ "British Columbia",
                              cps19_province == "Manitoba" ~ "Manitoba",
                              cps19_province == "New Brunswick" ~ "New Brunswick",
                              cps19_province == "Newfoundland and Labrador" ~ "Newfoundland and Labrador",
                              cps19_province == "Nova Scotia" ~ "Nova Scotia",
                              cps19_province == "Ontario" ~ "Ontario",
                              cps19_province == "Prince Edward Island" ~ "Prince Edward Island",
                              cps19_province == "Quebec" ~ "Quebec",
                              cps19_province == "Saskatchewan" ~ "Saskatchewan"))

# Mutate sex
mutate_survey<- mutate_survey %>%
  mutate(sex = case_when(cps19_gender == "A man" ~ "Male",
                         cps19_gender == "A woman" ~ "Female"))

# Mutate education
mutate_survey<- mutate_survey %>%
  mutate(education = case_when(cps19_education == "No schooling" ~ "less than high school",
                               cps19_education == "Some secondary/ high school" ~ "less than high school",
                               cps19_education == "Completed secondary/ high school" ~ "high school",
                               cps19_education == "Some technical, community college, CEGEP, College Classique" ~ "less than non-university",
                               cps19_education == "Completed technical, community college, CEGEP, College Classique" ~ "non-university",
                               cps19_education == "Some university" ~ "less than university",
                               cps19_education == "Bachelor's degree" ~ "Bachelor's degree",
                               cps19_education == "Master's degree" ~ "above bachelor",
                               cps19_education == "Professional degree or doctorate" ~ "above bachelor"))

# Mutate age_group
mutate_survey<- mutate_survey %>%
  mutate(age_group = case_when(cps19_yob ==  "1920" ~ "80 & Above 80",
                               cps19_yob ==  "1921" ~ "80 & Above 80",
                               cps19_yob ==  "1922" ~ "80 & Above 80",
                               cps19_yob ==  "1923" ~ "80 & Above 80",
                               cps19_yob ==  "1924" ~ "80 & Above 80",
                               cps19_yob ==  "1925" ~ "80 & Above 80",
                               cps19_yob ==  "1926" ~ "80 & Above 80",
                               cps19_yob ==  "1927" ~ "80 & Above 80",
                               cps19_yob ==  "1928" ~ "80 & Above 80",
                               cps19_yob ==  "1929" ~ "80 & Above 80",
                               cps19_yob ==  "1930" ~ "80 & Above 80",
                               cps19_yob ==  "1931" ~ "80 & Above 80",
                               cps19_yob ==  "1932" ~ "80 & Above 80",
                               cps19_yob ==  "1933" ~ "80 & Above 80",
                               cps19_yob ==  "1934" ~ "80 & Above 80",
                               cps19_yob ==  "1935" ~ "80 & Above 80",
                               cps19_yob ==  "1936" ~ "80 & Above 80",
                               cps19_yob ==  "1937" ~ "80 & Above 80",
                               cps19_yob ==  "1938" ~ "80 & Above 80",
                               cps19_yob ==  "1939" ~ "80 & Above 80",
                               cps19_yob ==  "1940" ~ "60 to 79",
                               cps19_yob ==  "1941" ~ "60 to 79",
                               cps19_yob ==  "1942" ~ "60 to 79",
                               cps19_yob ==  "1943" ~ "60 to 79",
                               cps19_yob ==  "1944" ~ "60 to 79",
                               cps19_yob ==  "1945" ~ "60 to 79",
                               cps19_yob ==  "1946" ~ "60 to 79",
                               cps19_yob ==  "1947" ~ "60 to 79",
                               cps19_yob ==  "1948" ~ "60 to 79",
                               cps19_yob ==  "1949" ~ "60 to 79",
                               cps19_yob ==  "1950" ~ "60 to 79",
                               cps19_yob ==  "1951" ~ "60 to 79",
                               cps19_yob ==  "1952" ~ "60 to 79",
                               cps19_yob ==  "1953" ~ "60 to 79",
                               cps19_yob ==  "1954" ~ "60 to 79",
                               cps19_yob ==  "1955" ~ "60 to 79",
                               cps19_yob ==  "1956" ~ "60 to 79",
                               cps19_yob ==  "1957" ~ "60 to 79",
                               cps19_yob ==  "1958" ~ "60 to 79",
                               cps19_yob ==  "1959" ~ "60 to 79",
                               cps19_yob ==  "1960" ~ "40 to 59",
                               cps19_yob ==  "1961" ~ "40 to 59",
                               cps19_yob ==  "1962" ~ "40 to 59",
                               cps19_yob ==  "1963" ~ "40 to 59",
                               cps19_yob ==  "1964" ~ "40 to 59",
                               cps19_yob ==  "1965" ~ "40 to 59",
                               cps19_yob ==  "1966" ~ "40 to 59",
                               cps19_yob ==  "1967" ~ "40 to 59",
                               cps19_yob ==  "1968" ~ "40 to 59",
                               cps19_yob ==  "1969" ~ "40 to 59",
                               cps19_yob ==  "1970" ~ "40 to 59",
                               cps19_yob ==  "1971" ~ "40 to 59",
                               cps19_yob ==  "1972" ~ "40 to 59",
                               cps19_yob ==  "1973" ~ "40 to 59",
                               cps19_yob ==  "1974" ~ "40 to 59",
                               cps19_yob ==  "1975" ~ "40 to 59",
                               cps19_yob ==  "1976" ~ "40 to 59",
                               cps19_yob ==  "1977" ~ "40 to 59",
                               cps19_yob ==  "1978" ~ "40 to 59",
                               cps19_yob ==  "1979" ~ "40 to 59",
                               cps19_yob ==  "1980" ~ "20 to 39",
                               cps19_yob ==  "1981" ~ "20 to 39",
                               cps19_yob ==  "1982" ~ "20 to 39",
                               cps19_yob ==  "1983" ~ "20 to 39",
                               cps19_yob ==  "1984" ~ "20 to 39",
                               cps19_yob ==  "1985" ~ "20 to 39",
                               cps19_yob ==  "1986" ~ "20 to 39",
                               cps19_yob ==  "1987" ~ "20 to 39",
                               cps19_yob ==  "1988" ~ "20 to 39",
                               cps19_yob ==  "1989" ~ "20 to 39",
                               cps19_yob ==  "1990" ~ "20 to 39",
                               cps19_yob ==  "1991" ~ "20 to 39",
                               cps19_yob ==  "1992" ~ "20 to 39",
                               cps19_yob ==  "1993" ~ "20 to 39",
                               cps19_yob ==  "1994" ~ "20 to 39",
                               cps19_yob ==  "1995" ~ "20 to 39",
                               cps19_yob ==  "1996" ~ "20 to 39",
                               cps19_yob ==  "1997" ~ "20 to 39",
                               cps19_yob ==  "1998" ~ "20 to 39",
                               cps19_yob ==  "1999" ~ "20 to 39",
                               cps19_yob ==  "2000" ~ "Under 20",
                               cps19_yob ==  "2001" ~ "Under 20"))

# Mutate votechoice
mutate_survey<- mutate_survey %>%
  mutate(votechoice = case_when(cps19_votechoice == "Liberal Party" ~ "Liberal Party",
                                cps19_votechoice == "Conservative Party" ~ "Conservative Party",
                                cps19_votechoice == "ndp" ~ "ndp",
                                cps19_votechoice == "Bloc Québécois" ~ "Bloc Québécois",
                                cps19_votechoice == "Green Party" ~ "Green Party",
                                cps19_votechoice == "People's Party" ~ "People's Party"))

# Drop NA
mutate_survey<-na.omit(mutate_survey)

# Select final column
final_survey <- mutate_survey %>% 
  select(votechoice, vote_liberal, vote_conservative, vote_ndp, vote_bq, vote_green, vote_people, sex, age_group, education, province)

# Saving the survey data as a csv file in my working directory
write_csv(final_survey, "survey_data.csv")

