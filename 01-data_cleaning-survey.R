#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUM
# Author: Shilun Dai
# Data: 31 October 2020
# Contact: shilun.dai@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data_survey <- read_dta("/Users/shaelyndai/Desktop/ns20200625.dta")
# Add the labels
raw_data_survey <- labelled::to_factor(raw_data_survey)

# Just keep some variables
reduced_data_survey <- 
  raw_data_survey %>% 
  select(vote_2020,
         vote_intention,
         registration,
         age,
         gender,
         state,
         race_ethnicity,
         household_income,
         education)

reduced_data_survey$age <- as.numeric(reduced_data_survey$age)

filtered_data_survey <- reduced_data_survey %>% 
  filter(registration=='Registered'&
           (vote_intention == 'Yes, I will vote'|vote_intention == 'Not sure') &
           (vote_2020 == 'Donald Trump'|vote_2020=='Joe Biden'))

filtered_data_survey <- na.omit(filtered_data_survey)

filtered_data_survey <-
  filtered_data_survey %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))

filtered_data_survey <- filtered_data_survey %>% 
  select(-vote_intention, -registration)


#### What else???? ####
# Maybe make some age-groups?
# Maybe check the values?
# Is vote a binary? If not, what are you going to do?
filtered_data_survey <- filtered_data_survey %>% 
  mutate(agegroup = case_when(age <= 20 ~ '20 or less',
                              age > 20 & age <= 35 ~ '21 to 35', 
                              age > 35 & age <= 50 ~ '36 to 50',
                              age > 50 & age <= 65 ~ '51 to 65',
                              age > 65 & age <= 80 ~ '66 to 80',
                              age >80 ~ 'above 80'))

filtered_data_survey$education[filtered_data_survey$education=="Completed some graduate, but no degree"]<-"College Degree (such as B.A., B.S.)"
filtered_data_survey$education[filtered_data_survey$education=="Other post high school vocational training"]<-"High school graduate"

# map race
otherasian<-c("Asian (Asian Indian)","Asian (Vietnamese)","Asian (Other)",
              "Asian (Korean)","Asian (Filipino)", "Asian (Japanese)",
              "Pacific Islander (Native Hawaiian)","Pacific Islander (Other)",
              "Pacific Islander (Samoan)","Pacific Islander (Guamanian)")

filtered_data_survey<-filtered_data_survey %>% 
  mutate(race = case_when(
    race_ethnicity == "White"~"White",
    race_ethnicity == "Black, or African American"~"Black, or African American",
    race_ethnicity =="Asian (Chinese)" ~ 'Chinese',
    race_ethnicity %in% otherasian ~"other asian or pacific islander",
    race_ethnicity =="Some other race" ~ 'Other race',
    race_ethnicity=="American Indian or Alaska Native"~"American Indian or Alaska Native",
    race_ethnicity=="Other race "~"Other race")) 
filtered_data_survey <-  filtered_data_survey %>% select(-race_ethnicity)

filtered_data_survey$gender <-  as.factor(filtered_data_survey$gender)
filtered_data_survey$agegroup <-  as.factor(filtered_data_survey$agegroup)
filtered_data_survey$state <-  as.factor(filtered_data_survey$state)
filtered_data_survey$household_income <-  as.factor(filtered_data_survey$household_income)
filtered_data_survey$race <-  as.factor(filtered_data_survey$race)
filtered_data_survey$vote_2020 <- as.factor(filtered_data_survey$vote_2020)
# Saving the survey/sample data as a csv file in my
# working directory
write_csv(filtered_data_survey,'survey_data.csv')

