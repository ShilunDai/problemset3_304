#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from American Community Surveys(ACS)
# Author: Shilun Dai
# Data: 31 October 2020
# Contact: shilun.dai@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/shaelyndai/Desktop")
raw_data_census <- read_dta("usa_00001.dta")

# Add the labels
raw_data_census <- labelled::to_factor(raw_data_census)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data_census <- 
  raw_data_census %>% 
  select(sex, 
         age,
         race,
         citizen,
         stateicp,
         inctot,
         educd,
         perwt)
reduced_data_census$age <- as.numeric(reduced_data_census$age)    

filtered_data_census <- reduced_data_census %>% 
  filter(age>=18 &
           (citizen == 'naturalized citizen'|citizen == 'born abroad of american parents')&
           educd != 'n/a')

filtered_data_census <- na.omit(filtered_data_census)
  

#filtered_data_census <- 
#  filtered_data_census %>%
#  count(age) %>%
#  group_by(age)   
   

filtered_data_census <- 
  filtered_data_census %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

# add age group
filtered_data_census <- filtered_data_census %>% 
  mutate(agegroup = case_when(age <= 20 ~ '20 or less',
                              age > 20 & age <= 35 ~ '21 to 35', 
                              age > 35 & age <= 50 ~ '36 to 50',
                              age > 50 & age <= 65 ~ '51 to 65',
                              age > 65 & age <= 80 ~ '66 to 80',
                              age >80 ~ 'above 80'))

# map gender
filtered_data_census <- rename(filtered_data_census, gender=sex)
filtered_data_census$gender <- ifelse(filtered_data_census$gender == 'female', 'Female', 'Male')

# map education
#Some asumptions & changes made other than direct string conversion
#1.Other post high school vocational training=High school graduate (survey data)
#2.Completed some graduate, but no degree=College degree
#3.professional degree beyond a bachelor's degree=College degree

less_than_grade_3 <- c("no schooling completed","nursery school, preschool","kindergarten","grade 1","grade 2","grade 3")
grade_4_to_8<-c("grade 4","grade 5","grade 6","grade 7","grade 8")
grade_9_to_11<-c("grade 9","grade 10","grade 11","12th grade, no diploma")
highschool<-c("regular high school diploma", "ged or alternative credential")
colleges<-c("some college, but less than 1 year",
                "1 or more years of college credit, no degree")

filtered_data_census<-filtered_data_census %>% 
  mutate(education = case_when(educd =="associate's degree, type not specified" ~ 'Associate Degree',
                            educd=="doctoral degree"~'Doctorate degree',
                            educd =="master's degree" ~ 'Masters degree',
                            educd=="professional degree beyond a bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd =="bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
                            educd %in% colleges~"Completed some college, but no degree",
                            educd %in% highschool~"High school graduate",
                            educd %in% grade_9_to_11~"Completed some high school",
                            educd %in% grade_4_to_8~"Middle School - Grades 4 - 8",
                            educd %in% less_than_grade_3 ~"3rd Grade or less"))

filtered_data_census <- filtered_data_census %>% select(-educd)


# map state/stateicp
filtered_data_census<-filtered_data_census %>%
  mutate(state = case_when(stateicp=="alabama"~"AL",
                           stateicp=="alaska"~"AK",
                           stateicp=="arizona"~"AZ",
                           stateicp=="arkansas"~"AR",
                           stateicp=="california"~"CA",
                           stateicp=="colorado"~"CO",
                           stateicp=="connecticut"~"CT",
                           stateicp=="delaware"~"DE",
                           stateicp=="florida"~"FL",
                           stateicp=="georgia"~"GA",
                           stateicp=="hawaii"~"HI",
                           stateicp=="idaho"~"ID",
                           stateicp=="illinois"~"IL",
                           stateicp=="indiana"~"IN",
                           stateicp=="iowa"~"IA",
                           stateicp=="kansas"~"KS",
                           stateicp=="kentucky"~"KY",
                           stateicp=="louisiana"~"LA",
                           stateicp=="maine"~"ME",
                           stateicp=="maryland"~"MD",
                           stateicp=="massachusetts"~"MA",
                           stateicp=="michigan"~"MI",
                           stateicp=="minnesota"~"MN",
                           stateicp=="mississippi"~"MS",
                           stateicp=="missouri"~"MO",
                           stateicp=="montana"~"MT",
                           stateicp=="nebraska"~"NE",
                           stateicp=="nevada"~"NV",
                           stateicp=="new hampshire"~"NH",
                           stateicp=="new jersey"~"NJ",
                           stateicp=="new mexico"~"NM",
                           stateicp=="new york"~"NY",
                           stateicp=="north carolina"~"NC",
                           stateicp=="north dakota"~"ND",
                           stateicp=="ohio"~"OH",
                           stateicp=="oklahoma"~"OK",
                           stateicp=="oregon"~"OR",
                           stateicp=="pennsylvania"~"PA",
                           stateicp=="rhode island"~"RI",
                           stateicp=="south carolina"~"SC",
                           stateicp=="south dakota"~"SD",
                           stateicp=="tennessee"~"TN",
                           stateicp=="texas"~"TX",
                           stateicp=="utah"~"UT",
                           stateicp=="vermont"~"VT",
                           stateicp=="virginia"~"VA",
                           stateicp=="washington"~"WA",
                           stateicp=="west virginia"~"WV",
                           stateicp=="wisconsin"~"WI",
                           stateicp=="wyoming"~"WY",
                           stateicp=="district of columbia"~"DC")) 

filtered_data_census <- filtered_data_census %>% select(-stateicp)

# map income total
filtered_data_census<-filtered_data_census %>% 
  mutate(household_income = case_when(inctot<=14999 ~ "Less than $14,999",
                                      inctot>=15000 & inctot<=19999~"$15,000 to $19,999",
                                      inctot>=20000 & inctot<=24999~"$20,000 to $24,999",
                                      inctot>=25000 & inctot<=29999~"$25,000 to $29,999",
                                      inctot>=30000 & inctot<=34999~"$30,000 to $34,999",
                                      inctot>=35000 & inctot<=39999~"$35,000 to $39,999",
                                      inctot>=40000 & inctot<=44999~"$40,000 to $44,999",
                                      inctot>=45000 & inctot<=49999~"$45,000 to $49,999",
                                      inctot>=50000 & inctot<=54999~"$50,000 to $54,999",
                                      inctot>=55000 & inctot<=59999~"$55,000 to $59,999",
                                      inctot>=60000 & inctot<=64999~"$60,000 to $64,999",
                                      inctot>=65000 & inctot<=69999~"$65,000 to $69,999",
                                      inctot>=70000 & inctot<=74999~"$70,000 to $74,999",
                                      inctot>=75000 & inctot<=79999~"$75,000 to $79,999",
                                      inctot>=80000 & inctot<=84999~"$80,000 to $84,999",
                                      inctot>=85000 & inctot<=89999~"$85,000 to $89,999",
                                      inctot>=90000 & inctot<=94999~"$90,000 to $94,999",
                                      inctot>=95000 & inctot<=99999~"$95,000 to $99,999",
                                      inctot>=100000 & inctot<=124999~"$100,000 to $124,999",
                                      inctot>=125000 & inctot<=149999~"$125,000 to $149,999",
                                      inctot>=150000 & inctot<=174999~"$150,000 to $174,999",
                                      inctot>=175000 & inctot<=199999~"$175,000 to $199,999",
                                      inctot>=200000 & inctot<=249999~"$200,000 to $249,999",
                                      inctot>=250000~"$250,000 and above")) 
filtered_data_census <- filtered_data_census %>% select(-inctot)

# map race
filtered_data_census<-filtered_data_census %>% 
  mutate(new_race = case_when(
    race=="white"~"White",
    race=="chinese"~"Chinese",
    race=="black/african american/negro"~"Black, or African American",
    race=="two major races"~"Other race",
    race=="other race, nec"~"Other race",
    race=="japanese"~"other asian or pacific islander",
    race=="american indian or alaska native"~"American Indian or Alaska Native",
    race=="three or more major races"~"Other race",
    race=="other asian or pacific islander"~"other asian or pacific islander")) 

filtered_data_census <- filtered_data_census %>% select(-race)
filtered_data_census <- rename(filtered_data_census, race=new_race)

filtered_data_census$gender <-  as.factor(filtered_data_census$gender)
filtered_data_census$agegroup <-  as.factor(filtered_data_census$agegroup)
filtered_data_census$state <-  as.factor(filtered_data_census$state)
filtered_data_census$household_income <-  as.factor(filtered_data_census$household_income)
filtered_data_census$race <-  as.factor(filtered_data_census$race)
# Saving the census data as a csv file in my working directory
write_csv(filtered_data_census, "./../../outputs/census_data.csv")



         