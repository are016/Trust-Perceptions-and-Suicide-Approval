#Missing checks

#Load packages
require(tidyverse)
require(psych)
require(GPArotation)
require(here)


#Load functions
source(here("Functions", "FA_Functions.R"))
source(here("Functions", "General_Functions.R"))


#Read in data
data <- read.csv(here("Data", "WVS_6_7_280723.csv"))

#Limit variables to trust items and demographics
data <- select(data, 
               Family_Trust, Neighbour_Trust, Personal_Trust,
               Stranger_Trust, Religion_Trust, National_Trust,
               Country)

#get trust variable names
trust_vars <- grep("Trust", names(data), value = TRUE) 

#inspect trust variables
map(trust_vars, function(x) table(data[[x]])) %>% set_names(trust_vars)

#recode trust variables
data <- data %>%
  mutate_at(.vars = trust_vars,
            function(x) car::recode(x, "1=4;2=3;3=2;4=1"))


#Check missing observations for entire sample =================================
nrow(na.omit(data)) #Count observations after removing missing cases
get_missing(data) #Get proportions of missing and complte cases


#Check missing observations per country =======================================

#Get country names
countries <- unique(data$Country)

#Split data up by country
country_data <- map(countries, function(x) data[data$Country == x,])

#Assign neat names
names(country_data) <- countries

#Get missingness by country
country_missing <- map(country_data, function(x) get_missing(x))

#Convert to data frame
country_missing <- country_missing %>%
  plyr::ldply(rbind)

#For countries with no missing observations, replace NA with 0
country_missing$Yes <- ifelse(is.na(country_missing$Yes), 
                              0,
                              country_missing$Yes)

#Calculate average missigness across countries
mean(country_missing$Yes)

#Further checks for countries with high missingness ===========================

#Limit to countries with more than 25% missing observations
high_missing <- country_missing[country_missing$Yes > 25, ]

#Extract country names
high_missing <- high_missing[[".id"]]

#Inspect countries
country_missing %>%
  filter(.id %in% high_missing)

#Get summaries for missing countries
map(high_missing, function(x) summary(country_data[[x]])) %>%
  set_names(high_missing) 

#Check missingness by item 
map(high_missing, 
    function(x) map(trust_vars, 
                    function(y) get_missing(country_data[[x]][y]))) %>%
  set_names(high_missing) %>%
  map(function(x) set_names(x, trust_vars))


