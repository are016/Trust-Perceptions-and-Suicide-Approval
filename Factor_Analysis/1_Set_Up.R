#Set up ========================================================================

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
               Age, Sex, EduCAT,
               Country)

#get trust variable names
trust_vars <- grep("Trust", names(data), value = TRUE) 

#inspect trust variables
map(trust_vars, function(x) table(data[[x]])) %>% set_names(trust_vars)

#recode trust variables
data <- data %>%
  mutate_at(.vars = trust_vars,
            function(x) car::recode(x, "1=4;2=3;3=2;4=1"))



#Recode sex
table(data$Sex)
data$Sex <- factor(data$Sex, levels = 1:2, labels = c("Male", "Female"))


#Recode education
table(data$EduCAT)
data$EduCAT <- factor(data$EduCAT)


#Remove missing values for trust variables
data <- data[rownames(na.omit(data[trust_vars])),]