#Discrepant cases ============================================================

#Load Packages
require(MCMCglmm)
require(tidyverse)
require(here)

#Source functions
source(here("Functions", "MCMC_Functions.R"))
source(here("Functions", "Recode_Functions.R"))
source(here("Functions", "General_Functions.R"))
source(here("Functions", "Predict_Functions.R"))

#Read in data
data <- read.csv(here("data", "WVS_6_7_280723.csv"))

#Inspect data
names(data)
summary(data)


#Get trust variable names
trust_vars <- grep("Trust", names(data), value = TRUE) 

#Inspect trust variables
map(trust_vars, function(x) table(data[[x]])) %>% set_names(trust_vars)

#Recode trust variables
data <- data %>%
  mutate_at(.vars = trust_vars,
            function(x) car::recode(x, "1=4;2=3;3=2;4=1"))


#Construct particular and general trust indexes
data$Particular_Trust <- (data$Neighbour_Trust + data$Personal_Trust)/2
data$General_Trust <- (data$Stranger_Trust + data$Religion_Trust + data$National_Trust)/3
data$Self_Val <- value_index(data, index = "partial")

#Limit data to key variables
data <- select(data, Suicide, 
               Family_Trust, Neighbour_Trust, Personal_Trust,
               Stranger_Trust, Religion_Trust, National_Trust,
               Particular_Trust, General_Trust,
               Self_Val,
               Attendance, Religious, 
               Satisfied, Control,
               Marital, Children,
               Sex, Age, EduCAT, 
               Country, Wave, Year)


#Construct country level data (constructed before excluding NA values for more accuate measures of country characteristics)
country_data <- data %>%
  group_by(Country) %>%
  summarise(Agg_Religious = mean(Religious, na.rm = T),
            Agg_Self_Val = mean(Self_Val, na.rm = T))

#Remove missing values
data <- na.omit(data)

#Limit country-level data to relevant countries
country_data <- country_data %>%
  filter(Country %in% unique(data$Country))


#Load final model
mcmc_all <- readRDS("mcmc_all.Rdata")



#Random effects ===========================================================

#Get names of random effects for each trust variable
family_names <- grep("Family_Trust.Country", names(mcmc_all$estimates), value = TRUE)
particular_names <- grep("Particular_Trust.Country", names(mcmc_all$estimates), value = TRUE)
general_names <- grep("General_Trust.Country", names(mcmc_all$estimates), value = TRUE)

#Add random deviation to average effect to get country-specific effect
family_effects <- mcmc_all$estimates$Family_Trust + mcmc_all$estimates[family_names]
particular_effects <- mcmc_all$estimates$Particular_Trust + mcmc_all$estimates[particular_names]
general_effects <- mcmc_all$estimates$General_Trust + mcmc_all$estimates[general_names]

#Shorten column names to country names
names(family_effects) <- gsub("Family_Trust.Country.", "", names(family_effects))
names(particular_effects) <- gsub("Particular_Trust.Country.", "", names(particular_effects))
names(general_effects) <- gsub("General_Trust.Country.", "", names(general_effects))

#Calculate posterior statistics
family_effects <- post_sum(family_effects)
particular_effects <- post_sum(particular_effects)
general_effects <- post_sum(general_effects)


#Check countries where trust effect deviated from average =====================

#Family trust - effect is reliably positive
family_effects %>%
  filter(`lwr_95%` > 0)


#Particular trust - effect is reliably positive
particular_effects %>%
  filter(`lwr_95%` > 0)


#General trust - effect is reliably negative
general_effects %>%
  filter(`upr_95%` < 0)


#Z-tests ======================================================================

#Store names of discrepant cases
discrepant_cases <- c("Nigeria", #family trust
                      "Algeria", "UK", "Uzbekistan", "Zimbabwe", #particular trust
                      "Ecuador", "Georgia", "Kazakhstan") #general trust


#Run z-tests for aggregate levels of religiousness
map(discrepant_cases, function(x) z_score(country_data, "Agg_Religious", "Country", x)) %>%
  set_names(discrepant_cases) %>%
  map(as.data.frame)

#Run z-tests for aggregate levels of self-expression values
map(discrepant_cases, function(x) z_score(country_data, "Agg_Self_Val", "Country", x)) %>%
  set_names(discrepant_cases)  %>%
  map(as.data.frame)
  

#Additional check for Lebanon as a comparison to Nigeria
z_score(country_data, "Agg_Religious", "Country", "Lebanon") %>% as.data.frame()
z_score(country_data, "Agg_Self_Val", "Country", "Lebanon") %>% as.data.frame()

