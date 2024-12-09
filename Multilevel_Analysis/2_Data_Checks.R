#Data Preparation ============================================================

#Load Packages
require(tidyverse)
require(here)

#Source functions
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
data$Outgroup_Trust <- (data$Religion_Trust + data$National_Trust)/2


#Construct self-expression value index
data$Self_Val <- value_index(data, index = "partial")


#Recode attendance
table(data$Attendance)
data$Attendance <- car::recode(data$Attendance, "1=8;2=7;3=6;4=5;5=4;6=3;7=2;8=1")


#Recode sex
table(data$Sex)
data$Sex <- factor(data$Sex, levels = 1:2, labels = c("Male", "Female"))

#Recode marriage
table(data$Marital)
data$Marital <- factor(data$Marital, levels = 1:6, labels = c("Married_Coupled", "Married_Coupled",
                                                              "Divorced_Separated", "Divorced_Separated",
                                                              "Widowed", "Single"))

#Recode education
table(data$EduCAT)
data$EduCAT <- factor(data$EduCAT)


#Limit data to key variables
data <- select(data, Suicide, 
               Family_Trust, Neighbour_Trust, Personal_Trust,
               Stranger_Trust, Religion_Trust, National_Trust,
               Particular_Trust, General_Trust, Outgroup_Trust,
               Self_Val,
               Attendance, Religious, 
               Satisfied, Control,
               Marital, Children,
               Sex, Age, EduCAT, 
               Country, Wave, Year)


#Missing tests =================================================================

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

#Compare observations with complete and missing information
compare_missing(data = data, variable = "Suicide")
compare_missing(data = data, variable = "Age")
compare_missing(data = data, variable = "Sex")
compare_missing(data = data, variable = "EduCAT")

#Remove missing cases
data <- na.omit(data)


#Descriptive statistics and visualisations ====================================

#Get basic summary statistics
summary(data)

#Store names of categorical, ordinal and continuous variables
categorical <- get_class(data, of_class = c("factor", "character"))
continuous_ordinal <- get_class(data, of_class = c("numeric", "integer"))

#Add family trust and church attendance to categorical names
categorical <- c("Family_Trust", "Attendance", categorical)

#Compute modes, standard deviations and IQRs
map(categorical, function(x) get_mode(data[[x]])) %>% set_names(categorical)
map(continuous_ordinal, function(x) sd(data[[x]], na.rm = T)) %>% set_names(continuous_ordinal)
map(continuous_ordinal, function(x) IQR(data[[x]], na.rm = T)) %>% set_names(continuous_ordinal)


#Plot distribution of suicide approval
ggplot(data, aes(Suicide)) +
  geom_histogram(binwidth = 1) +
  labs(x = "Suicide Justififable; 1= Never, 10 = Always", y = "Frequency",
       caption = paste("n =", nrow(data), "\nSource: WVS, Waves 6 and 7")) +
  scale_x_continuous(breaks = 1:10) +
  theme(text = element_text(size = 20))

