#Data Preparation ============================================================

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


#Remove missing cases
data <- na.omit(data)


##Mean-centre trust variables
data$Family_Trust <- centre(data$Family_Trust)
data$Stranger_Trust <- centre(data$Stranger_Trust)
data$Particular_Trust <- centre(data$Particular_Trust)
data$General_Trust <- centre(data$General_Trust)
data$Outgroup_Trust <- centre(data$Outgroup_Trust)

#Mean-centre controls
data$Satisfied <- centre(data$Satisfied)
data$Control <- centre(data$Control)
data$Age <- centre(data$Age)
data$Self_Val <- centre(data$Self_Val)

#Median-centre controls
data$Children <- data$Children - median(data$Children)
data$Attendance <- data$Attendance - median(data$Attendance)

#Confirm changes have been applied correctly
summary(data)


#Dummy out nominal variables
data <- dummy_out("Marital", data, rm_original = FALSE)
data <- dummy_out("Sex", data, rm_original = FALSE)
data <- dummy_out("EduCAT", data, rm_original = FALSE)

