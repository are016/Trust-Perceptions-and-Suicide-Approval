#Centred analyses =============================================================

require(GPArotation)
require(here)


#Load functions
source(here("Functions", "FA_Functions.R"))
source(here("Functions", "General_Functions.R"))
source(here("Functions", "Recode_Functions.R"))

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


#Apply group-mean centring
data <- data %>% 
  group_by(Country) %>%
  mutate_at(.vars = trust_vars,
            function(x) centre(x)) %>%
  ungroup()



#EFA Models ========================================================

#Model 1 =======================================================

#Conduct factor analyses for 1 factor
fa_1 <- fa(data[trust_vars], nfactors = 1, rotate = "none", fm = "ml")

#Inspect results
fa_table(fa_1)

#Model 2 =======================================================

#Conduct factor analyses for 2 factors
fa_2 <- fa(data[trust_vars], nfactors = 2, rotate = "oblimin", fm = "ml")

#Inspect results
fa_table(fa_2)

#Inspect structure matrix
structure_mx(fa_2)

#Model 3 =======================================================

#Get names of trust variables excluding family trust
not_family <- trust_vars[!grepl("Family", trust_vars)]

#Conduct factor analyses for 2 factors
fa_3 <- fa(data[not_family], nfactors = 2, rotate = "oblimin", fm = "ml")

#Inspect results
fa_table(fa_3)

#Inspect structure matrix
structure_mx(fa_3)

#Model 4 =======================================================

#Conduct factor analyses for 2 factors
fa_4 <- fa(data[trust_vars], nfactors = 3, rotate = "oblimin", fm = "ml")

#Inspect results
fa_table(fa_4)

#Inspect structure matrix
structure_mx(fa_4)

  