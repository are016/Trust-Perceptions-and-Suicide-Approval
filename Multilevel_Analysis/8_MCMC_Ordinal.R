#Data Preparation ============================================================

#Load Packages
require(MCMCglmm)
require(tidyverse)
require(here)

#Source functions
source(here("Functions", "MCMC_Functions.R"))
source(here("Functions", "Recode_Functions.R"))
source(here("Functions", "Predict_Functions.R"))
source(here("Functions", "Probit_Functions.R"))

#Read in data
data <- read.csv(here("Data", "WVS_6_7_280723.csv"))

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


#Percentage/number of complete cases
nrow(na.omit(data))/nrow(data) # 0.7623242
nrow(na.omit(data)) # 185459

#Remove missing cases
data <- na.omit(data)

#Recode suicide approval into four categories 
data$Suicide_Cat <- car::recode(data$Suicide, 
                                "1=1;2=2;3=2;4=2;5=3;6=3;7=3;8=4;9=4;10=4")

#Confirm changes have been applied correctly
table(data$Suicide_Cat)
table(data$Suicide, data$Suicide_Cat)


#Standardize all variables to improve convergence
data <- data %>%
  mutate_at(.vars = c("Family_Trust", "Particular_Trust", "General_Trust", 
                      "Satisfied", "Control", "Age", "Self_Val",
                      "Children", "Attendance"),
            .funs = function(x) standardize(x))

#Confirm changes have been applied correctly
summary(data)


#Dummy out nominal variables
data <- dummy_out("Marital", data, rm_original = TRUE)
data <- dummy_out("Sex", data, rm_original = TRUE)
data <- dummy_out("EduCAT", data, rm_original = TRUE)


#Set estimation parameters ===================================================

#Set number of iterations, burn-in and chains
n_iter <- 40000
n_burn <- 30000
n_chains <- 5

#Set alpha level
alpha_lvl <- 0.05

#Ordinal Model =================================================================

#Store number of parameters
n_params <- 18

#Set priors
#Means (centred at 0)
prior_mu <- rep(0, n_params)

#Variances
prior_var <- diag(100, nrow = n_params, ncol = n_params)

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = 1, nu = 0.002)),
                  R = list(V = 1, fix = 1),
                  B = list(mu = prior_mu, V = prior_var))



#Run five chains
set.seed(2016)
mcmc_ord <- mcmc_chains(Suicide_Cat ~ Family_Trust + Particular_Trust + General_Trust + 
                          Self_Val + 
                          Attendance + 
                          Religious +
                          Satisfied + Control + 
                          Divorced_Separated + Widowed + Single + 
                          Children +
                          Female + Age + 
                          Below_Primary + Below_Secondary + University,
                        random = ~ Country,
                        pr = TRUE, #store random effects
                        data = data,
                        family = "ordinal",
                        prior = prior_dta,
                        nitt = n_iter, burnin = n_burn, thin = 10,
                        chains = n_chains)


#Get names of desired parameters
params <- names(mcmc_ord$estimates[,2:19])
cut_names <- names(mcmc_ord$cuts)[-1]

#View results
mcmc_summary(mcmc_ord, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_ord, element = "cuts", alpha = alpha_lvl, params = cut_names)

#Examine trace plots of coefficients
trace_plot_list <- map(params, function(x) mcmc_trace(mcmc_ord, param = x)) %>% 
  set_names(params)

trace_plot_list

#Examine trace plots of cut points
trace_plot_list <- map(cut_names, function(x) mcmc_trace(mcmc_ord, element = "cuts", param = x)) 

trace_plot_list


#Save ordinal model
saveRDS(mcmc.ord, file = "mcmc_ord_4.Rdata")


#Rescale model for 0 variance in residuals - confirmed that this produces identical output to
#MASS polr with probit link in the case of single level models
rescaled_mod <- scale_posterior(mcmc_ord)

#View results
mcmc_summary(rescaled_mod, alpha = alpha_lvl, params = params)

#Calculate cut points so that they are comparable to frequentist results
rescaled_mod$scaled_cuts <- get_cuts(rescaled_mod)

#Inspect cut points
mcmc_summary(rescaled_mod, element = "scaled_cuts", alpha = 0.01, params = names(rescaled_mod$scaled_cuts))
