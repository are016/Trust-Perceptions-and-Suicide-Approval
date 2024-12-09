#Heteroscedastic checks  ================================================================

#Run set-up script
source("1_Set_Up.R")

#Set estimation parameters ===================================================

#Set number of iterations, burn-in and chains
n_iter <- 15000
n_burn <- 7500
n_chains <- 5

#Set alpha level
alpha_lvl <- 0.05

#Religious =============================================================================

#Create factor version of religiousness variable
data$Religious_Cat <- factor(data$Religious)

#Store number of parameters
n_params <- 18
n_random <- 4

#Set priors
#Set prior mean based Stack & Kposowa (2008) and Boyd and Chung (2012)
int <- (2.648 +  2.56)/2

#Means (centred at 0)
prior_mu <- c(int, rep(0, n_params - 1))

#Variances
prior_var <- diag(100, nrow = n_params, ncol = n_params)


#Prior country variance in suicide approval after accounting for predictors:
#taken from Stack and Kposowa (2008) and Boyd and Chung (2012)
sigma2 <- (0.296 + 0.51)/2

#Residual covariance matrix for heteroscedasticity
n_religious <- length(unique(data$Religious_Cat)) #get number of categories
resid_var <- diag(rep(sigma2, n_religious)) #set up piror covaraince matrix
resid_nu <- n_religious #nu >= dimensions for proper prior distribution

#Setting prior variance in country slopes to 0.05. Rough guess based on variance in
#slopes from Boyd and Chung (2012)
u_var <- diag(c(sigma2, rep(0.05, n_random - 1)))


#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_random)),
                  R = list(V = resid_var, nu = resid_nu),
                  B = list(mu = prior_mu, V = prior_var))

#Level 1 Heteroscedastic Models

#Run five chains
set.seed(2016)
mcmc.het <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust + 
                          Self_Val + 
                          Attendance + 
                          Religious +
                          Satisfied + Control +
                          Divorced_Separated + Widowed + Single + 
                          Children +
                          Female + Age + 
                          Below_Primary + Below_Secondary + University,
                        random = ~ us(1 + Family_Trust + Particular_Trust + General_Trust):Country,
                        rcov = ~ idh(Religious_Cat):units,
                        pr = TRUE, #store random effects
                        data = data,
                        prior = prior_dta,
                        nitt = n_iter, burnin = n_burn, thin = 1,
                        chains = n_chains)


#Get names of desired parameters
params <- names(mcmc.het$estimates[,2:19])
resid_terms <- names(mcmc.het$residuals[-1])

#View results
mcmc_summary(mcmc.het, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc.het, element = "residuals", alpha = alpha_lvl, params = resid_terms)


#Education ===================================================================

#Residual covariance matrix for heteroscedasticity
n_edu <- length(unique(data$EduCAT)) #get number of categories
resid_var <- diag(rep(sigma2, n_edu)) #set up prior covaraince matrix
resid_nu <- n_edu #nu >= dimensions for proper prior distribution

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_random)),
                  R = list(V = resid_var, nu = resid_nu),
                  B = list(mu = prior_mu, V = prior_var))


#Level 1 Heteroscedastic Models

#Run five chains
set.seed(2016)
mcmc.het <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust + 
                          Self_Val + 
                          Attendance + 
                          Religious +
                          Satisfied + Control +
                          Divorced_Separated + Widowed + Single + 
                          Children +
                          Female + Age + 
                          Below_Primary + Below_Secondary + University,
                        random = ~ us(1 + Family_Trust + Particular_Trust + General_Trust):Country,
                        rcov = ~ idh(EduCAT):units,
                        pr = TRUE, #store random effects
                        data = data,
                        prior = prior_dta,
                        nitt = n_iter, burnin = n_burn, thin = 1,
                        chains = n_chains)

#Get names of desired parameters
params <- names(mcmc.het$estimates[,2:19])
resid_terms <- names(mcmc.het$residuals[-1])

#View results
mcmc_summary(mcmc.het, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc.het, element = "residuals", alpha = alpha_lvl, params = resid_terms)



#Marital ============================================================================

#Residual covariance matrix for heteroscedasticity
n_marital <- length(unique(data$Marital)) #get number of categories
resid_var <- diag(rep(sigma2, n_marital)) #set up prior covaraince matrix
resid_nu <- n_marital #nu >= dimensions for proper prior distribution


#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_random)),
                  R = list(V = resid_var, nu = resid_nu),
                  B = list(mu = prior_mu, V = prior_var))


#Level 1 Heteroscedastic Models

#Run five chains
set.seed(2016)
mcmc.het <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust + 
                          Self_Val + 
                          Attendance + 
                          Religious +
                          Satisfied + Control +
                          Divorced_Separated + Widowed + Single + 
                          Children +
                          Female + Age + 
                          Below_Primary + Below_Secondary + University,
                        random = ~ us(1 + Family_Trust + Particular_Trust + General_Trust):Country,
                        rcov = ~ idh(Marital):units,
                        pr = TRUE, #store random effects
                        data = data,
                        prior = prior_dta,
                        nitt = n_iter, burnin = n_burn, thin = 1,
                        chains = n_chains)

#Get names of desired parameters
params <- names(mcmc.het$estimates[,2:19])
resid_terms <- names(mcmc.het$residuals[-1])

#View results
mcmc_summary(mcmc.het, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc.het, element = "residuals", alpha = alpha_lvl, params = resid_terms)
