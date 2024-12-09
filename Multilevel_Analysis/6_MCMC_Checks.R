#MCMC Model checks ============================================================

#Run set-up script
source("1_Set_Up.R")

#Load lme4 for frequentist multilevel modelling
require(lme4)

#Set estimation parameters ===================================================

#Set number of iterations, burn-in and chains
n_iter <- 15000
n_burn <- 7500
n_chains <- 5

#Set alpha level
alpha_lvl <- 0.05



#Stranger trust checks =========================================================

#Store number of parameters
n_params <- 18
n_random <- 4

#Set prior mean based Stack & Kposowa (2008) and Boyd and Chung (2012)
int <- (2.648 +  2.56)/2

#Set priors means (centred at 0)
prior_mu <- c(int, rep(0, n_params - 1))

#Variances
prior_var <- diag(100, nrow = n_params, ncol = n_params)


#Prior country variance in suicide approval after accounting for predictors:
#taken from Stack and Kposowa (2008) and Boyd and Chung (2012)
sigma2 <- (0.296 + 0.51)/2

#Setting prior variance in country slopes to 0.05. Rough guess based on variance in
#slopes from Boyd and Chung (2012)
u_var <- diag(c(sigma2, rep(0.05, n_random - 1)))

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_random)),
                  R = list(V = 1, nu = 0.002),
                  B = list(mu = prior_mu, V = prior_var))



#Run five chains
set.seed(2016)
mcmc_stranger <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + Stranger_Trust  +
                               Self_Val + 
                               Attendance + 
                               Religious +
                               Satisfied + Control + 
                               Divorced_Separated + Widowed + Single + 
                               Children +
                               Female + Age + 
                               Below_Primary + Below_Secondary + University,
                             random = ~ us(1 + Family_Trust + Particular_Trust + Stranger_Trust):Country,
                             pr = TRUE, #store random effects
                             data = data,
                             prior = prior_dta,
                             nitt = n_iter, burnin = n_burn, thin = 1,
                             chains = n_chains)


#Get names of desired parameters
params <- names(mcmc_stranger$estimates[,2:19])
resid_terms <- names(mcmc_stranger$residuals)[-1]

#View results
mcmc_summary(mcmc_stranger, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_stranger, element = "residuals", alpha = alpha_lvl, params = resid_terms)

#Outgroup trust checks ====================================================================

#Same prior set-up as above

#Run five chains
set.seed(2016)
mcmc_outgroup <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + Outgroup_Trust  +
                               Self_Val + 
                               Attendance + 
                               Religious +
                               Satisfied + Control + 
                               Divorced_Separated + Widowed + Single + 
                               Children +
                               Female + Age + 
                               Below_Primary + Below_Secondary + University,
                             random = ~ us(1 + Family_Trust + Particular_Trust + Outgroup_Trust):Country,
                             pr = TRUE, #store random effects
                             data = data,
                             prior = prior_dta,
                             nitt = n_iter, burnin = n_burn, thin = 1,
                             chains = n_chains)


#Get names of desired parameters
params <- names(mcmc_outgroup$estimates[,2:19])
resid_terms <- names(mcmc_outgroup$residuals)[-1]

#View results
mcmc_summary(mcmc_outgroup, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_outgroup, element = "residuals", alpha = alpha_lvl, params = resid_terms)


#Self-expression value checks ==============================================================

#Store number of parameters
n_params <- 17
n_random <- 4

#Set priors means (centred at 0)
prior_mu <- c(int, rep(0, n_params - 1))

#Variances
prior_var <- diag(100, nrow = n_params, ncol = n_params)


#Prior country variance in suicide approval after accounting for predictors:
#taken from Stack and Kposowa (2008) and Boyd and Chung (2012)
sigma2 <- (0.296 + 0.51)/2

#Setting prior variance in country slopes to 0.05. Rough guess based on variance in
#slopes from Boyd and Chung (2012)
u_var <- diag(c(sigma2, rep(0.05, n_random - 1)))

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_random)),
                  R = list(V = 1, nu = 0.002),
                  B = list(mu = prior_mu, V = prior_var))


#Run five chains
set.seed(2016)
mcmc_sv <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust +
                         Attendance + 
                         Religious +
                         Satisfied + Control + 
                         Divorced_Separated + Widowed + Single + 
                         Children +
                         Female + Age + 
                         Below_Primary + Below_Secondary + University,
                       random = ~ us(1 + Family_Trust + Particular_Trust + General_Trust):Country,
                       pr = TRUE, #store random effects
                       data = data,
                       prior = prior_dta,
                       nitt = n_iter, burnin = n_burn, thin = 1,
                       chains = n_chains) 


#Get names of desired parameters
params <- names(mcmc_sv$estimates[,2:18])
resid_terms <- names(mcmc_sv$residuals)[-1]

#View results
mcmc_summary(mcmc_sv, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_sv, element = "residuals", alpha = alpha_lvl, params = resid_terms)


#Maximal model ========================================================================

#Store number of parameters
n_params <- 18

#Set priors means (centred at 0)
prior_mu <- c(int, rep(0, n_params - 1))

#Variances
prior_var <- diag(100, nrow = n_params, ncol = n_params)

#Prior country variance in suicide approval after accounting for predictors:
#taken from Stack and Kposowa (2008) and Boyd and Chung (2012)
sigma2 <- (0.296 + 0.51)/2

#Setting prior variance in country slopes to 0.05. Rough guess based on variance in
#slopes from Boyd and Chung (2012)
u_var <- diag(c(sigma2, rep(0.05, n_params - 1)))

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_params)),
                  R = list(V = 1, nu = 0.002),
                  B = list(mu = prior_mu, V = prior_var))



#Run five chains
set.seed(2016)
mcmc_maximal <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust +
                              Self_Val + 
                              Attendance + 
                              Religious +
                              Satisfied + Control +
                              Divorced_Separated + Widowed + Single + 
                              Children +
                              Female + Age + 
                              Below_Primary + Below_Secondary + University,
                            random = ~ us(1 + Family_Trust + Particular_Trust + General_Trust + 
                                            Self_Val + 
                                            Attendance + 
                                            Religious +
                                            Satisfied + Control +
                                            Divorced_Separated + Widowed + Single + 
                                            Children +
                                            Female + Age + 
                                            Below_Primary + Below_Secondary + University):Country,
                            data = data,
                            prior = prior_dta,
                            nitt = n_iter, burnin = n_burn, thin = 1,
                            chains = n_chains)


#Get names of desired parameters
params <- names(mcmc_maximal$estimates[,2:19])
resid_terms <- names(mcmc_maximal$residuals)[-1]

#View results
mcmc_summary(mcmc_maximal, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_maximal, element = "residuals", alpha = 0.01, params = resid_terms)


#Error by country ==============================================================


#Load intercept-only model and final model
mcmc_int_only <- readRDS(("mcmc_int_only.Rdata"))
mcmc_all <- readRDS("mcmc_all.Rdata")

#Get names of desired parameters
params <- names(mcmc_all$estimates[,2:19])

#Store names of countries
countries <- unique(data$Country)

#Error for model with all predictors
all_e <- map(countries, 
             function(x) get_error(mcmc_all, data, 
                                   outcome = "Suicide",
                                   cluster_var = "Country", cluster = x, 
                                   params = params))

#Error for empty model (no predictors, just varying intercept)
empty_e <- map(countries, 
               function(x) get_error(mcmc_int_only, data, 
                                     outcome = "Suicide",
                                     cluster_var = "Country", cluster = x, 
                                     params = "(Intercept)"))

#Calculate proportionate reduction in error variance
prop_e <- map2(.x = all_e, .y = empty_e,
               function(x, y) (y - x)/y) %>%
  set_names(countries)

#Get posterior statistics
error_stats <- post_sum(prop_e)

#Calculate quantiles for error variance
cut_points <- quantile(error_stats$mean, probs = c(0, 0.25, 0.5, 0.75, 1)) %>% as.numeric()

#Store cut point names
cut_names <- c("Low", "Med_Low", "Med_High", "High")

#Use cut points to classify countries in terms of low, medium-low, medium-high and high fit
error_stats$fit <- cut(error_stats$mean, 
                       breaks = cut_points, 
                       labels = cut_names,
                       include.lowest = TRUE)

#Inspect results
map(cut_names, function(x) filter(error_stats, fit == x)) %>% 
  set_names(cut_names)


#Variance Inflation Factors ====================================================

#Refit Model 3 under frequentist framework
freq_mod <- lmer(Suicide ~ Family_Trust + Particular_Trust + General_Trust +
                   Self_Val + 
                   Attendance + 
                   Religious +
                   Satisfied + Control + 
                   Divorced_Separated + Widowed + Single + 
                   Children +
                   Female + Age + 
                   Below_Primary + Below_Secondary + University +
                   (1 + Family_Trust + Particular_Trust + General_Trust|Country),
                 REML = FALSE,
                 lmerControl(optimizer = "bobyqa"),
                 data = data)

#Inspect results
summary(freq_mod)

#Calculate VIFs
car::vif(freq_mod)

