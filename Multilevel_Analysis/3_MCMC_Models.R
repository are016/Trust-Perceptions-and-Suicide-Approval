#mcmc models =================================================================

#Run set-up script
source("1_Set_Up.R")


#Set estimation parameters ===================================================

#Set number of iterations, burn-in and chains
n_iter <- 15000
n_burn <- 7500
n_chains <- 5

#Set alpha level
alpha_lvl <- 0.05

#intercept-only ==============================================================

#Store number of parameters
n_params <- 1

#Set prior mean based Stack & Kposowa (2008) and Boyd and Chung (2012)
int <- (2.648 +  2.56)/2

prior_mu <- int

#Variances
prior_var <- diag(100, nrow = n_params, ncol = n_params)

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = 1, nu = 0.002)),
                  R = list(V = 1, nu = 0.002),
                  B = list(mu = prior_mu, V = prior_var))


#Run three chains
set.seed(2016)
mcmc_int_only <- mcmc_chains(Suicide ~ 1,
                             random = ~ Country,
                             pr = TRUE, #store random effects
                             data = data,
                             prior = prior_dta,
                             nitt = n_iter, burnin = n_burn, thin = 1,
                             chains = n_chains)

#Inspect results
mcmc_summary(mcmc_int_only, params = "(Intercept)", alpha = alpha_lvl)

#Get names of residual terms
resid_terms <- names(mcmc_int_only$residuals)[-1]

#Inspect residual terms
mcmc_summary(mcmc_int_only, element = "residuals", alpha = alpha_lvl, params = resid_terms)

#Examine trace plots
mcmc_trace(mcmc_int_only, param = "(Intercept)")

#Save results
saveRDS(mcmc_int_only, file = "mcmc_int_only.Rdata")

#Get posterior information on variance partition coefficients (VPCs)
vpc_results <- get_vpc(mcmc_int_only, alpha = alpha_lvl)

#Inspect summary stats
vpc_results$posterior_estimates

#Extract names of level 2 residual terms (u02)
u0_terms <- grep("Country", names(mcmc_int_only$estimates), value = TRUE)

#Get posterior summary of u0
u0 <- mcmc_summary(mcmc_int_only, alpha = 0.01, params = u0_terms)

#Exclude grand intercept and deviance from u0
u0 <- u0$post_stats[u0_terms,]

#Add column with tidy country names
u0$country <- gsub("Country.", "", rownames(u0))

#Add column with rank of residual term
u0$rank <- rank(u0$mean, ties.method = "first")

#Set row names to rank
rownames(u0) <- u0$rank

#Arrange countries in order of rank
u0 <- u0[as.character(1:nrow(u0)),]

#Plot results
ggplot(u0, aes(y = rank, x = mean)) + 
  geom_point(stat = "identity") + 
  geom_pointrange(aes(xmin = `lwr_99%`, xmax = `upr_99%`)) +
  geom_vline(xintercept = 0, colour = "red") +
  scale_y_continuous(breaks = 1:nrow(u0),
                     labels = u0$country) +
  labs(x = "Residual", y = "Country") 

#trust variables ============================================================

#Store number of parameters
n_params <- 4

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
mcmc_trust <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust,
                          random = ~ us(1 + Family_Trust + Particular_Trust + General_Trust):Country,
                          pr = TRUE, #store random effects
                          data = data,
                          prior = prior_dta,
                          nitt = n_iter, burnin = n_burn, thin = 1,
                          chains = n_chains)


#Get names of desired parameters
params <- names(mcmc_trust$estimates[,2:5])
resid_terms <- names(mcmc_trust$residuals)[-1]

#View results
mcmc_summary(mcmc_trust, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_trust, element = "residuals", alpha = alpha_lvl, params = resid_terms)

#Examine trace plots of coefficients
trace_plot_list <- map(params, function(x) mcmc_trace(mcmc_trust, param =  x)) %>% 
  set_names(params)

trace_plot_list

#Examine trace plot of residual terms
trace_plot_list <- map(resid_terms, 
                       function(x) mcmc_trace(mcmc_trust, element = "residuals", param =  x)) %>% 
  set_names(resid_terms)

trace_plot_list


#Compute posterior means of covariance matrix for random effects
cov_mx <- mcmc_trust$residuals[,2:17] %>% 
  colMeans() %>%
  structure(.Dim = c(n_params, n_params)) 


#Inspect standard deviations
sqrt(diag(cov_mx))

#Convert to correlations
cor_mx <- cov2cor(cov_mx) %>% as.data.frame()

#Apply neater names
colnames(cor_mx) <- params[1:4]
rownames(cor_mx) <- params[1:4]

#Inspect correlations
cor_mx

#Save results
saveRDS(mcmc_trust, file = "mcmc_trust.Rdata")


#All variables ========================================================================

#Store number of parameters and random effects
n_params <- 18
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
u_var <- diag(c(sigma2, rep(0.05, 3)))

#Organize priors into lists
prior_dta <- list(G = list(G1 = list(V = u_var, nu = n_random)),
                  R = list(V = 1, nu = 0.002),
                  B = list(mu = prior_mu, V = prior_var))



#Run five chains
set.seed(2016)
mcmc_all <- mcmc_chains(Suicide ~ Family_Trust + Particular_Trust + General_Trust +
                          Self_Val + 
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
params <- names(mcmc_all$estimates[,2:19])
resid_terms <- names(mcmc_all$residuals)[-1]

#View results
mcmc_summary(mcmc_all, alpha = alpha_lvl, params = params)
mcmc_summary(mcmc_all, element = "residuals", alpha = alpha_lvl, params = resid_terms)

#Examine trace plots of coefficients
trace_plot_list <- map(params, function(x) mcmc_trace(mcmc_all, param =  x)) %>% 
  set_names(params)

trace_plot_list

#Examine trace plot of residual terms
trace_plot_list <- map(resid_terms, 
                       function(x) mcmc_trace(mcmc_all, element = "residuals", param =  x)) %>% 
  set_names(resid_terms)

trace_plot_list


#Compute posterior means of covariance matrix for random effects
cov_mx <- mcmc_all$residuals[,2:17] %>% 
  colMeans() %>%
  structure(.Dim = c(4, 4)) 


#Inspect standard deviations
sqrt(diag(cov_mx))

#Convert to correlations
cor_mx <- cov2cor(cov_mx) %>% as.data.frame()

#Apply neater names
colnames(cor_mx) <- params[1:4]
rownames(cor_mx) <- params[1:4]

#Inspect correlations
cor_mx

#Save results
saveRDS(mcmc_all, file = "mcmc_all.Rdata")