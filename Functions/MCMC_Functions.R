#index_chain
index_chain <- function(post_draws, chain_id) {
  
  #Convert to data frame
  post_draws <- as.data.frame(post_draws) 
  
  #Store variable names
  var_names <- names(post_draws)
  
  #Add chain indicator
  post_draws[["chain"]] <- chain_id
  
  #Reorder variables so chain is first column
  post_draws <- post_draws[c("chain", var_names)]
  
  return(post_draws)
}


#arrange_mcmc
arrange_mcmc <- function(post_draws) {
  
  #Get number of chains
  n_chains <- length(post_draws)
  
  #Index each chain
  post_draws <- map2(.x = post_draws,
                     .y = 1:n_chains,
                     function(x, y) index_chain(x, y))
  
  #Combine all chains
  post_draws <- reduce(post_draws, rbind)
  
  return(post_draws)
}


#mcmc_chains
mcmc_chains <- function(..., chains, start = NULL) {
  
  #If no starting values are provided, create empty list of values
  if(is.null(start)) {
    start <- list(as.list(rep(NA, chains)))
  }
  
  #Run model with specified number of chains
  mcmc_mod <- map2(1:chains, start, function(i, s) {
    MCMCglmm::MCMCglmm(..., start = s)
  })
  
  #Estimates
  #Extract chains of estimates as lists
  estimates <- map(mcmc_mod, function(x) x[["Sol"]])
  
  #Combine all chains into a single data frame with a chain index
  estimates <- arrange_mcmc(estimates)
  
  #Cuts
  #Check if information is available on cut-off points (ordinal models)
  check_cuts <- map_vec(mcmc_mod, function(x) is.null(x[["CP"]]))
  
  #If check_cuts is null, create object indicating no cuts are available
  if(any(check_cuts)) {
    cuts <- "No cut points available"
  } else {
    #Extract cut-off points for intercept (for ordinal values)
    cuts <- map(mcmc_mod, function(x) x[["CP"]])
    
    #Combine all chains into a single data frame with a chain index
    cuts <- arrange_mcmc(cuts)
  }
  
  #Deviance
  #Extract chains of deviance as lists
  deviance <-  map(mcmc_mod, function(x) x[["Deviance"]])
  
  #Combine all chains into a single data frame with a chain index
  deviance <- arrange_mcmc(deviance)
  
  #Apply appropriate names
  names(deviance) <- c("chain", "deviance")
  
  #Residuals
  #Extract chains of residuals
  residuals <- map(mcmc_mod, function(x) x[["VCV"]])
  
  #Combine all chains into a single data frame with a chain index
  residuals <- arrange_mcmc(residuals)
  
  #Estimation information
  n_samps <- nrow(estimates)
  n_per_chain <- n_samps/chains
  
  #Combine reuslts into a list
  results <- list(estimates = estimates,
                  cuts = cuts,
                  deviance = deviance,
                  residuals = residuals,
                  n_samps = n_samps,
                  n_per_chain = n_per_chain,
                  chains = chains)
  
  return(results)
}




#jags_dic
jags_dic <- function(mod){
  
  #Extract posterior draws of deviance
  deviance <- mod[["deviance"]][["deviance"]]
  
  #Calculate DIC
  dic <- mean(deviance) + (0.5*var(deviance))
  
  return(dic)
}




#mcmc_summary
mcmc_summary <- function(mod, element = "estimates", alpha = 0.05, params = NULL) {
  
  #Extract DIC
  dic <- jags_dic(mod)
  
  #Extract number of chains and number of samples per chain
  chains <- mod[["chains"]]
  n_per_chain <- mod[["n_per_chain"]]
  
  #Extract deviance
  deviance <- mod[["deviance"]][["deviance"]]
  
  #Extract model estimates
  mod <- mod[[element]]
  
  #Add deviance to model estimates
  mod <- cbind(mod, deviance)
  
  #If no parameters are specified, only return the intercept and deviance
  if(is.null(params)) {
    mod <- mod["deviance"]
  } else {
    #Otherwise, include additional parameters
    params <- c(params, "deviance")
    mod <- mod[params]
  }
  
  #Get upper and lower bounds for credible intervals
  lw <- alpha/2
  up <- 1 - (alpha/2)
  
  #Estimates
  #Get posterior means
  mcmc_mean <- map_vec(mod, mean)
  mcmc_mean <- round(mcmc_mean, 3)
  
  #Get posterior SD values
  mcmc_sd <- map_vec(mod, sd)
  mcmc_sd <- round(mcmc_sd, 3)
  
  #Get values for lower credible interval bound
  mcmc_lw <- map_vec(mod, function(x) quantile(x, probs = lw))
  mcmc_lw <- round(mcmc_lw, 3)
  
  #Get values for upper credible interval bound
  mcmc_up <- map_vec(mod, function(x) quantile(x, probs = up))
  mcmc_up <- round(mcmc_up, 3)
  
  #Diagnostics
  #Organize posterior draws into a list of matrices
  mx_draws <- map(names(mod), function(x) matrix(mod[[x]],
                                                 nrow = n_per_chain, 
                                                 ncol = chains))
  
  #Calculate Rhats for each variable
  rhats <- map_vec(mx_draws, function(x) rstan::Rhat(x))
  rhats <- round(rhats, 5)
  
  #Calculate bulk effective sample size for each variable
  bulk <- map_vec(mx_draws, function(x) rstan::ess_bulk(x))
  bulk <- round(bulk, 3)
  
  #Calculate tail effective sample size for each variable
  tail <- map_vec(mx_draws, function(x) rstan::ess_tail(x))
  tail <- round(tail, 3)
  
  #Combine results 
  post_stats <- data.frame(mcmc_mean, 
                           mcmc_sd, 
                           mcmc_lw, 
                           mcmc_up,
                           rhats,
                           bulk,
                           tail)
  
  #Set up names for credible bounds
  cred_int <- (1 - alpha) * 100
  cred_int <- paste0(cred_int, "%")
  
  lw_name <- paste("lwr", cred_int, sep = "_")
  up_name <- paste("upr", cred_int, sep = "_")
  
  #Apply names
  names(post_stats ) <- c("mean", "sd", 
                          lw_name, up_name, 
                          "r-hat", "ess_bulk", "ess_tail")
  
  
  #Store all results as a list
  results <- list(post_stats  = post_stats , 
                  dic = dic)
  
  return(results)
}


#mcmc_trace
mcmc_trace <- function(mod, element = "estimates", param, segment = NULL) {
  
  #Extract posterior draws for desired parameter
  post_draws <- mod[[element]][c("chain", param)]
  
  #Extract number of samples per chain
  n_per_chain <- mod[["n_per_chain"]]
  
  #Extract number of chains
  chains <- mod[["chains"]]
  
  #Rename columns
  names(post_draws) <- c("chain", "draws")
  
  #Convert chain index to factor
  post_draws$chain <- factor(post_draws$chain)
  
  #Construct variable for iterations
  iter <- rep(1:n_per_chain, chains)
  
  #Add to data of posterior draws
  post_draws$iter <- iter
  
  #Construct titles
  plot_title <- paste("Trace of", param)
  plot_sub <- paste("Chains =", chains, "\nSamples per chain =", n_per_chain)
  
  
  #If no segement is specified, use the full range of iterations
  if(is.null(segment)) {
    segment <- c(1, n_per_chain)
  }
  
  #Construct trace plot
  trace_plot <- ggplot(post_draws, 
                       aes(x = iter, y = draws, colour = chain)) +
    geom_line(show.legend = FALSE) +
    scale_color_brewer(palette = "Set2") +
    labs(title = plot_title, subtitle = plot_sub, 
         x = "Iteration", y = "Posterior Draw") +
    xlim(segment)
  
  return(trace_plot)
}




#get_vpc
get_vpc <- function(mod, alpha = 0.05){
  
  #Extract number of chains and number of samples per chain
  chains <- mod[["chains"]]
  n_per_chain <- mod[["n_per_chain"]]
  
  #VPC draws
  #Extract residuals (excluding chain index)
  all_chains <- mod[["residuals"]][,-1] 
  
  #Extract column names of all_chains
  resid_terms <- names(all_chains)
  
  #Calculate totals 
  total <- rowSums(all_chains)
  
  #Calculate VPC values
  vpc_draws <- map(resid_terms, function(x) all_chains[[x]]/total)
  
  #Set names based on resid_term
  names(vpc_draws) <- resid_terms
  
  #Convert to data frame
  vpc_draws <- list2DF(vpc_draws)
  
  #Estimates
  #Get upper and lower bounds for credible intervals
  lw <- alpha/2
  up <- 1 - (alpha/2)
  
  #Get posterior means
  mcmc_mean <- map_vec(vpc_draws, mean)
  mcmc_mean <- round(mcmc_mean, 3)
  
  #Get posterior SD values
  mcmc_sd <- map_vec(vpc_draws, sd)
  mcmc_sd <- round(mcmc_sd, 3)
  
  #Get values for lower credible interval bound
  mcmc_lw <- map_vec(vpc_draws, function(x) quantile(x, probs = lw))
  mcmc_lw <- round(mcmc_lw, 3)
  
  #Get values for upper credible interval bound
  mcmc_up <- map_vec(vpc_draws, function(x) quantile(x, probs = up))
  mcmc_up <- round(mcmc_up, 3)
  
  #Diagnostics
  #Organize posterior draws into a list of matrices
  mx_draws <- map(resid_terms, function(x) matrix(vpc_draws[[x]],
                                                  nrow = n_per_chain, 
                                                  ncol = chains))
  
  #Calculate Rhats for each variable
  rhats <- map_vec(mx_draws, function(x) rstan::Rhat(x))
  rhats <- round(rhats, 3)
  
  #Calculate bulk effective sample size for each variable
  bulk <- map_vec(mx_draws, function(x) rstan::ess_bulk(x))
  bulk <- round(bulk, 3)
  
  #Calculate tail effective sample size for each variable
  tail <- map_vec(mx_draws, function(x) rstan::ess_tail(x))
  tail <- round(tail, 3)
  
  #Combine results 
  vpc_summary <- data.frame(mcmc_mean, 
                            mcmc_sd, 
                            mcmc_lw, 
                            mcmc_up,
                            rhats,
                            bulk,
                            tail)
  
  #Set up names for credible bounds
  cred_int <- (1 - alpha) * 100
  cred_int <- paste0(cred_int, "%")
  
  lw_name <- paste("lwr", cred_int, sep = "_")
  up_name <- paste("upr", cred_int, sep = "_")
  
  #Apply names
  names(vpc_summary) <- c("mean", "sd", 
                          lw_name, up_name, 
                          "r-hat", "ess_bulk", "ess_tail")
  
  
  #Arrange results into a list
  vpc_results <- list(vpc_summary, vpc_draws)
  
  #Name list elements
  names(vpc_results) <- c("posterior_estimates", "posterior_draws")
  
  return(vpc_results)
}




#post_sum
post_sum <- function(data, alpha = 0.05) {
  
  #Extract variable names
  var_names <- names(data)
  
  #Get upper and lower bounds for credible intervals
  lw <- alpha/2
  up <- 1 - (alpha/2)
  
  #Estimates
  #Get posterior means
  mcmc_mean <- map_vec(data, mean)
  mcmc_mean <- round(mcmc_mean, 3)
  
  #Get posterior SD values
  mcmc_sd <- map_vec(data, sd)
  mcmc_sd <- round(mcmc_sd, 3)
  
  #Get values for lower credible interval bound
  mcmc_lw <- map_vec(data, function(x) quantile(x, probs = lw))
  mcmc_lw <- round(mcmc_lw, 3)
  
  #Get values for upper credible interval bound
  mcmc_up <- map_vec(data, function(x) quantile(x, probs = up))
  mcmc_up <- round(mcmc_up, 3)
  
  #Arrange results
  post_stats <- data.frame(mcmc_mean, 
                           mcmc_sd, 
                           mcmc_lw, 
                           mcmc_up)
  
  #Set up names for credible bounds
  cred_int <- (1 - alpha) * 100
  cred_int <- paste0(cred_int, "%")
  
  lw_name <- paste("lwr", cred_int, sep = "_")
  up_name <- paste("upr", cred_int, sep = "_")
  
  #Apply names
  names(post_stats ) <- c("mean", "sd", 
                          lw_name, up_name)
  
  
  return(post_stats)
  
}


