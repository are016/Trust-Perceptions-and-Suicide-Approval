#scale_posterior
scale_posterior <- function(mod) {
  
  #Extract posterior distribution of variance in residuals
  sigma2 <- mod[["residuals"]][["units"]]
  
  #Scale estimates
  mod[["estimates"]][,-1] <- mod[["estimates"]][,-1]/sqrt(1 + 1 * sigma2)
  
  #Scale cut points
  mod[["cuts"]][,-1] <- mod[["cuts"]][,-1]/sqrt(1 + 1 * sigma2)
  
  #Scale deviance
  mod[["deviance"]][,-1] <- mod[["deviance"]][,-1]/sqrt(1 + 1 * sigma2)
  
  return(mod)
}



#get_predictions
get_predictions <- function(mod, params, variable, min_val, max_val, n_vals) {
  
  if("(Intercept)" %in% params == TRUE) {
    params <- params[!grepl("(Intercept)", params)]
  }
  
  #Make a data frame to predict from
  pred_data <- data.frame(matrix(0, nrow = n_vals, ncol = length(params)))
  
  #Name columns after parameters
  names(pred_data) <- params
  
  #Extract coefficients
  coefs <- mod[["estimates"]][params]
  coefs <- as.matrix(coefs)
  
  #Get range of values for family trust to predict from
  pred_data[[variable]] <- seq(from = min_val, to = max_val, length.out = n_vals)
  
  
  #Convert prediction data to matrix form
  pred_data <- as.matrix(pred_data)
  
  #Predict
  pred_vals <- t(pred_data %*% t(coefs)) 
  
  #COnvert to data frame
  pred_vals <- as.data.frame(pred_vals)
  
  return(pred_vals)
}


#mcmc_probit_predict
mcmc_probit_prob <- function(mod, params, variable, min_val, max_val, n_vals, alpha = 0.05) {
  
  #Get predicted values without accounting for cut points
  pred_vals <- get_predictions(mod, 
                               params = params,
                               variable = variable, 
                               min_val = min_val, 
                               max_val = max_val, 
                               n_vals = n_vals)
  
  #Subtract intercept from cut points
  mod[["cuts"]][-1] <- mod[["cuts"]][-1] - mod[["estimates"]][["(Intercept)"]]
  
  #Multiply intercept by -1
  mod[["estimates"]][["(Intercept)"]] <- mod[["estimates"]][["(Intercept)"]] * -1
  
  #Extract cuts data frame and add intercept (first cut point)
  cuts <-  cbind(mod[["estimates"]][["(Intercept)"]], mod[["cuts"]][-1])
  
  #Get number of cut points
  k <- ncol(cuts)
  
  #Rename cut points
  names(cuts) <- paste("cut", 1:k, sep = "_")
  
  #Substract predicted values from cut points
  cuts <- map(names(cuts), function(x) cuts[[x]] - pred_vals)
  
  #Calculate cumulative probability of being in a given category
  cuts <- map(cuts, 
              function(x) map(x, pnorm) %>% list2DF)
  
  #Extract category 1
  cat_1 <- list(cuts[[1]])
  
  #Create data frame for final category
  cat_n <- list(1 - cuts[[k]]) 
  
  #Get probability of being in specific category
  cuts <- map2(.x = 1:(k-1), .y = 2:k,
               function(x, y) cuts[[y]] - cuts[[x]])
  
  #Add first and final category back to list
  cuts <- append(cat_1, cuts)
  cuts <- append(cuts, cat_n)
  
  #Get upper and lower bounds for credible intervals
  lw <- alpha/2
  up <- 1 - (alpha/2)
  
  #Extract average predicted probs per list element
  fit <- map(cuts, function(x) map_vec(x, mean))
  
  #Extract lower credible interval bounds per list element
  lw_ci <- map(cuts, function(x) map_vec(names(x), 
                                         function(y) quantile(x[[y]], probs = lw)))
  
  #Extract upper credible interval bounds per list element
  up_ci <- map(cuts, function(x) map_vec(names(x), 
                                         function(y) quantile(x[[y]], probs = up)))
  
  #Combine fitted values and credible intervals into data frames per list element
  probs <- pmap(.l = list(fit = fit, lw = lw_ci, up = up_ci), 
                function(fit, lw, up) data.frame(fit = fit, lw = lw, up = up))
  
  #Combine each list element into a single data frame
  probs <- bind_rows(probs)
  
  #Assign neater row names
  rownames(probs) <- 1:nrow(probs)
  
  #Add values of predictor variable to data
  vals <- seq(min_val, max_val, length.out = n_vals)
  vals <- rep(vals, k + 1)
  probs[[variable]] <- vals
  
  #Add category indicator to data
  cat_vals <- map(1:(k + 1), function(x) rep(x, n_vals))
  cat_vals <- unlist(cat_vals)
  probs[["cat"]] <- factor(cat_vals)
  
  return(probs)
}




#get_cuts
get_cuts <- function(mod) {
  #Subtract intercept from cut points
  cuts <- mod[["cuts"]][-1] - mod[["estimates"]][["(Intercept)"]]
  
  #Multiply intercept by -1
  cut_1 <- mod[["estimates"]][["(Intercept)"]] * -1
  
  #Extract cuts data frame and add intercept (first cut point)
  cuts <-  cbind(cut_1, cuts)
  
  return(cuts)
}
