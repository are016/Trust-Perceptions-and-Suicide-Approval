#mcmc_predict
mcmc_predict <- function(mod, params, variable, min_val, max_val, n_vals, poly_terms = NULL, alpha = 0.05) {
  
  #If the intercept is missing from parameters, add it to the vector
  if("(Intercept)" %in% params == FALSE) {
    params <- c("(Intercept)", params)
  }
  
  #Make a data frame to predict from (setting all variables to 0)
  pred_data <- data.frame(matrix(0, nrow = n_vals, ncol = length(params)))
  
  #Name columns after parameters
  names(pred_data) <- params
  
  #Extract coefficients
  coefs <- mod[["estimates"]][params]
  coefs <- as.matrix(coefs)
  
  #Get range of values for variable to predict from
  pred_data[[variable]] <- seq(from = min_val, to = max_val, length.out = n_vals)
  
  if(!is.null(poly_terms)) {
    #Get number of polynomial terms and add 1 (needs to start at 2 for squared terms)
    n_poly <- length(poly_terms) + 1
    
    #Get non-linear versions of variable
    poly_vars <- map(2:n_poly,
                     function(x) pred_data[[variable]]^x)
    
    #Name non-linear variables after respective polynomial terms
    names(poly_vars) <- poly_terms
    
    #Convert to data frame
    poly_vars <- list2DF(poly_vars)
    
    #Add non-linear variables to prediction data
    pred_data[poly_terms] <- poly_vars
  }
  
  #Add constant
  pred_data[["(Intercept)"]] <- 1
  
  #Convert prediction data to matrix form
  pred_data <- as.matrix(pred_data)
  
  #Predict
  pred_vals <- t(pred_data %*% t(coefs)) 
  
  #Convert to data frame
  pred_vals <- as.data.frame(pred_vals)
  
  
  #Prepare data frame for posterior means and credible intervals
  estimates <- data.frame(matrix(0, nrow = nrow(pred_data), ncol = 4))
  names(estimates) <- c(variable, "fit", "lw", "up")
  
  #Get range of values for variable that was used for prediction
  estimates[[variable]] <- seq(from = min_val, to = max_val, length.out = n_vals)
  
  #Get upper and lower bounds for credible intervals
  lw <- alpha/2
  up <- 1 - (alpha/2)
  
  #Get average of predicted values and lower/upper bounds
  estimates[["fit"]] <- map_vec(pred_vals, mean)
  estimates[["lw"]] <- map_vec(names(pred_vals), 
                               function(x) quantile(pred_vals[[x]], probs = lw))
  estimates[["up"]] <- map_vec(names(pred_vals), 
                               function(x) quantile(pred_vals[[x]], probs = up))
  
  return(estimates)
}




#get_error
get_error <- function(mod, data, outcome, cluster_var, cluster, params, alpha = 0.05) {
  
  #Limit data to relevant cluster
  data <- data[data[[cluster_var]] == cluster,] 
  
  #Extract outcome variable
  outcome <- data[[outcome]]
  
  #Add (Intercept) term to data
  data[["(Intercept)"]] <- 1
  
  #Arrange data in same order as params
  data <- data[params]
  
  #Create cluster-specific term for extracting random effects
  cluster_term <- paste0(".", cluster_var, ".", cluster)
  
  #Get random effect names
  re_names <- grep(cluster_term, names(mod[["estimates"]]), value = TRUE)
  
  #Extract random effects
  re <- mod[["estimates"]][re_names]
  
  #Get names of variables that have random effects
  var_names <- gsub(cluster_term, "", re_names)
  
  #Add random effects to relevant coefficients
  mod[["estimates"]][var_names] <- mod[["estimates"]][var_names] + re
  
  #Extract coefficients 
  coefs <- mod[["estimates"]][params]
  
  #Convert to data and coefs to matrix
  data <- as.matrix(data)
  coefs <- as.matrix(coefs)
  
  #Predict
  pred_vals <- t(data %*% t(coefs)) 
  
  #Convert to data frame
  pred_vals <- as.data.frame(pred_vals)
  
  #Subtract observed values
  pred_vals <- map2(.x = outcome, .y = pred_vals,
                    function(x, y) x - y)
  
  
  #Convert to data frame
  pred_vals <- list2DF(pred_vals)
  
  
  #Get average error per iteration
  pred_var <- apply(pred_vals, 1, var)
  
  return(pred_var)
}



#get_re
get_re <- function(mod, variable, cluster_var) {
  
  #Extract grand mean of slopes
  grand_effect <- mod[["estimates"]][[variable]]
  
  #If intercept is requested, remove first bracket to avoid errors with grep
  if(variable == "(Intercept)") {
    variable <- "Intercept)"
  }
  
  #Get common name in slope terms
  common_name <- paste(variable, cluster_var, sep = ".")
  
  #Get cluster deviations from the slope grand mean
  re_terms <- grep(common_name, names(mod[["estimates"]]), value = TRUE)
  
  #Extract cluster deviations from the slope grand mean
  re_deviations <- mod[["estimates"]][re_terms]
  
  #Add grand mean to deviations to get cluster slopes
  re <- re_deviations + grand_effect
  
  return(re)
}


#arrange_re
arrange_re <- function(re, var_name) {
  
  #Convert to list
  re <- as.list(re)
  
  #Make each list element a data frame with one column
  re <- map(re, function(x) data.frame(x))
  
  #Give each column the same name
  re <- map(re, function(x) set_names(x, var_name))
  
  return(re)
}


#mcmc_slopes
mcmc_slopes <- function(mod, params, variable, cluster_var, poly_terms = NULL, min_val, max_val, n_vals) {
  
  if("(Intercept)" %in% params == FALSE) {
    params <- c("(Intercept)", params)
  }
  
  #Make a data frame to predict from
  pred_data <- data.frame(matrix(0, nrow = n_vals, ncol = length(params)))
  
  #Name columns after parameters
  names(pred_data) <- params
  
  #Extract random intercepts and slopes
  intercepts <- get_re(mod, "(Intercept)", cluster_var)
  slopes <- get_re(mod, variable, cluster_var)
  
  #Arrange random effects
  intercepts <- arrange_re(intercepts, "(Intercept)")
  slopes <- arrange_re(slopes, variable)
  
  #Get common name in slope terms
  common_name <- paste(variable, cluster_var, sep = ".")
  
  #Construct cluster names
  cluster_names <- gsub(paste0(common_name, "."), "", names(slopes))
  
  #Apply neater names
  names(slopes) <- cluster_names
  names(intercepts) <- cluster_names
  
  if(!is.null(poly_terms)) {
    #If polynomial terms are included, extract them
    poly_slopes <- map(poly_terms, 
                       function(x) get_re(mod, x, cluster_var))
    
    #Convert to data frame
    poly_slopes <- reduce(poly_slopes, function(x, y) cbind(x, y))
    
    #Get names of all terms per cluster
    poly_cluster <- map(cluster_names, function(x) grep(x, names(poly_slopes), value = TRUE))
    
    #Convert to a list where each element is a data frame of polynomial terms that cluster
    poly_slopes <- map(poly_cluster, function(x) poly_slopes[x])
    
    #Give each element the same column names
    poly_slopes <- map(poly_slopes, function(x) set_names(x, poly_terms))
    
    #Name each element after its cluster name
    names(poly_slopes) <- cluster_names
  }
  
  #Extract coefficients and remove intercept and variable of interest
  coefs <- mod[["estimates"]][params]
  coefs <- coefs[!names(coefs) %in% c(variable, "(Intercept)")]
  
  #If polynomial terms are inlcuded, remove them from the coefficients
  if(!is.null(poly_terms)) {
    coefs <- coefs[!names(coefs) %in% c(poly_terms)]
  }
  
  #Add the cluster specific slopes and intercepts to the coefficients
  coefs <- map2(slopes, intercepts, function(x, y) cbind(x, y, coefs))
  
  #If polynomial terms are inlcuded, add the cluster specific slopes to the coefficients
  if(!is.null(poly_terms)) {
    coefs <- map2(coefs, poly_slopes, function(x, y) cbind(x, y))
  }
  
  #Reorder the parameters so they are in the original order
  coefs <- map(coefs, function(x) x[params])
  
  #Convert each set of coefficients to matrix form
  coefs <- map(coefs, as.matrix)
  
  #Get range of values for variable to predict from
  pred_data[[variable]] <- seq(from = min_val, to = max_val, length.out = n_vals)
  
  #Add constant
  pred_data[["(Intercept)"]] <- 1
  
  if(!is.null(poly_terms)) {
    #Get number of polynomial terms and add 1 (needs to start at 2 for squared terms)
    n_poly <- length(poly_terms) + 1
    
    #Get non-linear versions of variable
    poly_vars <- map(2:n_poly,
                     function(x) pred_data[[variable]]^x)
    
    #Name non-linear variables after respective polynomial terms
    names(poly_vars) <- poly_terms
    
    #Convert to data frame
    poly_vars <- list2DF(poly_vars)
    
    #Add non-linear variables to prediction data
    pred_data[poly_terms] <- poly_vars
  }
  
  #Convert prediction data to matrix form
  pred_data <- as.matrix(pred_data)
  
  #Predict
  pred_vals <- map(coefs, 
                   function(x) t(pred_data %*% t(x))) 
  
  #Convert to data frame
  pred_vals <- map(pred_vals, as.data.frame)
  
  #Get average of predicted values 
  estimates <- map(pred_vals, colMeans)
  
  #Convert to data frame
  estimates <- list2DF(estimates)
  
  #Add predictor variable
  estimates[[variable]] <- seq(from = min_val, to = max_val, length.out = n_vals)
  
  #Get names of clusters
  cluster_names <- names(estimates)[!names(estimates) == variable]
  
  #Pivot longer
  estimates <- pivot_longer(estimates,
                            cols = all_of(cluster_names),
                            names_to = cluster_var,
                            values_to = "Fit")
  
  return(estimates)
}


#select_clusters
select_clusters <- function(mod, re, n_cluster) {
  
  #Calculate posterior means of random effects
  post_means <- colMeans(re)
  
  #Order effects from smallest to largest
  post_means <- post_means[order(post_means)]
  
  #Construct an a column id running from the minimum to the maximum covering the desired number of clusters
  col_id <- seq(1, length(post_means), length.out = n_cluster) 
  col_id <- round(col_id)
  
  #Extract cluster names
  clusters <- names(post_means[col_id])
  
  return(clusters)
}
