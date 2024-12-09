#cor_mx
cor_mx <- function(data, vars = NULL, type = "Pearson") {
  
  #If vars is not NULL, limit data to selected variables
  if(!is.null(vars)){
    data <- data[vars]
  }
  
  #Implement appropriate correlation type
  if(type == "Pearson") {
    mx <- round(cor(data), 3)
  }
  
  if(type == "Polychoric") {
    mx <- round(psych::polychoric(data)$rho, 3)
  }
  
  #Blank out upper tirangle
  mx[upper.tri(mx)] <- " "
  
  return(mx)
}




#alpha_table
alpha_tab <- function(data) {
  
  #Get item correlation with total scale
  r_post_drop <- round(data$item.stats["r.drop"], 3)
  
  #Get scale alpha after dropping item
  alpha_post_drop <- round(data[["alpha.drop"]]["raw_alpha"], 3)
  
  #Arrange results into a data frame
  results <- data.frame(r_post_drop,
                        alpha_post_drop)
  
  #Assign column names
  names(results) <- c("Scale cor.", "Alpha after drop")
  
  #Create blank row
  blank <- rep(" ", 2)
  
  #Add blank row to end of results
  results <- rbind(results, blank)
  
  #Get overall alpha
  alpha_total <- paste("Raw Alpha =", round(data[["total"]]["raw_alpha"], 3))
  
  #Set last row name as the overall alpha
  rownames(results)[nrow(results)] <- alpha_total
  
  return(results)
}


#eigen_table
eigen_table <- function(data){
  
  #Get number of components
  n <- length(data)
  
  #Calculate proportion of explained variance
  prop <- round(data/n, 3)
  
  #Calculate cumulative proportion of explained variance
  cum_prop <- round(cumsum(data/n), 3)
  
  #Construct variable listing each component
  n_comp <- as.character(seq(1, n, 1))
  
  #Combine results 
  results <- rbind(n_comp, prop, cum_prop)
  
  #Assign names
  rownames(results) <- c("Component", "% Var. Explained", "% Cumulative")
  colnames(results) <- rep(" ", n)
  
  #Convert to data frame
  results <- as.data.frame(results)
  
  return(results)
}


#arrange_loadings
arrange_loadings <- function(mod, n_dim) {
  
  #Extract factor loadings
  loadings <- structure(.Data = c(mod[["loadings"]]),
                        .Dim = n_dim)
  
  #Extract communalities
  coms <- c(mod[["communalities"]])
  
  #Attach items and communalities
  loadings <- cbind(loadings, coms)
  
  #Construct factor names
  factor_names <- paste("Factor", 1:n_dim[2])
  
  #Apply names
  colnames(loadings) <- c(factor_names, "Communalities")
  
  #Round to 2 decimal places
  loadings <- round(loadings, 3)
  
  return(loadings)
}


#arrange_cors
arrange_cors <- function(mod, n_dim) {
  
  #If an orthogonal rotation was used, set factor correlation to blank
  if(mod[["rotation"]] %in% c("none", "varimax")) {
    f_cor <- diag(1, nrow = n_dim[2], ncol = n_dim[2])
    
    #Apply row names
    rownames(f_cor) <- paste("Factor", 1:ncol(f_cor))
  } else {
    #Otherwise, extract factor correlation matrix
    f_cor <- mod[["Phi"]]
    
    #Round to 2 decimal places
    f_cor <- round(f_cor, 3)
    
    #Apply row names
    rownames(f_cor) <- paste("Factor", 1:ncol(f_cor))
  }
  
  return(f_cor)
}


#arrange_fit_indices
arrange_fit_indices <- function(mod) {
  
  #If degrees of freedom is equal to or less than 0, set TLI, RMSEA and the confidence interval to NA
  if(mod[["dof"]] <= 0) {
    TLI <- NA
    RMSEA <- NA
  } else {
    #Otherwise, extract these fit indices
    TLI <- mod[["TLI"]]
    RMSEA <- mod[["RMSEA"]][1]
    lwr <- mod[["RMSEA"]][2]
    upr <- mod[["RMSEA"]][3]
    
    #Round to 2 decimal places
    TLI <- round(TLI, 3)
    RMSEA <- round(RMSEA, 3)
    lwr <- round(lwr, 3)
    upr <- round(upr, 3)
    
    #Add confidence intervals to RMSEA
    lwr <- paste0("(", lwr)
    upr <- paste0(upr, ")")
    CI <- paste(lwr, upr)
    RMSEA <- paste(RMSEA, CI)
  }
  
  #Extract and round RMSR
  RSMR <- mod[["rms"]]
  RSMR <- round(RSMR, 3)
  
  #Organise results
  fit_indices <- matrix(c(RMSEA, TLI, RSMR), ncol = 1, nrow = 3)
  
  #Assign row names
  rownames(fit_indices) <- c("RMSEA (90% CI)", "TLI", "RSMR")
  
  return(fit_indices)
}

#fa_table
fa_table <- function(mod) {
  
  #Get rows/columns for factors
  n_dim <- dim(mod[["loadings"]])
  
  #Arrange loadings
  loadings <- arrange_loadings(mod = mod, n_dim = n_dim)
  
  #Extract variance explained and round
  var_explained <- mod[["Vaccounted"]][2,]
  var_explained <- round(var_explained, 3)
  
  #Arrange correlations
  f_cor <- arrange_cors(mod = mod, n_dim = n_dim)
  
  #Arrange fit indices
  fit_indices <- arrange_fit_indices(mod = mod)
  
  #Create blank row for spacing
  space <- rep(" ", length(var_explained)) 
  
  #Add blank columns for fit indices 
  fit_indices <- cbind(fit_indices,
                       matrix(" ", nrow = 3, ncol = length(var_explained) - 1))
  
  #Stack variance explained and factor correlations
  lower <- rbind(" " = space, 
                 "% Var." = var_explained,
                 " " = space,
                 "Phi" = " ",
                 f_cor,
                 " " = space,
                 "Fit Indices" = " ",
                 fit_indices)
  
  #Add a blank column to var_cor
  lower <- cbind(lower, " ")
  
  #Add lower part of table to loadings
  results <- rbind(loadings, lower)
  
  return(results)
}


#structure_mx
structure_mx <- function(mod) {
  
  #Get rows/columns for factors
  n_dim <- dim(mod[["loadings"]])
  
  #Extract loadings
  loads <- structure(.Data = c(mod[["loadings"]]),
                     .Dim = n_dim)
  
  #Extract factor correlation
  f_cor <- mod[["Phi"]]
  
  #Get structure matrix from pattern matrix of standardized loadings
  str_mx <- loads %*% f_cor
  
  
  #Apply columnn and row names
  colnames(str_mx) <- paste("Factor", 1:n_dim[2])
  rownames(str_mx) <- rownames(mod[["loadings"]])
  
  #Round to 3 decimal places
  str_mx <- round(str_mx, 3)
  
  return(str_mx)
}




#map_table
map_table <- function(test, n_factors = NULL) {
  
  #If no number of factors is specificed, show all results
  if(is.null(n_factors)) {
    n_factors <- length(test[["map"]])
  }
  
  #Extract MAP results
  map_vals <- test[["map"]][1:n_factors]
  
  #Extract degrees of freedom and RMSEA
  fit_indices <- test[["vss.stats"]][1:n_factors, c("dof", "RMSEA")]
  
  #Arrange results into data frame
  results <- cbind(map_vals, fit_indices)
  
  #Roudn to 3 decimal places
  results <- round(results, 3)
  
  return(results)
}



#verify_structure
verify_structure <- function(loads, factor_str, cutoff) {
  
  two_factors <- map_vec(loads, ncol)
  
  if(any(two_factors) > 2) stop("verify_structure only suitable for 2-factor models")
  if(ncol(factor_str) > 2) stop("verify_structure only suitable for 2-factor models")
  if(is.null(names(loads))) stop("List elements need to be named prior to using verify_structure")
  
  #Apply cutoffs to absolute loadings (disregarding positive or negative sign)
  loads_logical <- map(loads, function(x) abs(x) > cutoff)
  
  #Check for factor structure
  structure_check <- map_vec(loads_logical, function(x) all(x == factor_str))
  
  
  #Correct results for positive and negative signs
  all_pos <- sign(factor_str)
  all_neg <- all_pos * -1
  neg_pos <- cbind(all_neg[,1], all_pos[,2])
  pos_neg <- cbind(all_pos[,1], all_neg[,2])
  
  #Set loadings below cutoff to zero
  adj_loads <- map(loads[structure_check],
                   function(x) ifelse(factor_str,
                                      x, 0))
  
  #Set adjusted loadings to 1 if positive and -1 if negative
  adj_loads <- map(adj_loads, sign)
  
  #Run checks per each possible factor structure
  all_pos_check <- map(adj_loads, function(x) all(x == all_pos))
  all_neg_check <- map(adj_loads, function(x) all(x == all_neg))
  neg_pos_check <- map(adj_loads, function(x) all(x == neg_pos))
  pos_neg_check <- map(adj_loads, function(x) all(x == pos_neg))
  
  
  #Record whether factor structure was observed anywhere
  obs_str <- ifelse(all_pos_check == TRUE | 
                      all_neg_check == TRUE | 
                      neg_pos_check == TRUE |
                      pos_neg_check == TRUE,
                    TRUE, FALSE)
  
  #Convert to list
  obs_str <- as.list(obs_str)
  
  #Remove groups where factor structure was not observed
  obs_str <- discard(obs_str, function(x) x == FALSE)
  
  #Get groups where structure was observed
  observed <- names(obs_str)
  
  #Get groups where structure was not observed
  not_observed <- names(loads)[!names(loads) %in% observed]
  
  #Compile results
  results <- list(observed = observed,
                  not_observed = not_observed)
  
  return(results)
}