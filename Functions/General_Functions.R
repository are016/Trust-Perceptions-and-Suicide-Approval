#get_mode
get_mode <- function(variable) {
  
  #Get counts of observations per category
  tab <- table(variable)
  
  #Convert to data frame
  tab <- as.data.frame(tab)
  
  #Apply names
  names(tab) <- c("Categories", "Count")
  
  #Get maximum count 
  max_val <- max(tab[["Count"]])
  
  #Get logical indicator of whether row contains maximum value
  log_ind <- tab[["Count"]] == max_val
  
  #Subset data based on logical indicator
  modal_value <- tab[log_ind, "Categories"]
  
  if(length(modal_value) > 1) {
    #If there are multiple modes, collapse them into a single value
    modal_value <- paste(modal_value, collapse = ", ")
  }
  
  return(as.character(modal_value))
}



#get_class
get_class <- function(data, of_class) {
  
  #Get class of each variable
  var_class <- map_vec(names(data), function(x) class(data[[x]]))
  
  #Limit variable names to those of the specified class
  var_names <- names(data)[var_class %in% of_class]
  
  return(var_names)
}




#get_missing
get_missing <- function(data) {
  
  #Get rows without missing values
  not_missing <- rownames(na.omit(data))
  
  #Construct indicator variable for missing values
  missing <- ifelse(rownames(data) %in% not_missing, "No", "Yes")
  
  #Get proportion of missing cases
  prop_miss <- prop.table(table(missing)) * 100
  
  return(prop_miss)
}



#compare_missing
compare_missing <- function(data, variable, alpha = 0.05) {
  
  #Get rows without missing values
  not_missing <- rownames(na.omit(data))
  
  #Construct indicator variable for missing values
  missing <- ifelse(rownames(data) %in% not_missing, "No", "Yes")
  
  #Extract variable
  variable <- data[[variable]]
  
  #If variable is continuous or ordinal, perform a t-test
  if(class(variable) %in% c("numeric", "integer")) {
    results <- t.test(variable ~ missing, conf.level = 1 - alpha)
  }
  
  #If variable is categorical, perform a chi-square test
  if(class(variable) %in% c("factor", "character")) {
    results <- list() # prepare empty list
    results[["chisq_test"]] <- chisq.test(variable, missing) #store test results
    
    counts <- results[["chisq_test"]][["observed"]] #extract counts
    results[["counts"]] <- prop.table(counts, 2) * 100 #convert to percentage
  }
  
  return(results)
}


#z_score
z_score <- function(data, variable, obs_var, obs) {
  
  #Create standardized version of varaible
  data[["z_score"]] <- standardize(data[[variable]])
  
  #Limit data to observation of interest
  obs <- data[data[[obs_var]] == obs, c(variable, "z_score")]
  
  return(obs)
}
