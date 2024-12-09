#standardize 
standardize <- function(variable) {
  #Standardize variable
  (variable - mean(variable, na.rm = TRUE))/sd(variable, na.rm = TRUE)
}


#centre
centre <- function(variable) {
  #Mean-centre variable
  variable - mean(variable, na.rm = TRUE)
}



#dummy out
dummy_out <- function(variable, data, rm_original = FALSE) {
  
  #store variable name
  var_name <- variable
  
  #extract variable
  variable <- data[[variable]]
  
  if(rm_original == TRUE) {
    #get pattern of variables to keep and remove
    pattern <- !names(data) == var_name
    
    #remove redundant variables
    data <- data[,pattern]
  }
  
  #get categories of variable
  cats <- unique(variable)
  
  #remove NA values
  cats <- cats[!is.na(cats)]
  
  #dummy out categories
  new_data <- map(cats, function(x) as.numeric(variable == x))
  
  #name list elements after variable categories
  names(new_data) <- cats
  
  #convert to data
  new_data <- list2DF(new_data)
  
  #add new data to original data
  data <- cbind(data, new_data)
  
  return(data)
} 



#value_index
value_index <- function(data, index) {
  
  #Store names of necessary variables
  var_names <- c("Abortion", "Divorce", "Homosexuality",
                 "Men_Jobs", "Men_Pol", "Men_Uni", 
                 "Independence", "Imagine", "Obedience", "Faith")
  
  #Check variables are in data
  check_names <- var_names %in% names(data)
  
  if(all(check_names) == FALSE) {
    #Get problematic variables
    miss_vars <- var_names[!check_names]
    
    #Construct error message
    error <- paste("The following variables were not found in the data:", paste0(miss_vars, collapse = ", "))
    
    stop(error)
  }
  
  #Extract variables
  Abortion <- data[["Abortion"]]
  Divorce <- data[["Divorce"]]
  Homosexuality <- data[["Homosexuality"]]
  
  Men_Jobs <- data[["Men_Jobs"]]
  Men_Pol <- data[["Men_Pol"]]
  Men_Uni <- data[["Men_Uni"]]
  
  Independence <- data[["Independence"]]
  Imagine <- data[["Imagine"]]
  Obedience <- data[["Obedience"]]
  Faith <- data[["Faith"]]
  
  if(index == "full") {
    #Construct components
    comp_1 <- (Abortion + Divorce + Homosexuality)/3
    comp_2 <- (Men_Jobs + Men_Pol + Men_Uni)/3
    comp_3 <- (Independence + Imagine + Obedience + Faith)/4
    
    #Add component scores
    self_val <- comp_1 + comp_2 + comp_3
  }
  
  if(index == "partial") {
    #Construct components
    comp_1 <- (Abortion + Divorce)/2
    comp_2 <- (Men_Jobs + Men_Pol + Men_Uni)/3
    comp_3 <- (Independence + Imagine + Obedience + Faith)/4
    
    #Add component scores
    self_val <- comp_1 + comp_2 + comp_3
  }
  
  return(self_val)
}
