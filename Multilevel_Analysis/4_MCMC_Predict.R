# MCMC Predictions ==========================================================

#Run set-up script
source("1_Set_Up.R")

#Set alpha level
alpha_lvl <- 0.05

#Load final model
mcmc_all <- readRDS("mcmc_all.Rdata")

#Get names of desired parameters
params <- names(mcmc_all$estimates[,2:19])

#Predicted values for average effects ======================================

#Family Trust ===============================================================

#Get fitted values 
plot_data <- mcmc_predict(mcmc_all, #model to predict with
                          params = params, #parameters to predict with
                          variable = "Family_Trust", #predictor variable
                          min_val = min(data$Family_Trust), #predict using values from the minimum of family trust...
                          max_val = max(data$Family_Trust), #... up to the maximum of family trust...
                          n_val = 100, #... in steps of 100
                          alpha = alpha_lvl) #95% credible intervals

#Adjust trust values (undo mean-centring)
plot_data$Family_Trust <- seq(1, 4, length.out = 100)

#Plot
ggplot(plot_data, aes(x = Family_Trust, y = fit)) +
  geom_line(colour = "steelblue", linewidth = 1) + 
  geom_ribbon(data = plot_data, aes(x = Family_Trust, 
                                    ymin = lw, ymax = up),
              fill = "blue", alpha = 0.2) +
  labs(x = "Family Trust", y = "Suicide Approval")  +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme(text = element_text(size = 20))


#Compute range of fitted values
min(plot_data$fit)
max(plot_data$fit)
max(plot_data$fit) - min(plot_data$fit)


#Particular Trust ============================================
#Get fitted values 
plot_data <- mcmc_predict(mcmc_all, #model to predict with
                          params = params, #parameters to predict with
                          variable = "Particular_Trust", #predictor variable 
                          min_val = min(data$Particular_Trust), #predict using values from the minimum of particular trust...
                          max_val = max(data$Particular_Trust), #... up to the maximum of particular trust...
                          n_val = 100, #... in steps of 100
                          alpha = 0.01) #95% credible intervals

#Adjust trust values (undo mean-centring)
plot_data$Particular_Trust <- seq(1, 4, length.out = 100)

#Plot
ggplot(plot_data, aes(x = Particular_Trust, y = fit)) +
  geom_line(colour = "steelblue", linewidth = 1) + 
  geom_ribbon(data = plot_data, aes(x = Particular_Trust, 
                                    ymin = lw, ymax = up),
              fill = "blue", alpha = 0.2) +
  labs(x = "Particular Trust", y = "Suicide Approval")  +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme(text = element_text(size = 20))

#Compute range of fitted values
min(plot_data$fit)
max(plot_data$fit)
max(plot_data$fit) - min(plot_data$fit)


#General Trust ===============================================
#Get fitted values 
plot_data <- mcmc_predict(mcmc_all, #model to predict with
                          params = params, #parameters to predict with
                          variable = "General_Trust", #predictor variable 
                          min_val = min(data$General_Trust), #predict using values from the minimum of general trust...
                          max_val = max(data$General_Trust), #... up to the maximum of general trust...
                          n_val = 100, #... in steps of 100
                          alpha = 0.01) #95% credible intervals

#Adjust trust values (undo mean-centring)
plot_data$General_Trust <- seq(1, 4, length.out = 100)


#Plot
ggplot(plot_data, aes(x = General_Trust, y = fit)) +
  geom_line(colour = "steelblue", linewidth = 1) + 
  geom_ribbon(data = plot_data, aes(x = General_Trust, 
                                    ymin = lw, ymax = up),
              fill = "blue", alpha = 0.2) +
  labs(x = "General Trust", y = "Suicide Approval")  +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme(text = element_text(size = 20))

#Compute range of fitted values
min(plot_data$fit)
max(plot_data$fit)
max(plot_data$fit) - min(plot_data$fit)


#Random effects ===========================================================

#Set up ===============================

#Get names of random effects for each trust variable
family_names <- grep("Family_Trust.Country", names(mcmc_all$estimates), value = TRUE)
particular_names <- grep("Particular_Trust.Country", names(mcmc_all$estimates), value = TRUE)
general_names <- grep("General_Trust.Country", names(mcmc_all$estimates), value = TRUE)

#Add random deviation to average effect to get country-specific effect
family_effects <- mcmc_all$estimates$Family_Trust + mcmc_all$estimates[family_names]
particular_effects <- mcmc_all$estimates$Particular_Trust + mcmc_all$estimates[particular_names]
general_effects <- mcmc_all$estimates$General_Trust + mcmc_all$estimates[general_names]

#Shorten column names to country names
names(family_effects) <- gsub("Family_Trust.Country.", "", names(family_effects))
names(particular_effects) <- gsub("Particular_Trust.Country.", "", names(particular_effects))
names(general_effects) <- gsub("General_Trust.Country.", "", names(general_effects))

#Select 10 countries for each trust varaible to repsent its variability across countries
family_countries <- select_clusters(mcmc_all, family_effects, n = 10)
particular_countries <- select_clusters(mcmc_all, particular_effects, n = 10)
general_countries <- select_clusters(mcmc_all, general_effects, 10)

#Clean country names
family_countries <- gsub("_", " ", family_countries)
particular_countries <- gsub("_", " ", particular_countries)
general_countries <- gsub("_", " ", general_countries)


#Family Trust ==============================================================

#Get fitted values 
plot_data <- mcmc_slopes(mcmc_all, #model for predictions
                         params = params, #parameters to predict with
                         variable = "Family_Trust", #predictor variable
                         cluster = "Country", #cluster variable
                         min_val = min(data$Family_Trust), #predict using values from the minimum of family trust...
                         max_val = max(data$Family_Trust), #... up to the maximum of family trust...
                         n_val = 100) #... in steps of 100

#Adjust trust values (undo mean-centring)
plot_data$Family_Trust <- seq(1, 4, length.out = 100) %>%
  map(function(x) rep(x, 99)) %>%
  unlist()

#Tidy up country names
plot_data$Country <- gsub("_", " ", plot_data$Country)

#Plot results
plot_data %>%
  filter(Country %in% family_countries) %>%
  ggplot(mapping = aes(x = Family_Trust, y = Fit, colour = Country)) +
  geom_line(linewidth = 1) +
  labs(x = "Family Trust", y = "Suicide Approval")  +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme(text = element_text(size = 20))


#Particular_Trust ============================================================

#Get fitted values 
plot_data <- mcmc_slopes(mcmc_all, #model for predictions
                         params = params, #parameters to predict with
                         variable = "Particular_Trust", #predictor variable
                         cluster = "Country", #cluster variable
                         min_val = min(data$Particular_Trust), #predict using values from the minimum of particular trust...
                         max_val = max(data$Particular_Trust), #... up to the maximum of particular trust...
                         n_val = 100) #... in steps of 100

#Adjust trust values (undo mean-centring)
plot_data$Particular_Trust <- seq(1, 4, length.out = 100) %>%
  map(function(x) rep(x, 99)) %>%
  unlist()

#Tidy up country names
plot_data$Country <- gsub("_", " ", plot_data$Country)

#Plot results
plot_data %>%
  filter(Country %in% particular_countries) %>%
  ggplot(mapping = aes(x = Particular_Trust, y = Fit, colour = Country)) +
  geom_line(linewidth = 1) +
  labs(x = "Particular Trust", y = "Suicide Approval")  +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme(text = element_text(size = 20))


#General Trust ===============================================================

#Get fitted values 
plot_data <- mcmc_slopes(mcmc_all, #model for predictions
                         params = params,  #parameters to predict with
                         variable = "General_Trust", #predictor variable
                         cluster = "Country",  #cluster variable
                         min_val = min(data$General_Trust), #predict using values from the minimum of particular trust...
                         max_val = max(data$General_Trust), #... up to the maximum of particular trust...
                         n_val = 100) #... in steps of 100

#Adjust trust values (undo mean-centring)
plot_data$General_Trust <- seq(1, 4, length.out = 100) %>%
  map(function(x) rep(x, 99)) %>%
  unlist()

#Tidy up country names
plot_data$Country <- gsub("_", " ", plot_data$Country)

#Plot results
plot_data %>%
  filter(Country %in% general_countries) %>%
  ggplot(mapping = aes(x = General_Trust, y = Fit, colour = Country)) +
  geom_line(linewidth = 1) +
  labs(x = "General Trust", y = "Suicide Approval")  +
  scale_y_continuous(breaks = 1:10, limits = c(1, 10)) +
  theme(text = element_text(size = 20))
