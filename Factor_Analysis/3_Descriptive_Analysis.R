#Descriptive analyses =========================================================

#Run set up script
source("1_Set_Up.R")

#Summary statistics ===========================================================

#Get basic summary statistics
summary(data)

#Store variable names
var_names <- names(data)

#Get names of integer/numeric variable
num_vars <- get_class(data, of_class = c("numeric", "integer"))

#Compute modes, standard deviations and IQRs
map(var_names, function(x) get_mode(data[[x]])) %>% set_names(var_names)
map(num_vars, function(x) IQR(data[[x]], na.rm = T)) %>% set_names(num_vars)
sd(data$Age, na.rm = TRUE)

#Graphing data =================================================================

#Get counts of observations per response option for each trust variable
trust_counts <- map(trust_vars, function(x) table(data[[x]])) %>% set_names(trust_vars)

#Convert list to data frame
trust_counts <- list2DF(trust_counts)

#Add variable for response option
trust_counts$cat <- rownames(trust_counts)

#Stack trust variables on top of one another 
trust_counts <- pivot_longer(trust_counts, cols = all_of(trust_vars), names_to = "trust")

#Remove _ from trust variable names
trust_counts$trust <- gsub("_", " ", trust_counts$trust)

#Convert trust variable names to factor
trust_counts$trust <- factor(trust_counts$trust, 
                             levels = unique(trust_counts$trust),
                             labels = unique(trust_counts$trust))

#Plot results
ggplot(data = trust_counts, aes(y = value, x = cat)) +
  geom_bar(stat = "identity") +
  labs(x = "Trust Score", y = "Count",
       caption = paste("n =", nrow(data), "\nSource: WVS, Waves 6 and 7")) + 
  facet_wrap(~ trust) +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.caption = element_text(size = 15))


#Correlations and Cronbach's Alpha =============================================

#Get Pearson and Polychoric correlation matrices
cor_data <- map(.x = c(Metrical = "Pearson", Ordinal = "Polychoric"), 
                function(x) cor_mx(data, trust_vars, type = x))

#Inspect correlation matrices
cor_data


#Calculate Cronbach's alpha using scaled (standardized) items
cronbach_alpha <- psych::alpha(scale(data[trust_vars]))

#Inspeact results
alpha_tab(cronbach_alpha)
