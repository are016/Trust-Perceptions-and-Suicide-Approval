#PCA =========================================================================

#Run set up script
source("1_Set_Up.R")

#Eigen values ================================================================


#Get Pearson and Polychoric correlation matrices
pers_mx <-cor(data[trust_vars])
poly_mx <- polychoric(data[trust_vars])$rho

#Extract eigen values from both matrices
eigen_vals <- map(list(Metrical = pers_mx, Ordinal = poly_mx), 
                  function(x) eigen(x)$values)

#Calculate PCA statistics
map(eigen_vals, function(x) eigen_table(x))


#Prepare eigen value data for plotting =======================================

#Convert to data frame
eigen_metr <- as.data.frame(eigen_vals$Metrical)
eigen_ord <- as.data.frame(eigen_vals$Ordinal)

#Rename columns
names(eigen_metr) <- "value"
names(eigen_ord) <- "value"

#Add component variable
eigen_metr$n_comp <- 1:6
eigen_ord$n_comp <- 1:6

#Stack metrical and ordinal data
plot_data <- rbind(eigen_metr, eigen_ord)

#Create variable for type of analysis: metrical or ordinal
plot_data$type <- c(rep("Metrical", 6), rep("Ordinal", 6))


#Plot eigen values for metrical model
plot_data %>%
  filter(type == "Metrical") %>%
  ggplot(mapping = aes(x = n_comp, y = value)) +
  geom_line() +
  geom_point(size = 3) +
  geom_hline(yintercept = 1, linetype = "dashed") +
  scale_y_continuous(breaks = seq(0, 3, 1)) +
  scale_x_continuous(breaks = seq(1, 6, 1)) +
  labs(caption = paste("N =", nrow(data)),
       x = "Number of components", y = "Eigen Value") +
  theme(axis.title = element_text(size = 20),
        axis.text = element_text(size = 15),
        plot.caption = element_text(size = 15))


#Run MAP tests
min_partial <- map(list(Metrical = "cor", Ordinal = "poly"),
                   function(x) vss(data[trust_vars], fm = "ml", cor = x, plot = FALSE))

#Present MAP results
map_table(min_partial$Metrical, n_factors = 3)
map_table(min_partial$Ordinal, n_factors = 3)
