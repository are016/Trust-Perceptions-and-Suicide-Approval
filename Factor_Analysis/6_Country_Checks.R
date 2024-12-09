#Country checks ===============================================================

#Run set up script
source("1_Set_Up.R")


#Run Checks ==================================================================

#Get names of trust variables excluding family trust
not_family <- trust_vars[!grepl("Family", trust_vars)]

#Get country names
countries <- unique(data$Country)

#Run 2 factor model per country
country_fa <- map(countries, 
                  function(x) fa(data[data$Country == x, not_family], nfactors = 2, rotate = "oblimin", cor = "cor", fm = "ml"))



#Name list elements after countries
names(country_fa) <- countries


#Extract loadings per country
country_loads <- map(country_fa, 
                     function(x) structure(.Data = c(x[["loadings"]]),
                                           .Dim = c(5, 2)))


#Set up patterns of TRUE (absolute loading > 0.3) and FALSE (absolute loading < 0.3) corresponding to 
#factor structure in Model 3

#Variant 1 - Factor 1 = General Trust, Factor 2 = Particular Trust
variant_1 <- structure(.Data = c(FALSE, FALSE, TRUE, TRUE, TRUE, #Stranger, religious and national trust load above 0.3
                                 TRUE, TRUE, TRUE, FALSE, FALSE), #neighbour, personal and stranger trust load above 0.3
                       .Dim = c(5, 2)) 


#Variant 2 - Factor 1 = Particular Trust, Factor 2 = General Trust
variant_2 <- cbind(variant_1[,2], variant_1[,1]) #Reverse ordering of columns in variant_1


#Check for variants
structure_1 <- verify_structure(country_loads, factor_str = variant_1, cutoff = 0.3)
structure_2 <- verify_structure(country_loads, factor_str = variant_2, cutoff = 0.3)


#Count number of countries where structure was observed
length(structure_1$observed) + length(structure_2$observed)

#Get all countries where structure was observed
match_countries <- c(structure_1$observed, structure_2$observed)

#Inspect countries where structure was observed
match_countries
map(country_fa[match_countries], fa_table)


#Get all countries where structure was not observed
mismatch_countries <- countries[!countries %in% match_countries]

#Inspect countries where structure was not observed
mismatch_countries
map(country_fa[mismatch_countries], fa_table)
