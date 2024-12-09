#EFA Models ========================================================

#Run set up script
source("1_Set_Up.R")


#Model 1 =======================================================

#Conduct factor analyses for 1 factor
fa_1 <- map(.x = list(Metrical = "cor", Ordinal = "poly"), 
            function(x) fa(data[trust_vars], nfactors = 1, rotate = "none", cor = x, fm = "ml"))

#Inspect results
map(fa_1, function(x) fa_table(x))

#Model 2 =======================================================

#Conduct factor analyses for 2 factors
fa_2 <- map(.x = list(Metrical = "cor", Ordinal = "poly"), 
            function(x) fa(data[trust_vars], nfactors = 2, rotate = "oblimin", cor = x, fm = "ml"))

#Inspect results
map(fa_2, function(x) fa_table(x))

#Inspect structure matrix
map(fa_2, function(x) structure_mx(x))

#Model 3 =======================================================

#Get names of trust variables excluding family trust
not_family <- trust_vars[!grepl("Family", trust_vars)]

#Conduct factor analyses for 2 factors, excluding family trust
fa_3 <- map(.x = list(Metrical = "cor", Ordinal = "poly"), 
            function(x) fa(data[not_family], nfactors = 2, rotate = "oblimin", cor = x, fm = "ml"))

#Inspect results
map(fa_3, function(x) fa_table(x))

#Inspect structure matrix
map(fa_3, function(x) structure_mx(x))

#Model 4 =======================================================

#Conduct factor analyses for 3 factors
fa_4 <- map(.x = list(Metrical = "cor", Ordinal = "poly"), 
            function(x) fa(data[trust_vars], nfactors = 3, rotate = "oblimin", cor = x, fm = "ml"))

#Inspect results
map(fa_4, function(x) fa_table(x))

#Inspect structure matrix
map(fa_4, function(x) structure_mx(x))
