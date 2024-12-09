#Prep WV7 =================

#Read in data
WV7 <- readRDS("EVS_WVS_Joint_Rds_v4_0.rds")

#Select relevant variables
WV7 <- select(WV7, f123, 
              d001_b, g007_18_b, g007_33_b,
              g007_34_b, g007_35_b, g007_36_b,
              x007, x011,
              x001, x003, x025a_01,
              f028, f034,
              a170, a173,
              f118, f120, f121,
              c001, d059, d060, 
              a029, a034, a040, a042,
              cntrycow, year)

#Rename variables 
names(WV7) <- c("Suicide", "Family_Trust", "Neighbour_Trust",
                "Personal_Trust", "Stranger_Trust",
                "Religion_Trust", "National_Trust",
                "Marital", "Children",
                "Sex", "Age", "Education",
                "Attendance", "Religious",
                "Satisfied", "Control",
                "Homosexuality", "Abortion", "Divorce",
                "Men_Jobs", "Men_Pol", "Men_Uni",
                "Independence", "Imagine", "Faith", "Obedience",
                "COW", "Year")


#Recode NAs
WV7[WV7 == -5] <- NA
WV7[WV7 == -4] <- NA
WV7[WV7 == -3] <- NA
WV7[WV7 == -2] <- NA
WV7[WV7 == -1] <- NA


#Store unique COW codes for each country 
COW_codes <- as.character(unique(WV7$COW))
COW_codes

#Store country names in order of COW codes
countries <- c("Albania", "Azerbaijan", "Austria", "Armenia", "Bosnia_Herzegovina", "Bulgaria", "Belarus",
               "Croatia", "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", "Germany",
               "Hungary", "Iceland", "Italy", "Latvia", "Lithuania", "Montenegro", "Netherlands", "Norway",
               "Poland", "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain", 
               "Sweden", "Switzerland", "Ukraine", "North_Macedonia", "UK", "Andorra", "Argentina",
               "Australia", "Bangladesh", "Bolivia", "Brazil", "Canada", "Colombia", "Cyprus", "Chile", 
               "China", "Ecuador", "Egypt", "Ethiopia", "Greece", "Guatemala", "Hong_Kong", "Indonesia",
               "Iran", "Iraq", "Japan", "Jordan", "Kazakhstan", "Kenya", "Kyrgyzstan", "Lebanon", "Libya",
               "Macau", "Malasia", "Maldives", "Mexico", "Mongolia", "Morocco", "Myanmar", "New_Zealand",
               "Nicaragua", "Nigeria", "Northern_Ireland", "Pakistan", "Peru", "Philippines", "Puerto_Rico",
               "Singapore", "South_Korea", "Taiwan", "Tajikistan", "Thailand", "Tunisia", "Turkey", "USA",
               "Uruguay", "Venezuela", "Vietnam", "Zimbabwe")

#Create country variable based on COW codes
WV7 <- WV7 %>%
  mutate(Country = factor(COW, levels = COW_codes, labels = countries))

table(WV7$Country, useNA = "a")


#Recode education variable
WV7$EduCAT <- factor(WV7$Education, levels = 0:8,
                     labels = c("Below_Primary",
                                "Below_Secondary", "Below_Secondary",
                                "Below_University", "Below_University", "Below_University",
                                "University", "University", "University"))



#Recode variables for self-expression index
WV7$Men_Jobs <- car::recode(WV7$Men_Jobs, "1=1;2=3;3=2")
WV7$Faith <- car::recode(WV7$Faith, "0=1;1=0")
WV7$Obedience <- car::recode(WV7$Obedience, "0=1;1=0")


WV7$Homosexuality <- (WV7$Homosexuality - 1)/9
WV7$Abortion <- (WV7$Abortion - 1)/9
WV7$Divorce <- (WV7$Divorce - 1)/9

WV7$Men_Jobs <- (WV7$Men_Jobs - 1)/2
WV7$Men_Pol <- (WV7$Men_Pol - 1)/3
WV7$Men_Uni <- (WV7$Men_Uni - 1)/3

#Ceheck recodes have been applied
summary(WV7)

#Dummy out religious person
WV7$Religious <- ifelse(WV7$Religious == 1, 1, 0)

#Add indicator variable for wave
WV7$Wave <- "Wave_7"