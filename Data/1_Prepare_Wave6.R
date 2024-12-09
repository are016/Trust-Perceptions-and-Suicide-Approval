#Load packages
require(tidyverse)

#Prep WV6 =========================

#Load in the data
load("WV6_Data_R_v20201117.rdata")

#Select relevant variables under new data object
WV6 <- select(WV6_Data_R_v20201117, V207, 
              V102, V103, V104, V105, V106, V107,
              V57, V58,
              V240, V242, V248,
              V145, V147,
              V23, V55,
              V203, V204, V205,
              V45, V51, V52, 
              V12, V15, V19, V21,
              cow, V262)

#Remove massive file with long name
rm(WV6_Data_R_v20201117)

#Rename variables
names(WV6) <- c("Suicide", "Family_Trust", "Neighbour_Trust",
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

#Extract unique correlates of ward (COW) cods for each country
COW_codes <- as.character(unique(WV6$COW))
COW_codes

#Store names of countries in order of COW codes
countries <- c("Algeria", "Argentina", "Armenia", "Australia", "Azerbaijan", "Belarus", "Brazil",
               "Colombia", "Cyprus", "Chile", "China", "Ecuador", "Egypt", "Estonia", "Georgia",
               "Germany", "Ghana", "Haiti", "Hong_Kong", "India", "Iraq", "Japan", "Jordan", "Kazakhstan",
               "Kuwait", "Kyrgyzstan", "Lebanon", "Libya", "Malasia", "Mexico", "Morocco", "Netherlands",
               "New_Zealand", "Nigeria", "Pakistan", "Palestine", "Peru", "Philippines", "Poland",
               "Qatar", "Romania", "Russia", "Rwanda", "Singapore", "Slovenia", "South_Korea",
               "South_Africa", "Spain", "Sweden", "Taiwan", "Thailand", "Trinidad", "Tunisia",
               "Turkey", "Ukraine", "USA", "Uruguay", "Uzbekistan", "Yemen", "Zimbabwe")

#Create country variable based on COW codes
WV6 <- WV6 %>%
  mutate(Country = factor(COW, levels = COW_codes, labels = countries))

table(WV6$Country)


#Recode children so 5 = 5+
WV6$Children <- ifelse(WV6$Children >= 5,
                       5,
                       WV6$Children) 



#Recode education variable
WV6$EduCAT <- factor(WV6$Education, levels = 1:9,
                     labels = c("Below_Primary", "Below_Primary",
                                "Below_Secondary", "Below_Secondary",
                                "Below_University", "Below_Secondary",
                                "Below_University", "Below_University",
                                "University"))


#Recode variables for self-expressionism index
WV6$Independence <- car::recode(WV6$Independence, "1=1;2=0")
WV6$Imagine <- car::recode(WV6$Imagine, "1=1;2=0")
WV6$Faith <- car::recode(WV6$Faith, "1=0;2=1")
WV6$Obedience <- car::recode(WV6$Obedience, "1=0;2=1")


WV6$Homosexuality <- (WV6$Homosexuality - 1)/9
WV6$Abortion <- (WV6$Abortion - 1)/9
WV6$Divorce <- (WV6$Divorce - 1)/9

WV6$Men_Jobs <- (WV6$Men_Jobs - 1)/2
WV6$Men_Pol <- (WV6$Men_Pol - 1)/3
WV6$Men_Uni <- (WV6$Men_Uni - 1)/3

#Check that recodes have been applied
summary(WV6)

#Dummy out religious person
WV6$Religious <- ifelse(WV6$Religious == 1, 1, 0)

#Add indicator variable for wave
WV6$Wave <- "Wave_6"

