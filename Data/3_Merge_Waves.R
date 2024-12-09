#Merge data ==================

#Run scripts for preparing wave 6 and 7
source("1_Prepare_Wave6.R")
source("2_Prepare_Wave7.R")

#Combine waves 6 and 7
data <- rbind(WV6, WV7)

#Save data
write.csv(data, file = "WVS_6_7_280723.csv")

#Test reading in data
data <- read.csv("WVS_6_7_280723.csv")
