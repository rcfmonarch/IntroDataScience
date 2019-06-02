#--------------------------------------------------------
# Module 3 Data Wrangling Ex-02
# Rob Fontenot
# rcfmonarch@gmail.com
# https://github.com/rcfmonarch/IntroDataScience
# 2019-06-02
#--------------------------------------------------------


library(dplyr)
library(tidyr)

# Step 0: Load data set into RStudio
titanicdata <- read.csv("D:\\Hermetic-Code\\R\\IntroDataScience\\Module-03-Data-Wrangle-Ex-02-titanic_original.csv")
glimpse(titanicdata) 

# 1: Port of Embarkation
# Replace missing embarcation values with 'S'
titanicdata$embarked[which(titanicdata$embarked %in% c(""))] <- "S"
print(titanicdata$embarked)

# 2: Age
# Calculate the mean of the age column and use that to populate
# missing age values
print(titanicdata$age)
titanicdata$age[which(is.na(titanicdata$age))] <- mean(titanicdata$age, na.rm=TRUE)
print(titanicdata$age)

# 3: Lifeboat
# Fill missing boat values with "None" or "NA"
print(titanicdata$boat)
titanicdata$boat[titanicdata$boat == ""] <- NA
print(titanicdata$boat)

# 4: Cabin Number
# Create a new column with a "1" if a cabin number exists
# or "0" if it does not
titanicdata$cabin
titanicdata <- titanicdata %>% 
  mutate("has_cabin_number" = ifelse(cabin == "",0,1))
View(titanicdata)