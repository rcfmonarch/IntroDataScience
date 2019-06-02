#--------------------------------------------------------
# Module 3 Data Wrangling Ex-01
# Rob Fontenot
# rcfmonarch@gmail.com
# https://github.com/rcfmonarch/IntroDataScience
# 2019-05-27
#--------------------------------------------------------

library(dplyr)
library(tidyr)

# Step 0: Load data set into RStudio
data <- read.csv("D:\\Hermetic-Code\\R\\IntroDataScience\\
                 Module-03-Data-Wrangle-Ex-01-Refine-Original.csv")
data

# Step 1: Clean up Brand Names
data$companyfix <- sapply(data$ï..company, tolower)

for(i in 1:length(data$companyfix)){
  if (data$companyfix[i] == "ak zo" 
      | data$companyfix[i] == "akz0") {
    data$companyfix[i] = "akzo"
  }
  if (data$companyfix[i] == "phillips" 
      | data$companyfix[i] == "fillips"
      | data$companyfix[i] == "phllips"
      | data$companyfix[i] == "phillps"
      | data$companyfix[i] == "phlips") {
    data$companyfix[i] = "philips"
  }
  if (data$companyfix[i] == "unilver") {
    data$companyfix[i] = "unilever"
  }
}
data$companyfix

# Step 2:  Separate Product Code and Number
datafix <- data %>% 
  separate(Product.code...number, c("product_code", "product_number"), remove=FALSE)
datafix

# Step 3: Add Product Categories
datafix2 <- datafix %>% 
  mutate("product_category" = case_when (
    datafix$product_code == "p" ~ "Smartphone",
    datafix$product_code == "q" ~ "Tablet",
    datafix$product_code == "v" ~ "TV", 
    datafix$product_code == "x" ~ "Laptop",
    TRUE ~ datafix$product_code
    ))
datafix2

# Step 4: Add full address for geocoding
datafix3 <- datafix2 %>% 
  mutate("full_address" = paste(address, city, country, sep=","))
datafix3

# Step 5: Create Dummary Variables for Company and Product Category
datafix4 <- datafix3 %>% 
  mutate("company_philips" = ifelse(companyfix == "philips",1,0)) %>% 
  mutate("company_akzo" = ifelse(companyfix == "akzo",1,0)) %>% 
  mutate("company_van_houten" = ifelse(companyfix == "van houten",1,0)) %>% 
  mutate("company_unilever" = ifelse(companyfix == "unilever",1,0)) %>% 
  mutate("product_smartphone" = ifelse(product_category == "Smartphone",1,0)) %>% 
  mutate("product_tv" = ifelse(product_category == "TV",1,0)) %>% 
  mutate("product_laptop" = ifelse(product_category == "Laptop",1,0)) %>% 
  mutate("product_tablet" = ifelse(product_category == "Tablet",1,0))
  
datafix4

write.csv(datafix4, file="Module-03-Data-Wrangle-Ex-01-Refine-Clean.csv", row.names=FALSE)