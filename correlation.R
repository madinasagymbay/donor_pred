setwd("/Users/zhengjingyi/Desktop")
projects.df <- read.csv("projects.csv")
#Drop categorical variables and dependent column
projects1.df <- projects.df[ , -c(1, 2, 3, 4, 5, 14, 15, 16, 17, 18)]
#check if there is missing value 
mean(projects1.df$vendor_shipping_charges) 
mean(projects1.df$vendor_shipping_charges,na.rm=T)
#create a column named as "MissingData", and mark the records with missing value as "True" (to find all the records with missing value)
projects1.df$MissingData=is.na(projects1.df$vendor_shipping_charges)
projects1.df$MissingData=is.na(projects1.df$vendor_shipping_charges) | is.na(projects1.df$sales_tax) | is.na(projects1.df$payment_processing_charges) | is.na(projects1.df$total_price_excluding_optional_support) | is.na(projects1.df$total_price_including_optional_support) | is.na(projects1.df$Optional_support) | is.na(projects1.df$students_reached) | is.na(projects1.df$num_donors) | is.na(projects1.df$Duration)
# delete records which have missing value
library(dplyr)
CleanData_Cor = filter(projects1.df, MissingData != TRUE)
# Drop "MissingData" column
CleanData_Cor <- CleanData_Cor[ , -c(10)] 
#plot correlation
library(metan)
corr1 <- corr_coef(CleanData_Cor)
plot(corr1)

cor(CleanData_Cor)




