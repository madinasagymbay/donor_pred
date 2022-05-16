setwd("/Users/zhengjingyi/Desktop")
projects.df <- read.csv("projects.csv")
#Drop categorical variables and dependent column
projects1.df <- projects.df[ , -c(1, 2, 3, 4, 5, 14, 15, 16, 17, 18)]  
# check if there is missing value in the dataset
mean(projects1.df$vendor_shipping_charges)
mean(projects1.df$vendor_shipping_charges,na.rm=T)
#create a column named as "MissingData", and mark the records with missing value as "True" (to find all the records with missing value)
projects1.df$MissingData=is.na(projects1.df$vendor_shipping_charges)
projects1.df$MissingData=is.na(projects1.df$vendor_shipping_charges) | is.na(projects1.df$sales_tax) | is.na(projects1.df$payment_processing_charges) | is.na(projects1.df$total_price_excluding_optional_support) | is.na(projects1.df$total_price_including_optional_support) | is.na(projects1.df$Optional_support) | is.na(projects1.df$students_reached) | is.na(projects1.df$num_donors) | is.na(projects1.df$Duration)
# delete records which have missing value
library(dplyr)
CleanData_Outlier = filter(projects1.df, MissingData != TRUE)
# Drop "MissingData" column and the columns which are decided to delete after checking the correlation heatmap (see code:correlation.R)
CleanData_Outlier1 <- CleanData_Outlier[ , -c(1, 2, 5, 6, 10)]  # Drop "missing value" column

#make the five plots in the same page
par(mfrow=c(1,5))
#plot outliers for 5 numerical vaiables using boxplot
boxplot(CleanData_Outlier1$payment_processing_charges)$out
boxplot(CleanData_Outlier1$total_price_excluding_optional_support)$out
boxplot(CleanData_Outlier1$students_reached)$out
boxplot(CleanData_Outlier1$num_donors)$out
boxplot(CleanData_Outlier1$Duration)$out

#as shown in the boxpots, there are a lot of outliers in each variables 
#The interquartile range (IQR) method is used here to delete outliers
#If an observation is 1.5 times the interquartile range more than the third quartile (Q3) or 1.5 times the interquartile range less than the first quartile (Q1), it is considered an outlier (Q1).
summary(CleanData_Outlier1$payment_processing_charges)
IQR_payment_processing_charges=8.380-3.480
upfen_payment_processing_charges=8.380+1.5*IQR_payment_processing_charges
upfen_payment_processing_charges

summary(CleanData_Outlier1$total_price_excluding_optional_support)
IQR_total_price_excluding_optional_support=527-253
upfen_total_price_excluding_optional_support=527+1.5*IQR_total_price_excluding_optional_support
upfen_total_price_excluding_optional_support

summary(CleanData_Outlier1$students_reached)
IQR_students_reached=90.00-22.00
upfen_students_reached=90+1.5*IQR_students_reached
upfen_students_reached

summary(CleanData_Outlier1$num_donors)
IQR_num_donors=5.14-2
upfen_num_donors=5.14+1.5*IQR_num_donors
upfen_num_donors

summary(CleanData_Outlier1$Duration)
IQR_Duration=155-149
upfen_Duration=155+1.5*IQR_Duration
#delete outliers for the first time
summary(CleanData_Outlier1)
CleanData2=subset(CleanData_Outlier1, Duration<=164 & num_donors<=9.85 & payment_processing_charges<=15.73 & students_reached<=192 & total_price_excluding_optional_support<=938) 
#plot the outliers for the rest of data
par(mfrow=c(1,5))
boxplot(CleanData2$payment_processing_charges)$out
boxplot(CleanData2$total_price_excluding_optional_support)$out
boxplot(CleanData2$students_reached)$out
boxplot(CleanData2$num_donors)$out
boxplot(CleanData2$Duration)$out
#as shown in the boxplots, there's still a lot of outliers in 5 numerical variables
#repeat the IQR method again
summary(CleanData2$payment_processing_charges)
IQR_payment_processing_charges=7.040-3.060
upfen_payment_processing_charges=7.040+1.5*IQR_payment_processing_charges
upfen_payment_processing_charges

summary(CleanData2$total_price_excluding_optional_support)
IQR_total_price_excluding_optional_support=473.83-232
upfen_total_price_excluding_optional_support=473.83+1.5*IQR_total_price_excluding_optional_support
upfen_total_price_excluding_optional_support

summary(CleanData2$students_reached)
IQR_students_reached=60-21
upfen_students_reached=60+1.5*IQR_students_reached
upfen_students_reached

summary(CleanData2$num_donors)
IQR_num_donors=4-2
upfen_num_donors=4+1.5*IQR_num_donors
upfen_num_donors

summary(CleanData2$Duration)
IQR_Duration=154-149
upfen_Duration=154+1.5*IQR_Duration
upfen_Duration
#delete outliers for the second time
summary(CleanData2)
CleanData3=subset(CleanData2, Duration<=161.5 & num_donors<=7 & payment_processing_charges<=13.01 & students_reached<=118.5 & total_price_excluding_optional_support<=836.575) 
##plot the outliers for the rest of data
par(mfrow=c(1,5))
boxplot(CleanData3$payment_processing_charges)$out
boxplot(CleanData3$total_price_excluding_optional_support)$out
boxplot(CleanData3$students_reached)$out
boxplot(CleanData3$num_donors)$out
boxplot(CleanData3$Duration)$out
#as shown in the boxplots, there's still a lot of outliers in variable "Duration"
summary(CleanData3$Duration)
IQR_Duration=154-149
upfen_Duration=154+1.5*IQR_Duration
lowfen_Duration=149-1.5*IQR_Duration
#delete outliers for the third time
CleanData4=subset(CleanData3, Duration<=161.5 & Duration>=141.5 & num_donors<=7 & payment_processing_charges<=13.245 & students_reached<=120 & total_price_excluding_optional_support<=719.565) 
##plot the outliers for the rest of data
par(mfrow=c(1,5))
boxplot(CleanData4$payment_processing_charges)$out
boxplot(CleanData4$total_price_excluding_optional_support)$out
boxplot(CleanData4$students_reached)$out
boxplot(CleanData4$num_donors)$out
boxplot(CleanData4$Duration)$out
#based on the plot, there's not too much outliers now, so we stopped repeating IQR method

