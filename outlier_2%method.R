setwd("/Users/zhengjingyi/Desktop")
projects.df <- read.csv("projects.csv")
# Drop insignificant and useless columns (Categorical variables are kept this time since we want to generate a clean datasets which can be directly used in modeling)
projects1.df <- projects.df[ , -c(6, 7, 10, 11, 15, 16, 17, 18)]  

# drop obs with missing values
CleanData_Outlier1 <- na.omit(projects1.df)

#replace 2% extreme value for the variable "payment_processing_charges" as "NA"
pct_V1_2 <- quantile(CleanData_Outlier1$payment_processing_charges, c(0.02, 0.98), type = 1)
CleanData_Outlier1["payment_processing_charges"][CleanData_Outlier1["payment_processing_charges"] <= pct_V1_2[1]] <- NA
CleanData_Outlier1["payment_processing_charges"][CleanData_Outlier1["payment_processing_charges"] >= pct_V1_2[2]] <- NA

#replace 2% extreme value for the variable "total_price_excluding_optional_support" as "NA"
pct_V2_2 <- quantile(CleanData_Outlier1$total_price_excluding_optional_support, c(0.02, 0.98), type = 1)
CleanData_Outlier1["total_price_excluding_optional_support"][CleanData_Outlier1["total_price_excluding_optional_support"] <= pct_V2_2[1]] <- NA
CleanData_Outlier1["total_price_excluding_optional_support"][CleanData_Outlier1["total_price_excluding_optional_support"] >= pct_V2_2[2]] <- NA

#replace 2% extreme value for the variable "students_reached" as "NA"
pct_V3_2 <- quantile(CleanData_Outlier1$students_reached, c(0.02, 0.98), type = 1)
CleanData_Outlier1["students_reached"][CleanData_Outlier1["students_reached"] <= pct_V3_2[1]] <- NA
CleanData_Outlier1["students_reached"][CleanData_Outlier1["students_reached"] >= pct_V3_2[2]] <- NA

#replace 2% extreme value for the variable "num_donors" as "NA"
pct_V4_2 <- quantile(CleanData_Outlier1$num_donors, c(0.02, 0.98), type = 1)
CleanData_Outlier1["num_donors"][CleanData_Outlier1["num_donors"] <= pct_V4_2[1]] <- NA
CleanData_Outlier1["num_donors"][CleanData_Outlier1["num_donors"] >= pct_V4_2[2]] <- NA

#replace 2% extreme value for the variable "Duration" as "NA"
pct_V5_2 <- quantile(CleanData_Outlier1$Duration, c(0.02, 0.98), type = 1)
CleanData_Outlier1["Duration"][CleanData_Outlier1["Duration"] <= pct_V5_2[1]] <- NA
CleanData_Outlier1["Duration"][CleanData_Outlier1["Duration"] >= pct_V5_2[2]] <- NA

#drop records with "NA" value
CleanData_Outlier2 <- na.omit(CleanData_Outlier1) # drop obs with missing values

#export the dataset which can be directly used in modeling, named as "Projects_Cleandata_other_models.csv"
write.table(CleanData_Outlier2,file="/Users/zhengjingyi/Desktop/Projects_Cleandata_other_models.csv", sep = ",", row.names = FALSE)
