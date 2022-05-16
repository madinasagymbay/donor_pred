rm(list=ls())

setwd("~/Desktop")

library(caret)
library(neuralnet)

projects.df = read.csv("Projects_Cleandata_other_models.csv")
projects1.df = projects.df[, -c(1)]

projects1.df$Success <- projects1.df$funding_status=="1"
projects1.df$Unsuccess <- projects1.df$funding_status=="0"

# we always use validation dataset to evaluate the performance of the model built based on training data
# reason: it does not make sense if we use training data to evaluate model performance because they are exactly the data building the model
# why we also construct confusion matrix before using training data:
# show potential overfitting problem (high performance among training; low performance among validation)

set.seed(5)
train.index <- sample(row.names(projects1.df), 0.6*dim(projects1.df)[1])  
valid.index <- setdiff(row.names(projects1.df), train.index)  
train.df <- projects1.df[train.index, ]
valid.df <- projects1.df[valid.index, ]


train.norm.df <- train.df
valid.norm.df <- valid.df
projects1.norm.df<-projects1.df

#The preProcess option "range" scales the data to the interval between zero and one.
norm.values <- preProcess(train.df[, c(1,2,3,4,5,6,7,8,10)], method=c("range")) 
train.norm.df[, c(1,2,3,4,5,6,7,8,10)] <- predict(norm.values, train.df[, c(1,2,3,4,5,6,7,8,10)])
valid.norm.df[, c(1,2,3,4,5,6,7,8,10)] <- predict(norm.values, valid.df[, c(1,2,3,4,5,6,7,8,10)])
projects1.norm.df <- predict(norm.values, projects1.df)

################################################################################################################
# train NN using training data (rep=1, hidden=9)
nn <- neuralnet(Success + Unsuccess ~ resource_usage + resource_type + 
                  poverty_level + grade_level + payment_processing_charges + 
                  total_price_excluding_optional_support + 
                  students_reached + num_donors + Duration, 
                data = train.norm.df, linear.output = F, 
                act.fct = 'logistic', rep=1, hidden =9,
                stepmax = 30000000)


nn$weights
prediction(nn)
plot(nn)
plot(nn, rep="best")

# evaluate model performance using validation data
predict <- compute(nn, data.frame(valid.norm.df$resource_usage, valid.norm.df$resource_type, 
                                  valid.norm.df$poverty_level, valid.norm.df$grade_level,
                                  valid.norm.df$payment_processing_charges, 
                                  valid.norm.df$total_price_excluding_optional_support,
                                  valid.norm.df$students_reached, valid.norm.df$num_donors,
                                  valid.norm.df$Duration))

predicted.class=apply(predict$net.result,1,which.max)
predicted.class

valid.norm.df$funding_status <- factor(valid.norm.df$funding_status, levels = c(1, 0), 
                                       labels = c("Success", "Unsuccess")) 

a <- factor(ifelse(predicted.class=="1", "Success", "Unsuccess"))
confusionMatrix(a, valid.norm.df$funding_status)
#############################################################################################################
# train NN using training data (rep=3, hidden=9)
nn <- neuralnet(Success + Unsuccess ~ resource_usage + resource_type + 
                  poverty_level + grade_level + payment_processing_charges + 
                  total_price_excluding_optional_support + 
                  students_reached + num_donors + Duration, 
                data = train.norm.df, linear.output = F, 
                act.fct = 'logistic', rep=3, hidden =9,
                stepmax = 30000000)


nn$weights
prediction(nn)
plot(nn)
plot(nn, rep="best")

# evaluate model performance using validation data
predict <- compute(nn, data.frame(valid.norm.df$resource_usage, valid.norm.df$resource_type, 
                                  valid.norm.df$poverty_level, valid.norm.df$grade_level,
                                  valid.norm.df$payment_processing_charges, 
                                  valid.norm.df$total_price_excluding_optional_support,
                                  valid.norm.df$students_reached, valid.norm.df$num_donors,
                                  valid.norm.df$Duration))

predicted.class=apply(predict$net.result,1,which.max)
predicted.class

valid.norm.df$funding_status <- factor(valid.norm.df$funding_status, levels = c(1, 0), 
                                       labels = c("Success", "Unsuccess")) 

a <- factor(ifelse(predicted.class=="1", "Success", "Unsuccess"))
confusionMatrix(a, valid.norm.df$funding_status)

#########################################################################################################
# train NN using training data (rep=10, hidden=3)
nn <- neuralnet(Success + Unsuccess ~ resource_usage + resource_type + 
                  poverty_level + grade_level + payment_processing_charges + 
                  total_price_excluding_optional_support + 
                  students_reached + num_donors + Duration, 
                data = train.norm.df, linear.output = F, 
                act.fct = 'logistic', rep=10, hidden =3,
                stepmax = 30000000)

nn$weights
prediction(nn)
plot(nn)
plot(nn, rep="best")

# evaluate model performance using validation data
predict <- compute(nn, data.frame(valid.norm.df$resource_usage, valid.norm.df$resource_type, 
                                  valid.norm.df$poverty_level, valid.norm.df$grade_level,
                                  valid.norm.df$payment_processing_charges, 
                                  valid.norm.df$total_price_excluding_optional_support,
                                  valid.norm.df$students_reached, valid.norm.df$num_donors,
                                  valid.norm.df$Duration))

predicted.class=apply(predict$net.result,1,which.max)
predicted.class

valid.norm.df$funding_status <- factor(valid.norm.df$funding_status, levels = c(1, 0), 
                                       labels = c("Success", "Unsuccess")) 

a <- factor(ifelse(predicted.class=="1", "Success", "Unsuccess"))
confusionMatrix(a, valid.norm.df$funding_status)

#ROC
y_act <- valid.norm.df$funding_status

library(pROC, warn.conflicts=F)
roc1<-roc(y_act,as.integer(a[1:length(y_act)])-1)
plot(roc1,print.auc=TRUE,plot=TRUE,print.thres=TRUE)



