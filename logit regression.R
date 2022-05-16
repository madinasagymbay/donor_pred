setwd("/Users/zhengjingyi/Desktop")
projects.df <- read.csv("Projects_Cleandata_other_models.csv")

#drop "ID" column
projects.df <- projects.df [ , -c(1)]

projects.df$resource_usage <- factor(projects.df$resource_usage, levels = c(1, 2), 
                                      labels = c("essential", "enrichment"))


projects.df$resource_type <- factor(projects.df$resource_type, levels = c(1, 2, 3, 4, 5, 6), 
                                     labels = c("books", "other", "supplies", "technology", "trips", "visitors"))

#“set.seed(1234)” is used in all models to gain the same training datasets and validation datasets, so the evaluation of the models is accurate.
set.seed(1234)

train.index <- sample(row.names(projects.df), 0.6*dim(projects.df)[1])  
valid.index <- setdiff(row.names(projects.df), train.index)  
train.df <- projects.df[train.index, ]
valid.df <- projects.df[valid.index, ]

logit.reg <- glm(funding_status ~ ., data = train.df) 
options(scipen=999)
summary(logit.reg)

# use predict() with type = "response" to compute predicted probabilities. 
logit.reg.pred <- predict(logit.reg, valid.df[, -9], type = "response",family="binomial")

head(logit.reg.pred)

# first 10 actual and predicted records
data.frame(actual = valid.df$funding_status[1:10], predicted = logit.reg.pred[1:10])

#confusion matrix
y_pred_num<- ifelse(logit.reg.pred > 0.5, 1, 0)
y_pred<- factor(y_pred_num, levels=c(0,1))
y_act <- valid.df$funding_status

#accuracy
mean(y_pred == y_act)

caret::confusionMatrix(y_pred, as.factor(y_act), positive="1")

#ROC
library(InformationValue)
InformationValue::plotROC(y_act, logit.reg.pred)
InformationValue::AUROC(y_act, logit.reg.pred)






