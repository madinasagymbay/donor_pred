setwd("~/Desktop")
projects.df <- read.csv("Projects_Cleandata_other_models.csv")
# delete the id column
projects.df <- projects.df [ , -c(1)]


#treat numerical as categorical

projects.df$resource_usage <- factor(projects.df$resource_usage, levels = c(1, 2), 
                            labels = c("essential", "enrichment"))


projects.df$resource_type <- factor(projects.df$resource_type, levels = c(1, 2, 3, 4, 5, 6), 
                                      labels = c("books", "other", "supplies", "technology", "trips", "visitors"))



set.seed(1234)
train.index <- sample(row.names(projects.df), 0.6*dim(projects.df)[1])  
valid.index <- setdiff(row.names(projects.df), train.index)  
train.df <- projects.df[train.index, ]
valid.df <- projects.df[valid.index, ]


# classification tree
library(rpart)
library(rpart.plot)
library(e1071)
default.ct <- rpart(funding_status ~ ., data = train.df, method = "class", cp=0.002)
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10)

library(caret)
# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")

# show some predictions of model based on train.df
data.frame(actual = train.df$funding_status[1:10], predicted = default.ct.point.pred.train [1:10])

y_act <- train.df$funding_status
#confusion matrix
caret::confusionMatrix(default.ct.point.pred.train, as.factor(y_act), positive="1")

###############

# set argument type = "class" in predict() to generate predicted class membership.
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")

## show some predictions of model based on valid.df
data.frame(actual = valid.df$funding_status[1:10], predicted = default.ct.point.pred.valid [1:10])

y_act <- valid.df$funding_status
#confusion matrix
caret::confusionMatrix(default.ct.point.pred.valid, as.factor(y_act), positive="1")

# generate confusion matrix for validation data
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$funding_status))


#full-grown tree
deeper.ct <- rpart(funding_status ~ ., data = train.df, method = "class", cp=0.00001, minsplit = 1)
#plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,
    box.col = ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))
##############
#############################################
library(caret)
# set argument type = "class" in predict() to generate predicted class membership.
deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")

# show some predictions of model based on train.df
data.frame(actual = train.df$funding_status[1:10], predicted = deeper.ct.point.pred.train [1:10])

y_act <- train.df$funding_status
#confusion matrix
caret::confusionMatrix(deeper.ct.point.pred.train, as.factor(y_act), positive="1")

###############

# set argument type = "class" in predict() to generate predicted class membership.
deeper.ct.point.pred.valid <- predict(deeper.ct,valid.df,type = "class")

# show some predictions of model based on valid.df
data.frame(actual = valid.df$funding_status[1:10], predicted = deeper.ct.point.pred.valid [1:10])

y_act <- valid.df$funding_status
#confusion matrix
caret::confusionMatrix(deeper.ct.point.pred.valid, as.factor(y_act), positive="1")

#plot ROC for default tree
library(pROC)
PredictionWithProbs<- predict(default.ct, valid.df, type = 'prob')
auc<-auc(valid.df$funding_status, PredictionWithProbs[,2])
plot(roc(valid.df$funding_status, PredictionWithProbs[,2]))

#plot ROC for fully grown tree
library(pROC)
PredictionWithProbs<- predict(deeper.ct, valid.df, type = 'prob')
auc<-auc(valid.df$funding_status, PredictionWithProbs[,2])
plot(roc(valid.df$funding_status, PredictionWithProbs[,2]))



