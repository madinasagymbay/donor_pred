setwd("/Users/zhengjingyi/Desktop")
projects.df <- read.csv("projects_cleandata_KNN.csv")
projects1.df <- projects.df[ , -c(1)]


set.seed(1234)
train.index <- sample(row.names(projects1.df), 0.6*dim(projects1.df)[1])  
valid.index <- setdiff(row.names(projects1.df), train.index) 
train.df <- projects1.df[train.index, ]
valid.df <- projects1.df[valid.index, ]

# initialize normalized training, validation data, complete data frames to originals
train.norm.df <- train.df
valid.norm.df <- valid.df
projects1.norm.df <- projects1.df

## scatter plot
plot(students_reached ~ num_donors, data=train.norm.df, pch=ifelse(train.norm.df$funding_status=="1", 1, 3))
text(train.norm.df$students_reached, train.norm.df$num_donors, rownames(train.df), pos=4)
text(60, 20, "X")
legend("topright", c("successful", "unsuccessful"), pch = c(1, 3, 4))

library(caret)
# use preProcess() from the caret package to normalize variables.
norm.values <- preProcess(train.df[, -c(15)], method=c("center", "scale")) 
train.norm.df[, -c(15)] <- predict(norm.values, train.df[, -c(15)])
valid.norm.df[, -c(15)] <- predict(norm.values, valid.df[, -c(15)])
projects1.norm.df[, -c(15)] <- predict(norm.values, projects1.df[, -c(15)])

# use knn() to compute knn. 
# knn() is available in library FNN (provides a list of the nearest neighbors) 
# and library class (allows a numerical output variable).
library(FNN)
nn <- knn(train = train.norm.df[, -c(15)], test = valid.norm.df[, -c(15)], 
          cl = train.norm.df[, 15], k = 3)
cm = table(valid.norm.df[, 15], nn)
cm

### choose K 
library(caret)


# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 20, 2), accuracy = rep(0, 10))

# compute knn for different k on validation.
for(i in 1:10) {
  knn.pred <- knn(train.norm.df[, -c(15)], valid.norm.df[, -c(15)], 
                  cl = train.norm.df[, 15], k = i*2-1)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, factor(valid.norm.df[, 15]))$overall[1] 
}
accuracy.df
#plot comparison of accuracy against k
plot(accuracy.df,
     xlab = "Number of neighbors (k)",
     main = "Comparision of Accuracy against k",
     type = "b",
     col = "black",
     lwd = 1.8,
     pch = "o")
#when k=9, model has the best accuracy
#run KNN model when k=9
nn <- knn(train = train.norm.df[, -c(15)], test = valid.norm.df[, -c(15)], 
          cl = train.norm.df[, 15], k = 9)

#confusion matrix for k=9
y_act <- valid.norm.df$funding_status
caret::confusionMatrix(nn, as.factor(y_act), positive="1")
#plot ROC
library(pROC, warn.conflicts=F)
roc1<-roc(y_act,as.integer(nn[1:length(y_act)])-1)
plot(roc1,print.auc=TRUE,plot=TRUE,print.thres=TRUE)

