###################   KNN (owndataset.csv)   ################
rm(list=ls(all=TRUE))
library(data.table) 
library(caret)
library(ROCR)

context = read.csv('owndataset.csv')
summary(context)
sum(is.na(context))

context$class = factor(context$class,levels = c('g', 'h'),labels = c(1,0))

set.seed(100)
split = sample.split(context$class, SplitRatio = 0.70)
training_set = subset(context, split == TRUE)
test_set = subset(context, split == FALSE)

training_set[,1:10] = scale(training_set[,1:10])
test_set[,1:10] = scale(test_set[,1:10])

trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(class ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = training_set)
fit
plot(fit)

prob_pred = predict(fit, newdata = test_set[-11])
cm2 = table(test_set[, 11], prob_pred)
cm2

prediction_rate=(cm2[1,1]+cm2[2,2])/sum(cm2)
print(paste("Prediction rate: ",prediction_rate))
print(paste("Error rate: ",1-prediction_rate))

prob_pred1 = predict(fit, newdata = training_set[-11],type='prob')
pred<-prediction(prob_pred1[,1],training_set$class)
perf<-performance(pred,measure="tpr",x.measure="fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(perf,col="Red",lty=3, lwd=3)        ### ROC curve
area_under_curve<- performance(pred, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

prob_pred11 = predict(fit, newdata = test_set[-11],type='prob')
pred1<-prediction(prob_pred11[,1],test_set$class)
perf1<-performance(pred1,measure="tpr",x.measure="fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(perf1,col="Red",lty=3, lwd=3)        ### ROC curve
area_under_curve<- performance(pred1, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

###################   KNN (OnlineNewsPopularity.csv)   ################
rm(list=ls(all=TRUE))
library(data.table) 
library(caret)
library(ROCR)

context = read.csv('OnlineNewsPopularity.csv')
context1 = context[,-c(1,2,38,39)]
summary(context1)
context1=context1[!context1$n_unique_tokens==701,]
summary(context1)
mean(context1$shares)

context1$shares<-ifelse(context1$shares>3395,1,0)
context1$shares<-as.factor(context1$shares)
summary(context1)

set.seed(100)
split = sample.split(context1$shares, SplitRatio = 0.70)
training_set = subset(context1, split == TRUE)
test_set = subset(context1, split == FALSE)

training_set[,1:56] = scale(training_set[,1:56])
test_set[,1:56] = scale(test_set[,1:56])

trControl <- trainControl(method  = "cv", number  = 5)
fit <- train(shares ~ .,
             method     = "knn",
             tuneGrid   = expand.grid(k = 1:10),
             trControl  = trControl,
             metric     = "Accuracy",
             data       = training_set)
fit
plot(fit)

prob_pred = predict(fit, newdata = test_set[-57])
cm2 = table(test_set[, 57], prob_pred)
cm2
prediction_rate=(cm2[1,1]+cm2[2,2])/sum(cm2)
print(paste("Prediction rate: ",prediction_rate))
print(paste("Error rate: ",1-prediction_rate))

prob_pred1 = predict(fit, newdata = training_set[-57],type='prob')
pred<-prediction(prob_pred1[,2],training_set$shares)
perf<-performance(pred,measure="tpr",x.measure="fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(perf,col="Red",lty=3, lwd=3)        ### ROC curve
area_under_curve<- performance(pred, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

prob_pred11 = predict(fit, newdata = test_set[-57],type='prob')
pred1<-prediction(prob_pred11[,2],test_set$shares)
perf1<-performance(pred1,measure="tpr",x.measure="fpr")
par(mar=c(5,5,2,2),xaxs = "i",yaxs = "i",cex.axis=1.3,cex.lab=1.4)
plot(perf1,col="Red",lty=3, lwd=3)        ### ROC curve
area_under_curve<- performance(pred1, measure = "auc")
print(paste("Area under Curve:",area_under_curve@y.values[[1]]))

