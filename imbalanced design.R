### DATA IMPORTATION

# setting working directory 
setwd("~/")

credit.card=read.csv("creditcard.csv")

# packages used
library(ROSE)
library(rpart)
library(ROCR)
library(xlsx)

# Checking the size of imbalance
prop.table(table(credit.card$Class))  # the class is 99.9% imbalance in favour of the group 0

# partitioning dataset
credit.train=head(credit.card,213605)
credit.test=tail(credit.card,71202)

# balance level of training data
prop.table(table(credit.train$Class))

# balance level of testing data
prop.table(table(credit.test$Class))

# Fitting a decision tree algorithm to access the modeling accuracy with this imbalanced dataset
attach(credit.card)
credit.imb.model=rpart(Class~.,data = credit.train)
credit.forecast=predict(credit.imb.model,credit.test)

# checking forecast accuracy
accuracy.meas(credit.test$Class,credit.forecast)
roc.curve(credit.test$Class,credit.forecast)


### BALANCING THE DATASET
#Note: there are 213207 0's and 398 1's from a total of 213605 observations

# oversampling
cred.bal.over=ovun.sample(Class~.,data = credit.train,method = "over",N=426414)$data
prop.table(table(cred.bal.over$Class))

# undersampling
cred.bal.under=ovun.sample(Class~.,data = credit.train,method = "under",N=796)$data
prop.table(table(cred.bal.under$Class))

# Both
cred.bal.both=ovun.sample(Class~.,data = credit.train,method = "both")$data
prop.table(table(cred.bal.both$Class))

# synthetic data generation method
cred.bal.syn=ROSE(Class~.,data = credit.train,seed = 1234)$data
prop.table(table(cred.bal.syn$Class))


## Model fitting using decision tree
tree.over=rpart(Class~.,data = cred.bal.over)
tree.under=rpart(Class~.,data = cred.bal.under)
tree.both=rpart(Class~.,data = cred.bal.both)
tree.syn=rpart(Class~.,data = cred.bal.syn)


## predicting crime rate from the testing data with each model
pred.tree.over=predict(tree.over,newdata = credit.test)
pred.tree.under=predict(tree.under,newdata = credit.test)
pred.tree.both=predict(tree.both,newdata = credit.test)
pred.tree.syn=predict(tree.syn,newdata = credit.test)


## testing accuracy of each model
par(mfrow=c(2,2))
roc.curve(credit.test$Class,pred.tree.over,main="ROC for Over sampling")
roc.curve(credit.test$Class,pred.tree.under,main="ROC for under sampling")
roc.curve(credit.test$Class,pred.tree.both,main="ROC for combined method")
roc.curve(credit.test$Class,pred.tree.syn,main="ROC for synthetic method")


## OVERALL forecast using the best model

# prediction
predict.overall=predict(tree.both,credit.card)

# accessing the optimal treshold for decision making
rocrpred=ROCR::prediction(predict.overall,credit.card$Class)
predict.overall.perf=performance(rocrpred,"tpr","fpr")

plot(predict.overall.perf, colorize=T,print.cutoffs.at=seq(0,1,0.25), text.adj=c(-0.2,1.7))

# Final classification and presentation with threshold level of 0.85
final.prediction=ifelse(predict.overall>0.85,1,0)
final.data=data.frame(cbind(credit.card$Time,credit.card$Amount,final.prediction,credit.card$Class))
names(final.data)=c("Time","Amount","Predicted class","Actual class")
write.csv(final.data,file = "crime.predict.csv")

