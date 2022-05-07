
#install.packages('caTools')
#install.packages("caret")

library(rpart)
library(tidyverse)
library(caTools)
library(caret)

set.seed(42)

#Decision Tree in R
patients_tb <- read.csv('diabetes2.csv')
patients_tb

patients_tb[1,8]

#Decision Tree in R tree
sample<-sample.split(patients_tb$Outcome,SplitRatio=0.7)
train_set<-filter(patients_tb,sample==TRUE)

dim(train_set)

test_set<-filter(patients_tb,sample==FALSE)
dim(test_set)

#Decision Trees in R - Training the Model and Visualizing the Decision Tree
library(rpart.plot)

model<-rpart(Outcome~., data=patients_tb,method="class")
summary(model)

rpart.plot(model)


#Decision Trees in R marking Predictions
prediction<-predict(model,newdata=test_set[1,],type="class")
prediction

patients_tb[1,9]

#Predicting on the entire test set
prediction<-predict(model,newdata = test_set,type="class")



#create the confusion matrix
is.factor(test_set$Outcome)
is.factor(prediction)

confusionMatrix(reference=as.factor(test_set$Outcome),data=prediction,mode="everything",positive="1")