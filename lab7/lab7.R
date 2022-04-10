
#install.packages("caTools")
#install.packages('rpart')
#install.packages("rpart.plot")

library(caTools)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(caret)

health <- read.csv('hospitals_infections.csv')
health

set.seed(42)
sample<-sample.split(health$readmission,SplitRatio=0.7)
train_set<-filter(health,sample==TRUE)
dim(train_set)
test_set<-filter(health,sample=FALSE)
dim(test_set)


model<-rpart(hospital_ownership ~ ., data=health,method="class")
summary(model)
rpart.plot(model)

prediction<-predict(model,newdata = test_set[1,],type="class")
prediction

health[1,9]

prediction<-predict(model,newdata = test_set,type="class")

is.factor(test_set$hospital_ownership)
is.factor(prediction)
confusionMatrix(reference=as.factor(test_set$hospital_ownership),data=prediction,positive="1")
