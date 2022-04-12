
#install.packages("caTools")
#install.packages('rpart')
#install.packages("rpart.plot")

#install.packages("e1071")
#install.packages("caTools")
#install.packages("class")

library(caTools)
library(rpart)
library(tidyverse)
library(rpart.plot)
library(caret)

library(e1071)
library(class)

health <- read.csv('hospitals_infections.csv')

df<-data.frame(health)

healtedata<-tibble(
  cpcd=df$cpcd,
  pecd=df$pecd,
  hsbp=df$hsbp,
  readmission=df$readmission,
  compared=df$c_diff_compared
)

boxplot(health$cpcd)

boxplot(health$pecd)

boxplot(health$hsbp)

boxplot(health$readmission)

#missing value
is.na(healtedata)

#check outliers
ggplot(data=health)+geom_boxplot(mapping=aes(cpcd))
ggplot(data=health)+geom_boxplot(mapping=aes(pecd))
ggplot(data=health)+geom_boxplot(mapping=aes(hsbp))
ggplot(data=health)+geom_boxplot(mapping=aes(readmission))

set.seed(42)
#health$c_diff_compared<-ifelse(health$c_diff_compared,1,0)
sample<-sample.split(health$c_diff_compared,SplitRatio=0.7)
train_set<-filter(health,sample==TRUE)
dim(train_set)
test_set<-filter(health,sample=FALSE)
dim(test_set)


model<-rpart(c_diff_compared ~ ., data=health,method="class")
summary(model)
rpart.plot(model)

prediction<-predict(model,newdata = test_set[1,],type="class")
prediction

health[1,9]

prediction<-predict(model,newdata = test_set,type="class")

is.factor(test_set$c_diff_compared)
is.factor(prediction)
confusionMatrix(reference=as.factor(test_set$c_diff_compared),data=prediction,positive="Better than the U.S. National Benchmark")



#exercise2
#K-NN Classifier
#data(health)
str(health)
head(health)

# Splitting data into train
# and test data
split <- sample.split(iris, SplitRatio = 0.7)
train_cl <- subset(iris, split == "TRUE")
test_cl <- subset(iris, split == "FALSE")

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale <- scale(test_cl[, 1:4])

# Fitting KNN Model 
# to training dataset
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 1)
classifier_knn

# Confusiin Matrix
cm <- table(test_cl$Species, classifier_knn)
cm

# Model Evaluation - Choosing K
# Calculate out of Sample error
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 3
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 3)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 5
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 5)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 7
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 7)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 15
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 15)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))

# K = 19
classifier_knn <- knn(train = train_scale,
                      test = test_scale,
                      cl = train_cl$Species,
                      k = 19)
misClassError <- mean(classifier_knn != test_cl$Species)
print(paste('Accuracy =', 1-misClassError))
