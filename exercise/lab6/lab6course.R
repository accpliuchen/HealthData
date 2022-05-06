library(ggplot2)
library(tidyverse)


hosptials_tb <- read.csv('hospitals_readmissions.csv')
hosptials_tb

#Simple Logistic Regression in R
hosptials_tb$compared_to_national<-ifelse(hosptials_tb$compared_to_national=='Better than U.S. National Rate',1,0)

hosptials_tb

result<-glm(compared_to_national~emergency_services,family="binomial",data=hosptials_tb)
summary(result)

exp(coef(result))

confint.default(result)

exp(confint.default(result))