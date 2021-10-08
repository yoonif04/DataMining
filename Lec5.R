library(MASS)
library(ISLR)
head(Boston)
dim(Boston)

lm.fit<-lm(medv~lstat+age, data=Boston)
summary(lm.fit)


lm.fit <-lm(medv~., data=Boston)
summary(lm.fit)

lm.fit1 <- update(lm.fit, ~. -age)
summary(lm.fit1)