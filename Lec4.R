install.packages("ISLR")
library(MASS)
library(ISLR)
head(Boston)

lm.fit = lm(medv~lstat, data=Boston)
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
confint(lm.fit)
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5,10,15))), interval="prediction")

attach(Boston)
plot(lstat,medv, pch=20)
abline(lm.fit, lwd=3, col="red")

par(mfrow=c(2,2))
plot(lm.fit)
