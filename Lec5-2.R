library(MASS)
library(ISLR)

# interaction term
head(Boston)
lm.fit <- lm(medv~lstat*age, data=Boston)
lm.fit <- lm(medv~lstat+age+lstat:age, Boston)
summary(lm.fit)
attach(Boston)
plot(age,medv)



# Non-linear regression
attach(Boston)
lm.fit2 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
lstat2 = lstat^2
lm.fit3 <- lm(medv~lstat+lstat2)
summary(lm.fit3)

par(mfrow=c(2,2))
plot(lm.fit2)


# polynomial regression
lm.fit4 <- lm(medv~lstat+I(lstat^2)+I(lstat^3))
summary(lm.fit4)
lm.fit5 <- lm(medv~poly(lstat,3))
summary(lm.fit5)



















