# (1) Validation Set Approach
library(ISLR)
set.seed(1)
train=sample(1:392,196)
test <- setdiff(1:392, train)
Auto.test <- Auto[test,]
y <- Auto.test$mpg

attach(Auto)
lm.fit <- lm(mpg~horsepower, data = Auto, subset=train)
summary(lm.fit)
y.hat <- predict(lm.fit,Auto.test)
mean((y-y.hat)^2) # MSE
mean(mpg~predict(lm.fit,Auto))[-train]^2


lm.fit2 <- lm(mpg~poly(horsepower,2), data=Auto,subset = train)
y.hat2 <- predict(lm.fit2,Auto.test)
mean((y-y.hat2)^2) # MSE

lm.fit3 <- lm(mpg~poly(horsepower,3), data=Auto,subset = train)
y.hat3 <- predict(lm.fit3,Auto.test)
mean((y-y.hat3)^2) # MSE

# (2) LOOCV
lm.fit <- lm(mpg~horsepower, data=Auto)
coef(lm.fit)

glm.fit <- glm(mpg~horsepower,data = Auto)
coef(glm.fit)

library(boot)
cv.err <-cv.glm(Auto, glm.fit)
names(cv.err)
cv.err$K
cv.err$delta

cv.err <- rep(0,5)
for(i in 1:5){
  print(i)
  glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
  cv.err[i] <- cv.glm(Auto, glm.fit)$delta[1]
}
cv.err



# (3) k-fold CV
set.seed(17)
cv.err.10 <- rep(0,10)
for(i in 1:10){
  print(i)
  glm.fit <- glm(mpg~poly(horsepower,i), data=Auto)
  cv.err.10[i] <- cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.err.10


# (4) Bootstrap
library(ISLR)
head(Portfolio)
# 통계량에 대한 함수
alpha.fn <-function(data,index){
  X <- data$X[index]
  Y <- data$Y[index]
  return((var(Y)-cor(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}
alpha.fn(Portfolio, 1:100) # alpha에 대한 추정치
set.seed(2)
alpha.fn(Portfolio, sample(1:100,replace=T)) # alpha에 대한 Bootstrap추정치


boot(Portfolio, alpha.fn, R=1000)


# Bootstrap for coefficients in linear model
# 1차 함수
boot.fn <- function(data, index){
  lm.fit <- lm(mpg~horsepower, data=data,subset=index)
  coef(lm.fit)
}
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(1:392,replace=T))
boot(Auto, boot.fn, R=1000)
summary(lm(mpg~horsepower,data=Auto))$coef

# 2차 함수
boot.fn <- function(data, index){
  lm.fit <- lm(mpg~horsepower+I(horsepower^2), data=data,subset=index)
  coef(lm.fit)
}
boot.fn(Auto, 1:392)
set.seed(1)
boot.fn(Auto, sample(1:392,replace=T))
boot(Auto, boot.fn, R=1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
