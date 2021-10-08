# Data
library(ISLR)
attach(Wage)
dim(Wage)

agelims <- range(age)
agelims
age.grid <- seq(18,80)
age.grid

# Regression Spline
install.packages("splines")
library(splines)
colnames(Wage)
fit <- lm(wage~bs(age,knots=c(25,40,60)), data=Wage)
fit
pred <- predict(fit, newdata=list(age=age.grid),se=T)
pred
plot(age,wage,col="grey")
lines(age.grid, pred$fit,lwd=2)
abline(v=c(25,20,60),lty="dashed",col="red")
lines(age.grid, pred$fit+1.96*pred$se.fit,lty="dashed")
lines(age.grid, pred$fit-1.96*pred$se.fit,lty="dashed")

# Natural Spline
fit1 <- lm(wage~bs(age,knots=c(25,40,60)), data=Wage)
pred1 <- predict(fit1, newdata=list(age=age.grid),se=T)

fit2 <- lm(wage~ns(age,df=4), data=Wage) 
summary(fit2)
pred2 <- predict(fit2,newdata=list(age=age.grid),se=T)
plot(age,wage,col="grey")
lines(age.grid, pred1$fit,lwd=2)
lines(age.grid, pred2$fit,lwd=2,col="red")

# Smoothing Spline useingCross-validation
plot(age,wage, xlim = agelims, col="dark grey")
fit<- smooth.spline(age,wage, df=16)
fit2<- smooth.spline(age,wage,cv=T)
fit2$df
lines(fit, col="red", lwd=2)
lines(fit2, col="blue", lwd=2)

# local regression
plot(age,wage, xlim=agelims, col="dark grey")
fit<- loess(wage~age, span=0.2, data=Wage)
fit2 <- loess(wage~age, span=0.5,data=Wage)
fit3 <- loess(wage~age, span=0.1,data=Wage)
pred <- predict(fit,data.frame(age=age.grid))
pred2<- predict(fit2,data.frame(age=age.grid))
pred3<- predict(fit3,data.frame(age=age.grid))

lines(age.grid,pred,col="red")
lines(age.grid, pred2, col="blue")
lines(age.grid, pred3, col="green")

# GAM
install.packages("gam")
library(gam)

# using natural spline, smoothing spline
gam <- lm(wage~ns(year,df=4)+ns(age,df=5)+education,data=Wage)
summary(gam)

gam.m3 <- gam(wage~s(year,4)+s(age,5)+education,data=Wage)
summary(gam.m3)
par(mfrow=c(1,3))
plot(gam.m3, se=T, col="blue")

# compare three model
gam.m1<- gam(wage~s(age,5)+education, data=Wage)
gam.m2<- gam(wage~year+s(age,5)+education,data=Wage)

anova(gam.m1,gam.m2,gam.m3,test="F")

# use local regression
gam.lo <- gam(wage~s(year,4)+lo(age,span=0.7)+education,data=Wage)
summary(gam.lo)
par(mfrow=c(1,3))
plot(gam.lo, se=T, col="blue")

# logistic regression GAM
install.packages("akima")
library(akima)
gam.lr<- gam(I(wage>250)~year+s(age,df=5)+education,
             family=binomial,data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")
table(Wage$education)
idx<- Wage$education != "1. < HS Grad"
sum(idx)
gam.lr<- gam(I(wage>250)~year+s(age,df=5)+education,
             family=binomial,data=Wage,
             subset=idx)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")
