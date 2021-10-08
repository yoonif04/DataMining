
setwd("C:/Users/3covl/Desktop/DataMining")
Auto <- read.csv("Auto.csv", header=T, na.strings = "?")
Auto <- na.omit(Auto) 
head(Auto)
attach(Auto)
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+year+origin+as.factor(name), Auto)

# pairs 저장
getwd()
pdf("pairs.pdf")
pairs(~mpg+cylinders+displacement+horsepower+weight+acceleration+year+origin, Auto)
dev.off()

# correlation
cor(Auto[,!(names(Auto) %in% c("name"))])

# lm
lm.fit <- lm(mpg~. -name, data=Auto)
summary(lm.fit)


# (d)
par(mfrow=c(2,2))
plot(lm.fit)

# (e)
lm.fit2 <- lm(mpg~ origin*year+weight*displacement, data=Auto)
summary(lm.fit2)
par(mfrow=c(2,2))
plot(lm.fit2)

# (f)
lm.fit3 <- lm(mpg~. -name-horsepower+I(log(horsepower)), data=Auto)
summary(lm.fit3)


