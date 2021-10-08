library(ISLR)
library(help="ISLR")
dim(Smarket)
head(Smarket)
cor(Smarket[,-9])

attach(Smarket)
table(Year)
plot(Volume, col="blue", pch=16, main="Plot for Volumn in Smarket Data")

# (2) Fit Logistic Regression
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)

# (3) Predicted probability
glm.prob <- predict(glm.fit, type="response")
glm.prob
contrasts(Direction)
glm.pred <- rep("Down", 1250)
glm.pred[glm.prob>0.5]="Up"
glm.pred[glm.prob<=0.5]="Down"
table(glm.pred, Direction)


# (4) Fit logistic regression using training data
train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)

Direction.2005 <- Direction[!train]
glm.fit <- glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
               data=Smarket, family=binomial, subset=train)
glm.probs <- predict(glm.fit, newdata=Smarket.2005, type="response")
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>0.5] ="Up"
table(glm.pred, Direction.2005)
prop.true <- mean(glm.pred==Direction.2005)
prop.true
prop.false <- mean(glm.pred != Direction.2005)
prop.false

# (6) Fit LDA using training data
library(ISLR)
library(MASS)
attach(Smarket)

train<-(Year<2005)
Smarket.2005 <- Smarket[!train,]
Direction.2005 <- Direction[!train]

lda.fit <- lda(Direction~ Lag1+Lag2, data=Smarket, subset=train)
lda.fit

lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.pred$class
table(lda.pred$class, Direction.2005)


# (7) QDA
qda.fit <- qda(Direction~ Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.class <- predict(qda.fit, Smarket.2005)$class
table(qda.class, Direction.2005)

# KNN
library(class)
train.X <- cbind(Lag1, Lag2)[train,]
train.X <- Smarket[train,c(2,3)]
train.X
dim(train.X)

test.X <- cbind(Lag1,Lag2)[!train,]
test.X <- Smarket[!train, c(2,3)]

train.Direction <- Direction[train]
test.Direction <- Direction[!train]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, test.Direction)
