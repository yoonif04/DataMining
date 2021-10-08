# (1) Classification Trees using training data
install.packages("tree")

library(tree)
library(ISLR)
attach(Carseats)
str(Carseats)
summary(Carseats)

High <- as.factor(ifelse(Sales<=8, "No", "Yes"))
table(High)
mode(High)

Carseats <- data.frame(Carseats,High)
str(Carseats)

tree.carseats <- tree(High~.-Sales, Carseats)
summary(tree.carseats)

plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats


# (2) classification tree using test rate
set.seed(3)
train <- sort(sample(1:nrow(Carseats), 200))
test <- setdiff(1:400, train)
Carseats.test <- Carseats[test,]
High.test <- High[test]
tree.carseats <- tree(High~.-Sales, Carseats, subset = train)
tree.pred <- predict(tree.carseats,Carseats.test, type="class")
table(tree.pred, High.test)

# (3) CV + prunning for classification tree
set.seed(6)
cv.carseats <- cv.tree(tree.carseats, FUN=prune.misclass)
cv.carseats
par(mfrow=c(1,2))
plot(cv.carseats$size, cv.carseats$dev, type="b")
plot(cv.carseats$k, cv.carseats$dev, type="b")


prune.carseats <- prune.misclass(tree.carseats, best=12)
plot(prune.carseats)
par(mfrow=c(1,1))
text(prune.carseats, pretty=0, cex=0.8)
tree.pred <- predict(prune.carseats, Carseats.test, type="class")
table(tree.pred, High.test)

# (4) Regression tree using training data
library(MASS)
set.seed(1)
train = sort(sample(1:nrow(Boston), nrow(Boston)/2))
test = setdiff(1:nrow(Boston), train)
tree.boston <- tree(medv~., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0, cex=0.8)

# CV + prunning for regression tree
cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")
prune.boston <- prune.tree(tree.boston, best=7)
plot(prune.boston)
text(prune.boston, pretty=0, cex=0.8)

yhat <- predict(tree.boston, newdata=Boston[test,])
boston.test <- Boston[test,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat-boston.test)^2)

# (6) Bagging
install.packages("randomForest")
library(randomForest)
library(MASS)
set.seed(1)
bag.boston <- randomForest(medv~., data=Boston, subset=train, mtry=13, importance=T)
bag.boston

yhat.bag <- predict(bag.boston, Boston[test,])
plot(yhat.bag, boston.test)
mean((yhat.bag-boston.test)^2)

# (7) Random Forests
set.seed(1)
rf.boston <- randomForest(medv~.,data=Boston, subset=train,mtry=6,importance=TRUE)
yhat.rf <- predict(rf.boston, newdata=Boston[test,])
plot(yhat.rf, boston.test)
mean((yhat.rf-boston.test)^2)

importance(rf.boston)
varImpPlot(rf.boston)

# (8) Boosting
install.packages("gbm")
library(gbm)
set.seed(1)
boost.boston <- gbm(medv~.,data=Boston[train,],distribution = "gaussian",n.trees=5000,interaction.depth = 4)
summary(boost.boston)

yhat.boost <- predict(boost.boston, newdata=Boston[test,])
mean((yhat.boost-boston.test)^2)

