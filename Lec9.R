## Support vector machines

# Generate training data/test data
set.seed(1)
x = matrix(rnorm(20*2),ncol=2)
x
y = c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] +1 
plot(x,col=(3-y),pch=16)
dat <- data.frame(x=x, y=as.factor(y))
dat

xtest <- matrix(rnorm(20*2), ncol=2)
ytest <- sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,] = xtest[ytest==1,] +1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

# (1) Supprot Vector Classifier
install.packages("e1071")
library(e1071)
svmfit <- svm(y~., data=dat, kernel="linear",cost=10,scale=FALSE)
plot(svmfit,dat)
svmfit$index
summary(svmfit)

# 10 fold-cv
set.seed(1)
tune.out <- tune(svm, y~., data = dat, kernel="linear",
                 ranges=list(cost=c(0.001,0.01,0.1,1,5,10,100)))
summary(tune.out)
names(tune.out)
bestmodel <- tune.out$best.model
bestmodel
ypred <- predict(bestmodel, testdat)
table(ypred,testdat$y)


# (2) Supprot Vector Machine
set.seed(1)
x <- matrix(rnorm(200*2), ncol=2)
x[1:100,] = x[1:100,] +2
x[101:150,] = x[101:150,] -2
y <- c(rep(1,150), rep(2,50))
dat <- data.frame(x=x, y=as.factor(y))
plot(x,col=y+1,pch=16)

# fit SVM with radial kernal
train = sample(200,100)
svmfit <- svm(y~.,data=dat[train,],kernel="radial",
              gamma=1, cost=1)
plot(svmfit,dat[train,])
summary(svmfit)
svmfit$index


# 10 fold CV
set.seed(1)
tune.out <- tune(svm, y~., data=dat[train,],kernel="radial",
                 ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
tune.out$best.parameters
table(true=dat[-train,"y"],
pred=predict(tune.out$best.model, newx=dat[-train,]))
table(pred,dat[-train,"y"])

# (4) SVM with multiple Classes
set.seed(1)
x <-rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y,rep(0,50))
length(y)
x[y==0,2]=x[y==0,2] +2
dat<-data.frame(x=x,y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="radial",cost=10,
              gamma=1)
plot(svmfit, dat)
summary(svmfit)
