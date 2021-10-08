## (1) Best Subset Selection
library(ISLR)
fix(Hitters)
head(Hitters)
dim(Hitters)
summary(Hitters)
Hitters <- na.omit(Hitters)



# (5) Ridge Regression
install.packages("glmnet")
library(glmnet)

# fit glmnet with all data
x <- model.matrix(Salary~.,Hitters)[,-1]
y <- Hitters$Salary
dim(x)
# Grid Search (lambda)
grid <- 10^seq(10,-2,lengt=100)

ridge.mod <- glmnet(x,y, alpha = 0, lambda=grid)
dim(coef(ridge.mod))
summary(ridge.mod)
ridge.mod$lambda[50]
grid[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) # l2 norm

# fit ridge using glmnet with training/test data
set.seed(1)
train <- sort(sample(1:nrow(x), nrow(x)/2))
length(test)
test <- sort(setdiff(1:nrow(x), train))
head(test)
y.test <- y[test]

ridge.mod <- glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred <- predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2) # MSE with lambda=4
mean((y[test]-mean(y[train]))^2)

ridge.pred <- predict(ridge.mod, s=1e10, newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred <- predict(ridge.mod, s=0, newx=x[test,])
mean((ridge.pred-y.test)^2)

# k-fold cv
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam <- cv.out$lambda.min
bestlam

ridge.pred <- predict(ridge.mod, s=bestlam, newx=x[test,])
mean((y.test-ridge.pred)^2)

# Obtain ridge coefficients with all data
out <- glmnet(x,y,alpha=0)
predict(out,type="coefficients", s=bestlam)[1:20,]

# (6) Lasso
lasso.model <- glmnet(x[train,],y[train], alpha=1,lambda=grid)
plot(lasso.model)

# Select the best lambda using k-fold CV
set.seed(1)
cv.out <- cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)

bestlam <- cv.out$lambda.min
bestlam
lasso.pred <- predict(lasso.model, s=bestlam, newx=x[test,])
mean((y.test-lasso.pred)^2)

# Obtain lasso coefficients with all data
out <- glmnet(x,y, alpha=1, lambda=grid)
lasso.coef <- predict(out, type="coefficients", s=bestlam)[1:20,]
lasso.coef
lasso.coef[lasso.coef!=0]

# (7) Principal Components Regression (PCR)
install.packages("pls")
library(pls)

# Fit PCR with all data
set.seed(2)
pcr.fit <- pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

# training/test data
set.seed(1)
pcr.fit <- pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type="MSEP")

pcr.pred <- predict(pcr.fit, x[test,],ncomp=7,newx=x[test,])
mean((y[test]-pcr.pred)^2)

# Final Model for PCR
pcr.fit <- pcr(Salary~., data=Hitters, scale=T, ncomp=7)
summary(pcr.fit)

# (8) Partial Least Squares Regression (PLS)
library(pls)
set.seed(1)
pls.fit <- plsr(Salary~., data=Hitters, subset=train, scale=T,
                validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type = "MSE")
pls.pred<- predict(pls.fit, x[test,], ncomp=2)
mean((y.test-pls.pred)^2)

# Obtain PLS coefficients with all data
pls.fit <- plsr(Salary~., data=Hitters, scale=T, ncomp=2)
summary(pls.fit)
