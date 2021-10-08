setwd("C:/Users/3covl/Desktop/DataMining")
wine <- read.csv("winequality-white.csv")

str(wine)  
sum(is.na(wine))  # 0 -> NA 값 없다

wine_input <-wine[1:11]   # input variables
attach(wine_input)

# correlation plot
install.packages("corrplot")
library(corrplot)
corrplot(cor(wine_input))   

# correlation test
cor.test(density,alcohol)   # -0.78
cor.test(density,residual.sugar)  # 0.83
cor.test(total.sulfur.dioxide,free.sulfur.dioxide) #0.61

# 절댓값 0.7이상의 상관관계 가지는 경우 제거
wine_input <- wine_input[-8]   # 8번 열 density 제거

# 제거 후 correlation plot
corrplot(cor(wine_input),addCoef.col = "gray")


######### 혜인 ###########
# wine_output (quality변수) 분류하기 

# (1) 필요한 라이브러리 로딩 
library(dplyr)
library(gridExtra)
library(RColorBrewer)
library(randomForest)
library(party)
library(ggplot2)
theme_set(theme_classic())

glimpse(wine)
str(wine)
summary(wine)


# (2) white wine distribution 시각적으로 확인하기 
print(" Number of wines for a partcular rating of wine:")
table(wine$quality)   # 3부터 9까지의 quality가 분포하고 있음을 확인할 수 있다. 
theme_set(theme_minimal())
ggplot(wine,aes(quality)) + geom_histogram(stat="count") +
  xlab("Quality of white wines") + ylab("Number of white wines")

# (3) classifying the quality of white wines 
wine$rating <- ifelse(as.integer(wine$quality)>6,1,0) # quality가 7이상일 때 good 그 이하일 때 not good으로 설정 
glimpse(wine)
table(wine$rating)


# 전처리 끝난 최종 wine 정보 (2,7,21,52,58,59번째 줄 실행)
rating<-wine$rating
wine<-cbind(wine_input,rating)

# 10 folds
install.packages("caret")
library(caret)
set.seed(1)
folds <- createFolds(as.integer(wine[,1]),k=10) #4898개 데이터 10개로 나눔 
str(folds) #10개의 fold생성됨 확인가능
# 각 fold 정보 각각 저장 -> 각 fold가 test셋이 됨
fold1 <- unname(unlist(folds[1]))  
fold2 <- unname(unlist(folds[2]))
fold3 <- unname(unlist(folds[3]))
fold4 <- unname(unlist(folds[4]))
fold5 <- unname(unlist(folds[5]))
fold6 <- unname(unlist(folds[6]))
fold7 <- unname(unlist(folds[7]))
fold8 <- unname(unlist(folds[8]))
fold9 <- unname(unlist(folds[9]))
fold10 <- unname(unlist(folds[10]))


# boosting
library(gbm)
set.seed(1)
cv_error_bt = rep(0,10)        # cv error 저장할 변수

# cv function for boosting
cv_funct_bt <-function(fold,i){
  train <- wine[-fold,] 
  test_x <- wine[fold,]
  test_y <- wine[fold,"rating"]
  bt <- gbm(rating~.,data=train,distribution = "bernoulli",n.trees=5000,interaction.depth = 4)
  summary(bt)
  yhat<-predict(bt,newdata=test_x,n.trees=5000)
  cv_error_bt[i] = mean((yhat-test_y)^2)
  return (cv_error_bt[i])
}

# 1~10까지 바꿔가면서 진행
cv_error_bt[10] <- cv_funct_bt(fold10,10)

mean(cv_error_bt)  #cv값의 평균

# supprot vector classifier
library(e1071)
set.seed(1)

cv_funct_svc <- function(fold){
  train_svc <- wine[-fold,]
  train_svc$rating <- as.factor(train_svc$rating) # y가 factor여야 함
  test_x_svc <-wine[fold,]
  test_x_svc <- test_x_svc[-11]
  test_y_svc <- as.factor(wine[fold,"rating"])
  testdat <- data.frame(test_x_svc,test_y_svc)
  
  tune.out <- tune(svm, as.factor(rating)~., data=train_svc, kernel="linear",ranges=list(cost=c(0.1,1,5,10)))
  ypred <- predict(tune.out$best.model, testdat)
  return(table(ypred, test_y_svc))
}
cv_funct_svc(fold10)

# support vector machine
cv_funct_svm <- function(fold){
  train_svm <- wine[-fold,]
  train_svm$rating <- as.factor(train_svm$rating) # y가 factor여야 함
  test_x_svm <-wine[fold,]
  test_x_svm <- test_x_svm[-11]
  test_y_svm <- as.factor(wine[fold,"rating"])
  testdat <- data.frame(test_x_svm,test_y_svm)
  
  tune.out <- tune(svm, as.factor(rating)~., data=train_svm, kernel="radial",ranges=list(cost=c(0.1,1,10),gamma=c(0.5,1,2)))
  ypred <- predict(tune.out$best.model,testdat)
  return(table(ypred,test_y_svm))
}
cv_funct_svm(fold1)

