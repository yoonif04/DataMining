# vector 객체 생성
x<-c(1,3,2,5)
x

x<-c(1,6,2)
y<-c(1,4,3)
x
y
length(x)
length(y)

x+y
x-y
x*y
x/y

union(x,y)
setdiff(x,y)
intersect(x,y)

a<-"park"
ls()            #객체 리스트
rm(a)
rm(list=ls())    #객체 전체 삭제


# Matrix 객체 생성
x<-matrix(c(1,2,3,4), nrow=2,ncol=2)
x
? matrix
y <- matrix(c(1,2,3,4),2,2,byrow=T)
y
sqrt(x)
x^2
x*x
x%*%x

A<- matrix(1:16,4,4)
A
A[3,3]
A[3:4,3:4]
A[-c(3:4),-c(3:4)]
A[-c(3:4)]
A[-c(3:4),]

length(A)
dim(A)

# Random Samples from Normal dist N(0,1)
x <- rnorm(50)
x <- rnorm(50, 0,1)
y <- x + rnorm(50,50,0.1)
x
y
cor(x,y)
plot(x,y)

set.seed(3)
y <- rnorm(100)
y
mean(y)
var(y)
sd(y)
sqrt(var(y))
