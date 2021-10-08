# scatter plots (산점도)
x <- rnorm(1000)    # rnorm(1000, mena=0, sd=1)
y <- rnorm(1000)
plot(x,y)
plot(x,y, xlab="x축", ylab="y축",col="red"
     , pch=16 , main="산점도")


# scatter plot 저장
getwd()
pdf("MyfirstPlot.pdf")
xy <- matrix(c(x,y), length(x), 2)
head(xy)
plot(xy, col=densCols(xy), xlab="x-axis", ylab="y-axis", 
     pch=20, cex=2, main="X vs Y")
dev.off()

length(densCols(xy))
head(densCols(xy))

# 3차원 그래프 (1)
x = y = 1:5
x
y
f <- outer(x,y)  # 외적 (x %*% t(y))
f
t(y)
contour(x,y, f)  # 등고선
image(x,y,f)     # Heatmap
persp(x,y,f)     # Perspective plot


# 3차원 그래프 (2)
x <- y <- seq(-pi, pi, length=50)
f <- outer(x,y, function(x,y) cos(y)/(1+x^2))
fa <- (f-t(f))/2
contour(x,y,fa)   # 등고선
image(x,y,fa)     # Heatmap
persp(x,y,fa)     # Perspective plot

persp(x,y,fa, theta=30, phi=20)
?persp

# 파일 읽고 쓰기
setwd("C:/Users/3covl/Desktop/DataMining/auto_data")
getwd()
setwd(readClipboard())  # 복사한 경로

Auto <- read.table("Auto.data", header=T, na.strings = "?")
Auto <- read.csv("Auto.csv", header=T, na.strings = "?")
head(Auto)
dim(Auto)
sum(is.na(Auto))

Auto <- na.omit(Auto)
dim(Auto)

write.csv(Auto, "Auto_remove_na.csv", row.names=F)

head(Auto)
table(Auto$cylinders)
plot(Auto$cylinders, Auto$mpg)
attach(Auto)   # Data frame내의 변수를 직접 사용 가능
plot(cylinders, mpg)

mode(Cylinders)
cylinders <- as.factor(cylinders)
class(cylinders)
plot(cylinders, mpg, col="red", varwidth=T, horizontal=T)

# histogram
hist(mpg, col=2, breaks=15)

# Scatter plot matrix
pairs(~mpg+displacement+horsepower+weight+acceleration)
plot(horsepower, mpg)
identify(horsepower, mpg, name)
name

summary(cylinders)

