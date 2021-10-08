setwd("C:/Users/3covl/Desktop/DataMining")
wine <- read.csv("winequality-white.csv")

str(wine)  
sum(is.na(wine))  # 0 -> NA 값 없다

wine_input <-wine[1:11]   # input variables
attach(wine_input)
wine_output <-wine[12]    # output variable

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
corrplot(cor(wine_input))

  


