# K-means Clustering
set.seed(2)
x <- matrix(rnorm(50*2),ncol=2)
x[1:25,1] <- x[1:25,1]+3
x[1:25,2] <- x[1:25,2]-4
plot(x)

km.out<-kmeans(x,centers=4,nstart=20)
km.out$cluster
plot(x,col=km.out$cluster+1, pch=16)
km.out$withinss

# Hierarchical Clustering
dist.x <- dist(x)
dim(x)
cor.x <- as.dist(1-cor(t(x)))
hc.complete <- hclust(dist.x, method="complete")
hc.average<-hclust(dist.x,method="average")
hc.single<-hclust(dist.x,method="single")
par(mfrow=c(1,3))
plot(hc.complete,main="complete")
plot(hc.average,main="average")
plot(hc.single,main="single")

cutree(hc.complete, h=5)

xx<- matrix(data=c(1,2,4,4,5,5,4,6,3,3),ncol=2)
dist.xx <- dist(xx)
hc.c<-hclust(dist.xx, method="complete")
hc.a<-hclust(dist.xx, method="average")
hc.s<-hclust(dist.xx, method="single")
par(mfrow=c(1,3))
plot(hc.c,main="complete")
plot(hc.a,main="average")
plot(hc.s,main="single")
