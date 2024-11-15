data_clust <- read.csv("https://raw.githubusercontent.com/kesslerz-dickinson/Data180_Fall24/refs/heads/main/data/cluster_data.csv")
plot(x1~x2,data=data_clust)

k_3 <- kmeans(data_clust,3)
k_4 <- kmeans(data_clust,4)

plot(x1~x2,data=data_clust,cex.axis=1.3,
     cex.lab=1.2,cex=1.2,pch=15+k_3$cluster,col=k_3$cluster)
points(x1~x2,data=k_3$centers,pch=10,cex=1.8,col="orange")

plot(x1~x2,data=data_clust,cex.axis=1.3,
     cex.lab=1.2,cex=1.2,pch=15+k_4$cluster,col=k_4$cluster)
points(x1~x2,data=k_4$centers,pch=10,cex=1.8,col="orange")

k_3_50 <- kmeans(data_clust,3,nstart=50)
k_4_50 <- kmeans(data_clust,4,nstart=50)

plot(x1~x2,data=data_clust,cex.axis=1.3,
     cex.lab=1.2,cex=1.2,pch=15+k_3_50$cluster,col=k_3_50$cluster)
points(x1~x2,data=k_3_50$centers,pch=10,cex=1.8,col="purple")

plot(x1~x2,data=data_clust,cex.axis=1.3,
     cex.lab=1.2,cex=1.2,pch=15+k_4_50$cluster,col=k_4_50$cluster)
points(x1~x2,data=k_4_50$centers,pch=10,cex=1.8,col="purple")

plot(x1~x2,data=data_clust,cex.axis=1.3,
     cex.lab=1.2,cex=1.2)
points(x1~x2,data=k_3$centers,pch=10,cex=1.8,col="orange")
points(x1~x2,data=k_3_50$centers,pch=10,cex=1.8,col="purple")

plot(x1~x2,data=data_clust,cex.axis=1.3,
     cex.lab=1.2,cex=1.2)
points(x1~x2,data=k_4$centers,pch=10,cex=1.8,col="orange")
points(x1~x2,data=k_4_50$centers,pch=10,cex=1.8,col="purple")

tots <- c()
labs <- c()
for (i in 2:10){
  test <- kmeans(data_clust,i,nstart=50)
  tots[i-1]<-test$tot.withinss
  labs[i-1]<-i
}
plot(labs,tots,type='b')

CH <- c()
for (i in 2:10){
  test <- kmeans(data_clust,i,nstart=50)
  CH[i-1]<-(test$betweenss/(i-1))/(test$tot.withinss/(nrow(data_clust)-i))
}
plot(labs,CH,type='b')
