library(cluster.datasets)
data(acidosis.patients)
example <- hclust(dist(acidosis.patients))

plot(example,xlab="",main="Example Clustering")
rect.hclust(example,k=6)

library(ggdendro)
data(USArrests)
dd <- dist(scale(USArrests),method="euclidian")
hc <- hclust(dd)
ggdendrogram(hc)
