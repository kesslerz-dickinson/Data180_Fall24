library(MASS)
library(ggplot2)
library(dplyr)

data(diamonds)
head(diamonds)

dist_data <- diamonds %>% filter(cut=="Premium",color %in% c("J","K","L")) %>% select(carat,depth,price,x,y,z)

clust <- dist_data %>% as.matrix() %>% dist(method="minkowski",p=1.5) %>% hclust(method="single")

plot(clust,xlab="",main="Clustering Test Diamonds")
rect.hclust(clust,k=4)



dist_data2 <- diamonds %>% group_by(cut) %>% summarize(m_carat=mean(carat),m_depth=mean(depth),m_price=mean(price)) %>% select(m_carat,m_depth,m_price)
clust2 <- dist_data2 %>% as.matrix() %>% dist(method="euclidian") %>% hclust(method="complete")
plot(clust2,xlab="",main="Clustering Test Diamonds")
rect.hclust(clust,k=2)

                                 