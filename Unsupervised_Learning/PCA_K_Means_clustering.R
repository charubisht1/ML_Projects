set.seed(600)
library(readxl)
data.raw <- read_excel("Data_Cortex_Nuclear.xls")
dim(data.raw)
library(dplyr)
library(tidyr)
data <- data.raw %>%
  mutate_if(is.numeric, ~replace_na(., mean(., na.rm = TRUE)))
sum(is.na(data)) == 0

attach(data)
data$Genotype <- as.factor(Genotype)
data$Treatment <- as.factor(Treatment)
data$Behavior <- as.factor(Behavior)
data$class <- as.factor(class)
detach(data)

data <- data[-1] # Drop MouseID
names(data)

#apply pca

Y <- data[,c(78:81)]
X <- data[,-c(78:81)]
pca.out <- prcomp(X, scale = TRUE)
head(pca.out[0])

#variance explained

var_explained <- pca.out$sdev / sum(pca.out$sdev)
cum_var_explained <- cumsum(var_explained)

plot(var_explained)

plot(cum_var_explained)
abline(h = 0.9, col = "red")

plot(pca.out)


#k-means
km.out <- kmeans(pca.out$x, 2, nstart = 15)
km.out$cluster


pc1 <- pca.out$x[,1]
pc2 <- pca.out$x[,2]

kmeans_2 <- data.frame(pc1, pc2, km.out$cluster)
head(kmeans_2)

plot(pc1, pc2, col = km.out$cluster, pch = 19)

plot(pc1,pc2, col = df_2$Genotype, pch =19)
plot(pc1,pc2, col = df_2$Treatment, pch =19)
plot(pc1,pc2, col = df_2$Behavior, pch =19)
plot(pc1,pc2, col = df_2$class, pch =19)
#behavior cluster more clear

for (i in c(1:10)){
  km.out <- kmeans(pca.out$x, i, nstart = 15)
  plot(pc1, pc2, col = km.out$cluster, pch = 19)
}
#3-4 cluster are perfectly seperable


#k-means with elbow
names(km.out)
#Get the total within cluster sum of squares.
km.out$tot.withinss


result <- c()
nr_clusters <- c(1:30)
for (i in nr_clusters){
  km.out <- kmeans(pca.out$x, i, nstart = 15)
  result <- c(result,km.out$tot.withinss)
}

plot(nr_clusters, result)


#5 clusters
km.out <- kmeans(pca.out$x, 4, nstart = 15)
plot(pc1, pc2, col = km.out$cluster, pch = 19)

km.result <- data.frame(X, km.out$cluster)
names(km.result)

c1 <- km.result[km.result$km.out.cluster == 1,]
c2 <- km.result[km.result$km.out.cluster == 2,]
c3 <- km.result[km.result$km.out.cluster == 3,]
c4 <- km.result[km.result$km.out.cluster == 4,]
rbind(lapply(c1, mean), lapply(c2, mean), lapply(c3,mean), lapply(c4,mean))

#heirarchiacal
methods <- c("single", "average", "complete","centroid")
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "black")
for (m in methods){
  
  hc <- hclust(dist(pca.out$x), method = m)
  plot(hc, ylab = "Height", nodePar = nodePar, leaflab = "none")
}

hc <- hclust(dist(pca.out$x), method = "complete")

for (i in c(1:10)){
  hc.cut <- cutree(hc, i)
  print(table(hc.cut))
}

