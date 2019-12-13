Tibet<-source("F:\\RSPCMA\\Data\\chap7tibetskull.dat")$value
#
attach(Tibet)

# K-Means Clustering with 2 clusters
  set.seed(5)#randomly assigning five points
  Clust <- kmeans(Tibet[,-6], 2)


# Cluster Plot against 1st 2 principal components

  library(cluster)
  clusplot(Tibet[,-6], Clust$cluster, color=TRUE, shade=FALSE, 
  	labels=2, lines=0,lwd=2)

# Comparison with existing Types (1, 2)
  table(Clust$cluster, Type)
# 
# Hierarchical clustering using Complete Linkage
set.seed(8)#
  hc <- hclust(dist(Tibet[,-6], method = "euclidean"), method = "complete" )

# Plot the possible dendrograms
   plot(hc, cex = 0.6, hang = -1, main = "Dendrogram using complete linkage")

# Cut tree into 2 groups, b/c we have only two clusters
  Grp <- cutree(hc, k = 2)

# plotting new clusters

  library(cluster)
  clusplot(Tibet[,-6], Grp, color=TRUE, shade=FALSE, 
  	labels=2, lines=0,lwd=2)

# Comparison with existing Types (1, 2)
  table(Grp,Type)
