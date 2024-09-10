library(readxl)
library(cluster)
library(factoextra)
set.seed(10)
data <- read_excel("C:/Users/aaron/Downloads/Real_Estate_data.xlsx")

head(data)
# Clean data
data <- data[, -which(names(data) == "No")]
data <- data[, -which(names(data) == "Y house price of unit area")]
data <- data[, -which(names(data) == "X1 transaction date")]
data <- data[, -which(names(data) == "X5 latitude")]
data <- data[, -which(names(data) == "X6 longitude")]
data
# Scale Data
df <- scale(data)
df

#These correlation coefficients show that Average linkage method creates a dendrogram that represents the original distances the best
res.dist <- dist(df, method = "euclidian")

methods <- c("ward.D2", "average", "ward.D", "single", "complete", "mcquitty", "median", "centroid")
correlations <- numeric(length(methods))
cophenetic_matrices <- list()

for (i in 1:length(methods)) {
  res.hc <- hclust(res.dist, method = methods[i])
  res.coph <- cophenetic(res.hc)
  cophenetic_matrices[[i]] <- res.coph
  correlations[i] <- cor(res.dist, res.coph)
}


data[3]
results <- data.frame(Method = methods,
                      Correlation = correlations)

print(results)

# Agglomerative Hierarchical clustering using average method
d <- dist(df, method = "euclidean")

final_clust <- hclust(d, method = "average" )
final_clust

plot(final_clust, main = "Agglomerative Hierarchical Clustering")
groups <- cutree(final_clust, k=4)

table(groups)

final_data <- cbind(data, cluster = groups)

head(final_data)

aggregate(df, by=list(cluster=final_data$cluster), mean)

pairs(df,gap=0,pch=groups, col= c("red","blue","green"))

fviz_cluster(list(data=df,cluster=groups),repel=TRUE,main = "Agglomerative Cluster Plot")

#K-Means Clustering
fviz_nbclust(df, kmeans,nstart=25,method = "wss")

km.res <- kmeans(df, centers = 4, nstart = 25)
km.res

aggregate(df, by=list(cluster= km.res$cluster),mean)

cl<-km.res$cluster
pairs(df,gap=0,pch=cl,col=c("red","blue","green"))

fviz_cluster(km.res,data=df,palette=c("red","blue","green","yellow"),ellipse.type="euclid",star.plot=TRUE,repel=TRUE,main="K-Means Cluster Plot")
# K_medoids clustering

fviz_nbclust(df,pam,method="silhouette")

k_medoid <- pam(df,k=4)
k_medoid

fviz_cluster(k_medoid, data= df,repel=TRUE, main= "K-Medoids Cluster Plot")

final_data<- cbind(data, cluster=k_medoid$cluster)
final_data

aggregate(df, by = list(cluster = k_medoid$cluster), mean)

library(NbClust)

nb <- NbClust(df,distance="euclidean",min.nc=2,max.nc=10,method="kmeans")
nb <- NbClust(df,distance="euclidean",min.nc=2,max.nc=10,method="ward.D2")
nb <- NbClust(df,distance="euclidean",min.nc=2,max.nc=10,method="average")



library(clValid)
clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df,nClust = 2:5, clMethods = clmethods,validation = "internal")
summary(intern)

stab<- clValid(df,nClust=2:5,clMethods= clmethods,validation="stability")
summary(stab)
