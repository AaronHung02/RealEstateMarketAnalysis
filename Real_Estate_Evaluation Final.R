# Installing and loading the required packages
library(car)
library(ggcorrplot)
library(MASS)
library(performance)
library(readxl)
library(see)
library(cluster)
library(factoextra)

# Regression Analysis
# Importing data and renaming columns for easier use
data <- read_excel("C:/Users/aaron/Downloads/Real_Estate_data.xlsx")
names(data) = gsub(" ", "_", names(data))
head(data)

# Pair plot
data_reduced <- subset(data, select = -No)
pairs(data_reduced)

# Forming the model
data_model = lm(Y_house_price_of_unit_area ~ X1_transaction_date + X2_house_age + 
                  X3_distance_to_the_nearest_MRT_station + X4_number_of_convenience_stores + 
                  X5_latitude + X6_longitude, data)
summary(data_model)

# Checking for potential problems
par(mfrow=c(2,3))
plot(data_model, 1:6)

# Checking for collinearity
data_model_reduced <- subset(data, select = c(-Y_house_price_of_unit_area, -No))
corr_matrix = round(cor(data_model_reduced), 2)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

# Checking for multicollinearity
vif(data_model)

# Stepwise Regression
stepforward <- stepAIC(data_model, direction="forward")
stepforward$anova 
stepback <- stepAIC(data_model, direction="back")
stepback$anova 

# Forming the model best selection of predictors
data_model_best = lm(Y_house_price_of_unit_area ~ X1_transaction_date + X2_house_age + 
                       X3_distance_to_the_nearest_MRT_station + X4_number_of_convenience_stores + 
                       X5_latitude, data)
summary(data_model_best)

# Checking for potential problems for best selection of predictors
plot(data_model_best, 1:6)

# Checking for collinearity for best selection of predictors
data_model_best_reduced <- subset(data, select = c(-Y_house_price_of_unit_area, -No, -X6_longitude))
corr_matrix_best = round(cor(data_model_best_reduced), 2)
ggcorrplot(corr_matrix_best, hc.order = TRUE, type = "lower", lab = TRUE)

# Checking for multicollinearity for best selection of predictors
vif(data_model_best)

# PCA 
copy_data = data
head(copy_data)
copy_data <- copy_data[,3:5]

pca = prcomp(copy_data, scale = TRUE)
summary(pca)

eigen_vals = get_eig(pca)
eigen_vals

fviz_eig(pca, addlabels = TRUE)

fviz_cos2(pca, choice = "var", axes = 1:2)

fviz_pca_var(pca, col.var="cos2", gradient.cols = c("black","orange","green"),repel=TRUE)

loadings <- pca$rotation[,1:2]
loadings

eigenvalues <- pca$sdev^2

num_components <- sum(eigenvalues > 1)
num_components

# Clustering
# Clean data
data <- read_excel("C:/Users/aaron/Downloads/Real_Estate_data.xlsx")

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
