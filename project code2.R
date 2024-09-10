library(readxl)
library(get_eig)
library(factoextra)
data <- read_excel("C:/Users/aaron/Downloads/Real_Estate_data.xlsx")
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
