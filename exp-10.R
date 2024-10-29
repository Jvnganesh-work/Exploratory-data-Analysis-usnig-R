library(datasets) 
library(ggplot2) 
library(cluster) 
library(factoextra) 
print("21BDS0085  JVNGANESH") 
data("iris") 
# Standardize the data (important for clustering) 
# Remove the species column for clustering (since it is categorical) 
iris_data <- iris[, -5] 
iris_scaled <- scale(iris_data) 

# Compute the distance matrix 
d <- dist(iris_scaled, method = "euclidean") 

# Perform hierarchical clustering using  complete linkage 
hc_complete <- hclust(d, method = 
                        "complete") 

# Perform hierarchical clustering using single linkage 
hc_single <- hclust(d, method = "single") 

# Perform hierarchical clustering using average linkage 
hc_average <- hclust(d, method = "average") 

# Plot the dendrogram for complete linkage 
plot(hc_complete, main = "Dendrogram - 
Complete Linkage", xlab = "", sub = "", cex = 
       0.9) 

# Plot the dendrogram for single linkage 
plot(hc_single, main = "Dendrogram - Single 
Linkage", xlab = "", sub = "", cex = 0.9) 

# Plot the dendrogram for average linkage 
plot(hc_average, main = "Dendrogram - 
Average Linkage", xlab = "", sub = "", cex = 0.9) 

# Cut the dendrogram into 3 clusters 
clusters <- cutree(hc_complete, k = 3) 

# Visualize the clusters 
fviz_cluster(list(data = iris_scaled, cluster = 
                    clusters), 
             geom = "point", stand = FALSE, 
             ellipse.type = "convex",  
             ggtheme = theme_minimal(), main = 
               "Cluster Plot - Complete Linkage") 

# Elbow Method to find the optimal number of clusters 
fviz_nbclust(iris_scaled, FUN = hcut, method 
             = "wss") + 
  geom_vline(xintercept = 3, linetype = 2) +  
  labs(subtitle = "Elbow Method") 

# Silhouette Method to validate the clusters 
sil <- silhouette(clusters, dist = d) 
fviz_silhouette(sil)
