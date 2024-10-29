# Load necessary libraries 
library(ggplot2)  # For data visualization 
library(dplyr)    # For data manipulation 
library(cluster)  # For clustering evaluation 
# metrics like silhouette 
library(factoextra)  # For visualizing clusters 
library(GGally)  

print("21BDS0085 JvnGanesh") 

# Function to perform detailed K-means analysis 
perform_detailed_kmeans_analysis <- function() { 
# Step 1: Load the iris dataset (without the Species column) 
data <- iris %>% select(-Species)  # 
#Removing the Species column for clustering 
cat("\nPerforming analysis on the built-in iris 
dataset\n") 

# Step 2: Basic dataset exploration 
cat("\nDataset Summary:\n") 
print(summary(data)) 

# Step 3: Prompt the user for the number of 
clusters (k) 
k <- as.numeric(readline(prompt = "Enter 
the number of clusters (k): ")) 

# Step 4: Perform K-means clustering 
set.seed(123)  # Setting seed for 
reproducibility 
kmeans_result <- kmeans(data, centers = k) 

# Step 5: Analyze and display cluster 
characteristics 
cat("\nK-means Clustering Results:\n") 
cat("Cluster Centers:\n") 
print(kmeans_result$centers)  # Print the centers of clusters 

cat("\nCluster Sizes:\n") 
print(kmeans_result$size)  # Print the number of points in each cluster 

# Step 6: Evaluate clusters using silhouette analysis 
cat("\nEvaluating the quality of clusters 
using silhouette analysis...\n") 
silhouette_avg <- 
  silhouette(kmeans_result$cluster, dist(data)) 
print(summary(silhouette_avg)) 

# Step 7: Visualization - PCA plot of clusters 
cat("\nVisualizing clusters using PCA...\n") 
data$Cluster <- 
  as.factor(kmeans_result$cluster) 

pca <- prcomp(data[, -ncol(data)], scale = 
                TRUE)  # Perform PCA for visualization 
data_pca <- data.frame(PC1 = pca$x[, 1], 
                       PC2 = pca$x[, 2], Cluster = data$Cluster) 

ggplot(data_pca, aes(x = PC1, y = PC2, color 
                     = Cluster)) + 
  geom_point(size = 3) + 
  labs(title = paste("K-means Clustering 
with", k, "Clusters on Iris Dataset (PCA)"), 
       x = "Principal Component 1", 
       y = "Principal Component 2") + 
  theme_minimal() 

# Step 8: Visualization - Pair plot for feature combinations 
cat("\nVisualizing pair plots for feature 
combinations...\n") 
ggpairs(data, aes(color = Cluster)) 

# Step 9: Elbow method for optimal cluster detection 
cat("\nVisualizing the Elbow Method to 
check the optimal number of clusters...\n") 
fviz_nbclust(data[, -ncol(data)], kmeans, 
             method = "wss") + 
  geom_vline(xintercept = k, linetype = 2) + 
  labs(subtitle = "Elbow method for optimal 
number of clusters") 

# Step 10: Cluster visualization with cluster centers 
cat("\nVisualizing clusters along with cluster 
centers...\n") 
fviz_cluster(kmeans_result, data = data[, 
                                        ncol(data)], geom = "point") + 
  labs(title = paste("K-means Clustering on 
Iris Dataset with", k, "Clusters"), 
       subtitle = "With Cluster Centers") + 
  theme_minimal() 
  } 

# Run the function to perform a detailed Kmeans clustering analysis 
perform_detailed_kmeans_analysis() 

#Code without k value input: 
install.packages("GGally") 
# Load necessary libraries 
library(ggplot2)  # For data visualization 
library(dplyr)    # For data manipulation 
library(cluster)  # For clustering evaluation metrics like silhouette 
library(factoextra)  # For visualizing clusters 
library(GGally)  

print("21BDS0085 JvnGanesh") 
# Function to perform detailed K-means 
analysis 
perform_detailed_kmeans_analysis <- function() { 
# Step 1: Load the iris dataset (without the Species column) 
data <- iris %>% select(-Species)  # 
#Removing the Species column for clustering 
cat("\nPerforming analysis on the built-in iris 
dataset\n") 

# Step 2: Basic dataset exploration 
cat("\nDataset Summary:\n") 
print(summary(data)) 

# Step 3: Automatically set the number of 
clusters (k) 
k <- 3  # As per domain knowledge (iris has 3 species) 

# Step 4: Perform K-means clustering 
set.seed(123)  # Setting seed for 
reproducibility 
kmeans_result <- kmeans(data, centers = k) 

# Step 5: Analyze and display cluster 
characteristics 
cat("\nK-means Clustering Results:\n") 
cat("Cluster Centers:\n") 
print(kmeans_result$centers)  # Print the centers of clusters 

cat("\nCluster Sizes:\n") 
print(kmeans_result$size)  # Print the number of points in each cluster 

# Step 6: Evaluate clusters using silhouette 
analysis 
cat("\nEvaluating the quality of clusters 
using silhouette analysis...\n") 
silhouette_avg <- 
  silhouette(kmeans_result$cluster, dist(data)) 
print(summary(silhouette_avg)) 

# Step 7: Visualization - PCA plot of clusters 
cat("\nVisualizing clusters using PCA...\n") 
data$Cluster <- 
  as.factor(kmeans_result$cluster) 

pca <- prcomp(data[, -ncol(data)], scale = 
                TRUE)  # Perform PCA for visualization 
data_pca <- data.frame(PC1 = pca$x[, 1], 
                       PC2 = pca$x[, 2], Cluster = data$Cluster) 

ggplot(data_pca, aes(x = PC1, y = PC2, color 
                     = Cluster)) + 
  geom_point(size = 3) + 
  labs(title = "K-means Clustering with 3 
Clusters on Iris Dataset (PCA)", 
       x = "Principal Component 1", 
       y = "Principal Component 2") + 
  theme_minimal() 

# Step 8: Visualization - Pair plot for feature 
combinations 
cat("\nVisualizing pair plots for feature 
combinations...\n") 
ggpairs(data, aes(color = Cluster)) 

# Step 9: Elbow method for optimal cluster 
detection 
cat("\nVisualizing the Elbow Method to 
check the optimal number of clusters...\n") 
fviz_nbclust(data[, -ncol(data)], kmeans, 
             method = "wss") + 
  geom_vline(xintercept = 3, linetype = 2) + 
  labs(subtitle = "Elbow method for optimal 
number of clusters") 

# Step 10: Cluster visualization with cluster 
centers 
cat("\nVisualizing clusters along with cluster 
centers...\n") 
fviz_cluster(kmeans_result, data = data[, 
                                        ncol(data)], geom = "point") + 
  labs(title = "K-means Clustering on Iris 
Dataset", subtitle = "With Cluster Centers") + 
  theme_minimal() 
  } 

# Run the function to perform a detailed Kmeans clustering analysis 
perform_detailed_kmeans_analysis()