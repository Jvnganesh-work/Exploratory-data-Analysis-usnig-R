#21BDS0085 
#Experiment-7 
library(ggplot2) 
library(dplyr) 

data("iris") 

# Display basic summary statistics 
summary(iris) 

# Z-score method to detect outliers 
z_scores <- as.data.frame(scale(iris[, 1:4]))  # Scaling numeric columns 

# Set Z-score threshold 
threshold <- 3 
outliers_zscore <- rowSums(abs(z_scores) > threshold) > 0 
outliers_data <- iris[outliers_zscore, ] 

print("Detected outliers based on Z-score:") 
print(outliers_data) 

# Visualization 1: Boxplot to visualize outliers 
boxplot(iris[, 1:4], main = "Boxplot to Visualize Outliers", col = "lightblue") 

# Visualization 2: Sepal Length distribution with outliers highlighted 
ggplot(iris, aes(x = Sepal.Length)) + 
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "lightblue", color = "black") + 
  geom_density(color = "red") + 
  geom_vline(aes(xintercept = mean(Sepal.Length)), color = "green", linetype = "dashed", 
             linewidth = 1) + 
  ggtitle("Distribution of Sepal Length with Outliers") + 
  theme_minimal() 

# IQR (Interquartile Range) method to detect outliers 
Q1 <- apply(iris[, 1:4], 2, quantile, 0.25) 
Q3 <- apply(iris[, 1:4], 2, quantile, 0.75) 
IQR <- Q3 - Q1 

# Define outlier condition using IQR 
outliers_iqr <- iris %>% 
  filter(if_any(everything(), ~ . < (Q1 - 1.5 * IQR) | . > (Q3 + 1.5 * IQR))) 

print("Outliers detected using the IQR method:") 
print(outliers_iqr) 

# Visualization 3: Scatter plot for Petal Length vs Petal Width highlighting outliers 
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = outliers_zscore)) + 
  geom_point() + 
  ggtitle("Scatter Plot of Petal Length vs Petal Width with Outliers Highlighted") + 
  theme_minimal() + 
  scale_color_manual(values = c("black", "red"))