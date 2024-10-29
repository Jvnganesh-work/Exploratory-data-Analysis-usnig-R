library(corrplot) 
library(rpart) 

# Load the dataset 
data(mtcars) 

# 1. Overview of the data 
head(mtcars) 
summary(mtcars) 
str(mtcars) 

# 2. Descriptive Statistics 
mean_vals <- sapply(mtcars, mean) 
median_vals <- sapply(mtcars, median) 
sd_vals <- sapply(mtcars, sd) 
var_vals <- sapply(mtcars, var) 
correlation_matrix <- cor(mtcars) 

# 3. Visualizations 
# Univariate 
hist(mtcars$mpg, main="Histogram of MPG", 
     xlab="Miles Per Gallon", col="lightblue") 
boxplot(mtcars$mpg, main="Boxplot of MPG", 
        ylab="Miles Per Gallon", col="orange") 

# Bivariate 
plot(mtcars$wt, mtcars$mpg, main="Weight vs MPG", 
     xlab="Weight", ylab="MPG", pch=19, col="blue") 
boxplot(mpg ~ cyl, data=mtcars, main="MPG by 
Cylinder", xlab="Number of Cylinders", ylab="MPG", 
        col=c("lightgreen", "lightblue", "lightcoral")) 

# Multivariate 
pairs(mtcars, main="Pair Plot of mtcars Data", pch=21, 
      bg=c("red", "green3", "blue")[unclass(mtcars$cyl)]) 
corrplot(correlation_matrix, method="circle") 

# Advanced Visualization 
heatmap(correlation_matrix, main="Correlation 
Heatmap", col=topo.colors(10)) 

# Principal Component Analysis (PCA) 
pca <- prcomp(mtcars, scale=TRUE) 
biplot(pca) 

# 4. Outlier Detection 
boxplot(mtcars$mpg, main="Boxplot for Outlier 
Detection") 
mtcars$mpg_zscore <- (mtcars$mpg - 
                        mean(mtcars$mpg)) / sd(mtcars$mpg) 
mtcars$mpg_outlier <- 
  ifelse(abs(mtcars$mpg_zscore) > 3, "Outlier", "Not 
Outlier") 
table(mtcars$mpg_outlier) 

# 5. Handling Missing Data (Example) 
# Check for missing values 
colSums(is.na(mtcars)) 
# Impute missing values with mean (if any) 
# mtcars$mpg[is.na(mtcars$mpg)] <- 
mean(mtcars$mpg, na.rm = TRUE) 

# 6. Feature Engineering 
# Creating new variable based on weight 
mtcars$wt_category <- ifelse(mtcars$wt > 3, "Heavy", 
                             "Light") 
# Binning the mpg variable 
mtcars$mpg_bin <- cut(mtcars$mpg, breaks = c(10, 
                                             15, 20, 25, 30, 35), labels = c("10-15", "15-20", "20-25", 
                                                                             "25-30", "30-35")) 

# 7. Hypothesis Testing 
# t-test 
t_test_result <- t.test(mpg ~ cyl, data = mtcars) 
# ANOVA 
anova_result <- anova(lm(mpg ~ cyl + wt + hp, data = 
                           mtcars)) 

# 8. Model Building 
# Linear Regression 
lm_model <- lm(mpg ~ wt + hp + cyl, data=mtcars) 
summary(lm_model) 

# Decision Tree 
tree_model <- rpart(mpg ~ ., data=mtcars, 
                    method="anova") 
plot(tree_model) 
text(tree_model) 

# Clustering 
kmeans_result <- kmeans(mtcars[, c("mpg", "wt", 
                                   "hp")], centers = 3) 
plot(mtcars$mpg, mtcars$wt, col = 
       kmeans_result$cluster, main="K-means Clustering", 
     xlab="MPG", ylab="Weight") 