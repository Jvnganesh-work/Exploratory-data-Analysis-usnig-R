library(corrplot)  # for the correlation heatmap 
library(ggplot2)   # for the scatter plot 
data("mtcars") 

# Extract the X (mpg) and Y (hp) values 
X <- mtcars$mpg  # miles per gallon 
Y <- mtcars$hp   # horsepower 

# Calculate covariance 
covariance <- cov(X, Y) 

# Calculate correlation 
correlation <- cor(X, Y) 

# Print the results 
cat("Covariance between mpg and hp:", covariance, "\n") 
cat("Correlation between mpg and hp:", correlation, "\n") 

# Visualization - Scatter plot with regression line 
plot(X, Y, main = "Scatter Plot of MPG vs Horsepower", 
     xlab = "Miles Per Gallon (MPG)", ylab = "Horsepower (HP)", 
     pch = 19, col = "blue") 

# Add a regression line 
abline(lm(Y ~ X), col = "red", lwd = 2) 

# Add a grid for better readability 
grid() 

# Calculate correlation matrix for all numeric variables in mtcars 
cor_matrix <- cor(mtcars) 

# Heatmap visualization of the correlation matrix 
corrplot(cor_matrix, method = "color", type = "lower",  
         tl.col = "black", tl.srt = 45,  
         addCoef.col = "black", number.cex = 0.8, 
         col = colorRampPalette(c("red", "white", "blue"))(200)) 

# Alternatively, for a heatmap with ggplot2, you can do: 
library(reshape2) 

# Melt the correlation matrix for ggplot2 
cor_melted <- melt(cor_matrix) 

# Plot the heatmap 
ggplot(cor_melted, aes(Var1, Var2, fill = value)) +  
  geom_tile(color = "white") + 
  scale_fill_gradient2(low = "red", high = "blue", mid = "white",  
                       midpoint = 0, limit = c(-1, 1), space = "Lab",  
                       name = "Correlation") + 
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, vjust = 1,  
                                   size = 12, hjust = 1)) + 
  coord_fixed() 