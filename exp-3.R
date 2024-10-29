# Load required libraries 
library(dlookr) 
library(dplyr) 
library(tidyr) 
library(ggplot2) 

print("21BDS0085 JvnGanesh") 
# Load the iris dataset 
data <- iris 
print(head(data)) 

print("21BDS0085 JvnGanesh") 
# Overview of the Data 
str(data) 
summary(data) 
glimpse(data) 

print("21BDS0085 JvnGanesh") 
# Check for Missing Values and Impute 
na_variables <- find_na(data) 
print(na_variables) 

print("21BDS0085 JvnGanesh") 
# Since the iris dataset does not contain missing values, no imputation is required here 
# However, if there were missing values, the following line would impute them: 
# data_imputed <- imputate_na(data) 

print("21BDS0085 JvnGanesh") 
# Check for Outliers and Impute 
outlier_variables <- find_outliers(data) 
print(outlier_variables) 

print("21BDS0085 JvnGanesh") 
# Plot Outliers 
plot_outlier(data) 

# Identify Outliers 
outlier_variables <- find_outliers(data) 
print(outlier_variables) 

# Custom Plot for Outliers using ggplot2 
ggplot(data, aes(x = Species, y = Sepal.Length)) +  
  geom_boxplot() +  
  ggtitle("Boxplot of Sepal Length by Species") +  
  theme_minimal() 

ggplot(data, aes(x = Species, y = Sepal.Width)) +  
  geom_boxplot() +  
  ggtitle("Boxplot of Sepal Width by Species") +  
  theme_minimal() 

ggplot(data, aes(x = Species, y = Petal.Length)) +  
  geom_boxplot() +  
  ggtitle("Boxplot of Petal Length by Species") +  
  theme_minimal() 

ggplot(data, aes(x = Species, y = Petal.Width)) +  
  geom_boxplot() +  
  ggtitle("Boxplot of Petal Width by Species") +  
  theme_minimal() 
########### 

# Install the e1071 package if you haven't 
install.packages("e1071") 

# Load the library 
library(e1071) 

# Calculate skewness and plot 
skewness_values <- data_outliers_imputed %>% 
  select(where(is.numeric)) %>% 
  summarise(across(everything(), skewness)) 

print(skewness_values) 

# Plotting skewness 
ggplot(gather(skewness_values), aes(x = key, y = value)) +  
  geom_bar(stat = "identity") + 
  ggtitle("Skewness of Numeric Variables") + 
  ylab("Skewness") + 
  xlab("Variables") + 
  theme_minimal() 

print("21BDS0085 JvnGanesh") 
# Function to Impute Outliers Manually Using IQR 
impute_outliers_iqr <- function(x) { 
  Q1 <- quantile(x, 0.25, na.rm = TRUE) 
  Q3 <- quantile(x, 0.75, na.rm = TRUE) 
  IQR <- Q3 - Q1 
  
  # Define the lower and upper bounds 
  lower_bound <- Q1 - 1.5 * IQR 
  upper_bound <- Q3 + 1.5 * IQR 
  
  # Replace outliers with the median 
  x[x < lower_bound] <- median(x, na.rm = TRUE) 
  x[x > upper_bound] <- median(x, na.rm = TRUE) 
  
  return(x) 
} 

# Apply to Numeric Columns 
data_outliers_imputed <- data %>% 
  mutate(across(where(is.numeric), impute_outliers_iqr)) 

print(summary(data_outliers_imputed)) 
########## 
# Plot distribution of each numeric variable 
data_outliers_imputed %>% 
  select(where(is.numeric)) %>% 
  gather(key = "Variable", value = "Value") %>% 
  ggplot(aes(x = Value)) +  
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) + 
  facet_wrap(~Variable, scales = "free_x") + 
  ggtitle("Distribution of Numeric Variables") + 
  theme_minimal() 

######### 
print("21BDS0085 JvnGanesh") 
# Check for Skewness and Transform 
skewed_variables <- find_skewness(data_outliers_imputed) 
print(skewed_variables) 

print("21BDS0085 JvnGanesh") 
# Plot Skewness 
plot_skewness(data_outliers_imputed) 

print("21BDS0085 JvnGanesh") 
# Apply log transformation only to numeric variables 
data_transformed <- data_outliers_imputed %>% 
  mutate(across(where(is.numeric), log)) 

# Check the transformed data 
summary(data_transformed) 

# Plot the transformed data distributions 
data_transformed %>% 
  select(where(is.numeric)) %>% 
  gather(key = "Variable", value = "Value") %>% 
  ggplot(aes(x = Value)) +  
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) + 
  facet_wrap(~Variable, scales = "free_x") + 
  ggtitle("Log-Transformed Numeric Variables") + 
  theme_minimal() 


print("21BDS0085 JvnGanesh") 

print("21BDS0085 JvnGanesh") 
# Apply Z-score standardization only to numeric variables 
data_standardized <- data_transformed %>% 
  mutate(across(where(is.numeric), ~ scale(.) %>% as.vector())) 

# Check the standardized data 
summary(data_standardized) 

# Plot the standardized data distributions 
data_standardized %>% 
  select(where(is.numeric)) %>% 
  gather(key = "Variable", value = "Value") %>% 
  ggplot(aes(x = Value)) +  
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) + 
  facet_wrap(~Variable, scales = "free_x") + 
  ggtitle("Standardized Numeric Variables") + 
  theme_minimal() 


print("21BDS0085 JvnGanesh") 

# Define a function to bin data into 3 bins (low, medium, high) 
bin_data <- function(x, bins = 3) { 
  cut(x, breaks = bins, labels = paste("Bin", 1:bins), include.lowest = TRUE) 
} 

# Apply binning to all numeric columns 
data_binned <- data_standardized %>% 
  mutate(across(where(is.numeric), bin_data)) 

# Check the binned data 
summary(data_binned) 

# View a sample of the binned data 
print(head(data_binned)) 


print("21BDS0085 JvnGanesh") 
# Transformation Report 
transformation_web_report(data_binned, output_format = "html") 
transformation_paged_report(data_binned) 

print("21BDS0085 JvnGanesh") 
# Arrange Observations by a Specific Variable 
data_arranged <- arrange(data_binned, Sepal.Length) 
print(head(data_arranged)) 

print("21BDS0085 JvnGanesh") 
# Select Specific Columns 
selected_data <- select(data_arranged, Sepal.Length, Sepal.Width, Species) 
print(head(selected_data)) 

print("21BDS0085 JvnGanesh") 
# Filter Observations Based on Values 
filtered_data <- filter(data_arranged, Species == "setosa") 
print(head(filtered_data)) 

print("21BDS0085 JvnGanesh") 
# Gather (Convert Wide Data to Long Format) 
gathered_data <- gather(data_arranged, key = "Measurement", value = "Value", -Species) 
print(head(gathered_data)) 

print("21BDS0085 JvnGanesh") 
# Spread (Convert Long Data to Wide Format) 
spread_data <- spread(gathered_data, key = "Measurement", value = "Value") 
print(head(spread_data)) 

print("21BDS0085 JvnGanesh") 
# Group Data by a Variable and Summarize 
grouped_data <- data_arranged %>% 
  group_by(Species) %>% 
  summarize(mean_sepal_length = mean(Sepal.Length), mean_sepal_width = 
              mean(Sepal.Width)) 
print(grouped_data) 

print("21BDS0085 JvnGanesh") 
# Mutate (Create New Variables) 
mutated_data <- mutate(data_arranged, Sepal.Ratio = Sepal.Length / Sepal.Width) 
print(head(mutated_data)) 

########################################################################
############################# 
# Load necessary libraries 
install.packages("corrplot") 
install.packages("naniar") 
install.packages("DataExplorer") 
install.packages("Hmisc") 
install.packages("caret") 
install.packages("ggcorrplot") 
install.packages("psych") 
library(dplyr) 
library(ggplot2) 
library(corrplot) 
library(DataExplorer) 
library(Hmisc) 
library(caret) 
library(psych) 

# View basic summary statistics 
summary(data) 

# View structure of the dataset 
str(data) 

# Check for missing values 
sum(is.na(data)) 

# Visualize missing data 
library(naniar) 
gg_miss_var(data) + theme_minimal() 

# Plot distributions of all numeric variables 
data %>% 
  select(where(is.numeric)) %>% 
  gather(key = "Variable", value = "Value") %>% 
  ggplot(aes(x = Value)) + 
  geom_histogram(bins = 30, fill = "blue", color = "black", alpha = 0.7) + 
  facet_wrap(~ Variable, scales = "free_x") + 
  theme_minimal() 

# Correlation matrix 
cor_matrix <- cor(data %>% select(where(is.numeric)), use = "complete.obs") 
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8) 

# Pair plot (scatterplot matrix) 
pairs(data %>% select(where(is.numeric))) 

# Create a correlation heatmap 
library(ggcorrplot) 
corrplot <- cor(data %>% select(where(is.numeric))) 
ggcorrplot(corrplot, lab = TRUE) 

# Create a QQ plot for checking normality 
ggplot(data, aes(sample = Sepal.Length)) + 
  stat_qq() + 
  stat_qq_line() + 
  ggtitle("QQ Plot of Sepal Length") + 
  theme_minimal() 

# Outlier detection with boxplots 
ggplot(data, aes(x = Species, y = Sepal.Length)) +  
  geom_boxplot() +  
  ggtitle("Boxplot of Sepal Length by Species") +  
  theme_minimal() 

# Visualize distributions using density plots 
ggplot(data, aes(x = Sepal.Length, fill = Species)) +  
  geom_density(alpha = 0.5) + 
  theme_minimal() 

# Visualize relationships with scatterplots 
ggplot(data, aes(x = Sepal.Length, y = Petal.Length, color = Species)) +  
  geom_point() + 
  theme_minimal() 

# Generate a report using DataExplorer 
create_report(data) 

# Check multicollinearity (Variance Inflation Factor - VIF) 
vif(lm(Sepal.Length ~ ., data = data)) 

# Summary statistics using the psych package 
describe(data) 

# Check for zero variance predictors 
nearZeroVar(data, saveMetrics = TRUE) 

# Detect highly correlated variables and remove them 
highly_correlated <- findCorrelation(cor_matrix, cutoff = 0.75) 
data_reduced <- data[, -highly_correlated] 

# Scatterplot matrix (SPLOM) using the psych package 
pairs.panels(data %>% select(where(is.numeric)), method = "pearson", hist.col = "blue") 

# Visualize the distribution of all variables using plotly 
install.packages("plotly") 
library(plotly) 
plot_ly(data, x = ~Sepal.Length, y = ~Petal.Length, color = ~Species, type = "scatter", mode 
        = "markers") 

# Visualize data using a parallel coordinates plot 
install.packages("MASS") 
library(MASS) 
parcoord(data %>% select(where(is.numeric)), col = as.factor(data$Species)) 

# Binning continuous variables 
data_binned <- data %>% 
  mutate(Sepal.Length.Binned = cut(Sepal.Length, breaks = 3, labels = c("Short", "Medium", 
                                                                        "Long"))) 

# Group by and summarize data 
grouped_summary <- data %>% 
  group_by(Species) %>% 
  summarize(mean_sepal_length = mean(Sepal.Length), mean_petal_length = 
              mean(Petal.Length)) 

# Plot a heatmap for categorical variables 
install.packages("pheatmap") 
library(pheatmap) 
data_cat <- data %>% 
  mutate_if(is.factor, as.numeric) 
pheatmap(cor(data_cat)) 

# Feature engineering: Create new variable based on existing data 
data <- data %>% 
  mutate(Sepal.Ratio = Sepal.Length / Sepal.Width) 

# Extracting insights using Hmisc::describe 
describe(data)