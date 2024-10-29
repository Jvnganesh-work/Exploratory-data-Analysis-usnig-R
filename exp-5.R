print("21BDS0085 JVNGANESH") 
#  
# #Install necessary libraries (uncomment if not already installed) 
# install.packages("Hmisc")     # For single variable imputation 
# install.packages("mice")      # For multiple imputation methods 
# install.packages("VIM")       # For visualizing missing data 
# install.packages("dplyr")     # For data manipulation 


# Load necessary libraries 
library(dlookr) 
library(dplyr) 
library(ggplot2) 
library(mice) 
library(naniar)  # For visualizing missing data 
library(Hmisc)   # For single variable imputation 
library(VIM)     # For missing data visualization 

# Load default dataset - airquality 
data("airquality") 
print("21BDS0085 JVNGANESH") 

# 1. View basic summary 
print("Basic Summary of airquality dataset:") 
summary(airquality) 

# 2. Data Transformation (Standardization and Log Transformation) 
airquality$Ozone_scaled <- as.numeric(scale(airquality$Ozone, center = TRUE, scale = 
                                              TRUE)) 
print("Standardized Ozone column:") 
print(head(airquality$Ozone_scaled, 10)) 

airquality$Log_Wind <- log(airquality$Wind + 1) 
print("Log transformed Wind column:") 
print(head(airquality$Log_Wind, 10)) 

# 3. Imputation of Missing Values 
airquality$Ozone[is.na(airquality$Ozone)] <- mean(airquality$Ozone, na.rm = TRUE) 
airquality$Solar.R[is.na(airquality$Solar.R)] <- median(airquality$Solar.R, na.rm = TRUE) 

airquality <- airquality %>% 
  group_by(Month) %>% 
  mutate(Ozone = ifelse(is.na(Ozone), median(Ozone, na.rm = TRUE), Ozone)) 

# 4. Multiple Imputation Using MICE 
set.seed(123)  # Initialize random seed 
airquality_for_imputation <- airquality %>% select(-Ozone_scaled) 
airquality_imputed <- mice(airquality_for_imputation, method = 'pmm', m = 5) 
completed_airquality <- complete(airquality_imputed) 
print("Completed airquality dataset after multiple imputation:") 
print(completed_airquality) 

# 5. Exploratory Data Analysis (EDA) 
summary(airquality) 

ggplot(airquality, aes(x = Wind, y = Ozone)) + 
  geom_point() + 
  geom_smooth(method = 'lm') + 
  labs(title = 'Scatter Plot of Wind vs Ozone') 

# 6. Data Cleaning and Outlier Detection 
Q1 <- quantile(airquality$Ozone, 0.25, na.rm = TRUE) 
Q3 <- quantile(airquality$Ozone, 0.75, na.rm = TRUE) 
IQR <- Q3 - Q1 
outliers <- which(airquality$Ozone < (Q1 - 1.5 * IQR) | airquality$Ozone > (Q3 + 1.5 * IQR)) 
print("Detected Outliers:") 
print(airquality[outliers, ]) 

# 7. Diagnose Missing Values using dlookr 
airquality %>% diagnose() %>% print() 

# Visualize missing data using naniar 
gg_miss_var(airquality) 

# 8. Regression-based Imputation 
ozone_model <- lm(Ozone ~ Wind + Temp, data = airquality) 
airquality$Ozone[is.na(airquality$Ozone)] <- predict(ozone_model, newdata = 
                                                       airquality[is.na(airquality$Ozone), ]) 
print("After regression-based imputation of Ozone column:") 
print(head(airquality$Ozone, 10)) 

# 9. Data Visualization after Imputation 
ggplot(airquality, aes(x = Ozone)) + 
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) + 
  labs(title = "Distribution of Ozone after Imputation") 

# Step 10: Additional Imputation Techniques 

# Imputation with Constant Value (Wind column) 
airquality$Wind <- impute(airquality$Wind, 5) 

# Visualize Missing Data Patterns using VIM 
aggr_plot <- aggr(airquality, col = c('navyblue', 'yellow'), numbers = TRUE, sortVars = TRUE, 
                  labels = names(airquality), cex.axis = .7, gap = 3, ylab = c("Missing data", "Pattern")) 

# Imputation for Entire Dataset using Median 
all_column_median <- apply(airquality, 2, median, na.rm = TRUE) 
for (i in colnames(airquality)) { 
  airquality[, i][is.na(airquality[, i])] <- all_column_median[i] 
} 

# View the dataset after global median imputation 
print("Dataset after Global Median Imputation:") 
head(airquality) 