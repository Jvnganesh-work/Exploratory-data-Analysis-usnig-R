# Install and load required packages 
install.packages("psych") 
install.packages("moments") 
install.packages("ggplot2") 

library(psych) 
library(moments) 
library(ggplot2) 

# Load the mtcars dataset 
data(mtcars) 
head(mtcars) 

# Measures of Central Tendency 
print("JVNGANESH 21BDS0085") 
# Mean 
mean_values <- sapply(mtcars, mean) 
print("Mean Values:") 
print(mean_values) 

# Median 
median_values <- sapply(mtcars, median) 
print("Median Values:") 
print(median_values) 

# Quantiles (25%, 50%, 75%) 
quantiles <- apply(mtcars, 2, quantile) 
print("Quantiles (25%, 50%, 75%):") 
print(quantiles) 

# Deciles (ntile) 
deciles <- apply(mtcars, 2, function(x) quantile(x, probs = seq(0, 1, 0.1))) 
print("Deciles:") 
print(deciles) 

# Percentiles (90th percentile) 
percentiles <- apply(mtcars, 2, function(x) quantile(x, probs = 0.9)) 
print("90th Percentiles:") 
print(percentiles) 

# Measures of Dispersion 

# Range 
range_values <- sapply(mtcars, function(x) diff(range(x))) 
print("Range:") 
print(range_values) 

# Interquartile Range (IQR) 
iqr_values <- sapply(mtcars, IQR) 
print("Interquartile Range (IQR):") 
print(iqr_values) 

# Interdecile Range 
interdecile_range <- apply(mtcars, 2, function(x) quantile(x, probs = 0.9) - quantile(x, probs 
                                                                                      = 0.1)) 
print("Interdecile Range:") 
print(interdecile_range) 

# Mean Deviation 
mean_deviation <- apply(mtcars, 2, function(x) mean(abs(x - mean(x)))) 
print("Mean Deviation:") 
print(mean_deviation) 

# Standard Deviation 
sd_values <- sapply(mtcars, sd) 
print("Standard Deviation:") 
print(sd_values) 

# Skewness 
skewness_values <- sapply(mtcars, skewness) 
print("Skewness:") 
print(skewness_values) 

# Kurtosis 
kurtosis_values <- sapply(mtcars, kurtosis) 
print("Kurtosis:") 
print(kurtosis_values) 

# Frequency Distribution and Plots 

# Frequency Distribution (for mpg) 
mpg_freq <- table(cut(mtcars$mpg, breaks = 5)) 
print("Frequency Distribution for MPG:") 
print(mpg_freq) 

# Histogram for mpg 
ggplot(mtcars, aes(x=mpg)) +  
  geom_histogram(binwidth=2, color="black", fill="blue") +  
  labs(title="Histogram of MPG", x="Miles per Gallon", y="Frequency") 

# Relative Frequency Distribution 
mpg_rel_freq <- prop.table(mpg_freq) 
print("Relative Frequency Distribution for MPG:") 
print(mpg_rel_freq) 

# Cumulative Frequency Distribution 
mpg_cum_freq <- cumsum(mpg_freq) 
print("Cumulative Frequency Distribution for MPG:") 
print(mpg_cum_freq) 

# Categorical Variable Analysis (Pie Plot and Stacked Bar Plot) 

# Pie Plot for cyl 
cyl_freq <- table(mtcars$cyl) 
pie(cyl_freq, labels = names(cyl_freq), main = "Pie Chart of Cylinders") 

# Stacked Bar Plot for cyl and gear 
ggplot(mtcars, aes(x=factor(cyl), fill=factor(gear))) +  
  geom_bar(position="stack") +  
  labs(title="Stacked Bar Plot of Cylinders and Gears", x="Number of Cylinders", y="Count")