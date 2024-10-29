data("mtcars") 
data("iris") 
data("airquality") 
print("21BDS0085 JVNGANESH") 
# Checking if the dataset is imported or not 
head(mtcars) 
head(iris) 
head(airquality) 

# Basic Descriptive Statistics on mtcars dataset 
mean(mtcars$mpg)                           # Mean of miles per gallon (mpg) 
median(mtcars$hp)                          # Median of horsepower (hp) 
sd(mtcars$wt)                              # Standard deviation of weight (wt) 
var(mtcars$disp)                           # Variance of displacement (disp) 
range(mtcars$qsec)                         # Range of quarter-mile time (qsec) 
min(mtcars$mpg)                            # Minimum miles per gallon 
max(mtcars$mpg)                            # Maximum miles per gallon 
quantile(mtcars$hp)                        # Quantiles of horsepower 
IQR(mtcars$drat)                           # Interquartile range of rear axle ratio (drat) 
sum(mtcars$cyl)                            # Sum of cylinder counts 
prod(mtcars$gear)                          # Product of gear counts 
cumsum(mtcars$mpg)                         # Cumulative sum of mpg 
cumprod(mtcars$gear)                       # Cumulative product of gear counts 
cummax(mtcars$hp)                          # Cumulative maximum of horsepower 
cummin(mtcars$hp)                          # Cumulative minimum of horsepower 
cor(mtcars$mpg, mtcars$hp)                 # Correlation between mpg and hp 
cov(mtcars$mpg, mtcars$wt)                 # Covariance between mpg and wt 

# Table and summary functions on iris dataset only 
table(iris$Species)                        # Contingency table of species 
prop.table(table(iris$Species))            # Proportions table of species 
fivenum(iris$Sepal.Length)                 # Five-number summary of Sepal Length 
summary(iris$Sepal.Width)                  # Summary of Sepal Width 

# Hypothesis Testing on mtcars dataset 
t.test(mtcars$mpg ~ mtcars$am)             # t-test between mpg for automatic vs 
#manual 
wilcox.test(mtcars$mpg ~ mtcars$am)        # Wilcoxon test between mpg for 
#automatic vs manual 
chisq.test(table(mtcars$am, mtcars$gear))  # Chi-squared test for am and 
#gear 
fisher.test(table(mtcars$cyl, mtcars$am))  # Fisher's exact test for cyl and am 
shapiro.test(mtcars$mpg)                   # Shapiro-Wilk test for normality of mpg 
ks.test(mtcars$mpg, "pnorm", mean = mean(mtcars$mpg), sd = 
          sd(mtcars$mpg))  # Kolmogorov-Smirnov test for mpg 
bartlett.test(mtcars$mpg ~ mtcars$gear)    # Bartlett's test for homogeneity of 
#variance 
fligner.test(mtcars$mpg ~ mtcars$gear)     # Fligner-Killeen test for 
#homogeneity of variance 
mcnemar.test(table(mtcars$vs, mtcars$am))  # McNemar's test for paired 
#nominal data 
kruskal.test(mpg ~ gear, data = mtcars)    # Kruskal-Wallis test for mpg across 
#gears 
friedman.test(as.matrix(mtcars[,c("mpg", "hp", "qsec")]))  # Friedman test for 
#repeated measures 
# Regression and Model Fitting on mtcars dataset 
model <- lm(mpg ~ hp + wt, data = mtcars) 
summary(model) 
aov_model <- aov(mpg ~ gear, data = mtcars) 
summary(aov_model) 
glm_model <- glm(vs ~ mpg + hp, data = mtcars, family = binomial) 
summary(glm_model) 
predict(model, newdata = mtcars[1:5, ]) 
residuals(model) 
confint(model) 
step(model) 

# Multivariate Statistics on iris dataset 
pca <- prcomp(iris[,1:4], scale. = TRUE) 
summary(pca) 
kmeans_result <- kmeans(iris[,1:4], centers = 3) 
kmeans_result 
hc <- hclust(dist(iris[,1:4])) 
plot(hc) 
distance_matrix <- dist(iris[,1:4]) 
distance_matrix 

# Time Series Analysis on airquality dataset 
airquality_ts <- ts(airquality$Temp, start = c(1973, 5), frequency = 12) 
acf(airquality_ts)                          # Autocorrelation function 
pacf(airquality_ts)                         # Partial autocorrelation function 
fit <- arima(airquality_ts, order = c(1, 0, 0)) 
fit 
forecast_values <- predict(fit, n.ahead = 5) 
forecast_values 
decomposed_ts <- decompose(airquality_ts) 
plot(decomposed_ts) 
#Code (part 2): 
  # Install all required packages 
install.packages("glmnet")      # For Ridge Regression 
install.packages("forecast")    # For Holt-Winters Forecasting 
install.packages("CCA")         # For Canonical Correlation Analysis 
install.packages("rstanarm")    # For Bayesian Linear Regression 
install.packages("dbscan")      # For DBSCAN Clustering 
install.packages("MASS")        # For Robust Regression and Canonical 
#Discriminant Analysis 
install.packages("lavaan")      # For Structural Equation Modeling (SEM) 
install.packages("plm")         # For Panel Data Analysis 
install.packages("survival")    # For Kaplan-Meier Estimator 
install.packages("lme4")        # For Linear Mixed Models (LMM) 
install.packages("coda")        # For MCMC Simulation 


# Load necessary libraries 
library(glmnet)      # Ridge Regression 
library(forecast)    # Holt-Winters Forecasting 
library(CCA)         # Canonical Correlation Analysis 
library(rstanarm)    # Bayesian Linear Regression 
library(dbscan)      # DBSCAN Clustering 
library(MASS)        # Robust Regression, Canonical Discriminant Analysis 
library(lavaan)      # Structural Equation Modeling (SEM) 
library(plm)         # Panel Data Analysis 
library(survival)    # Kaplan-Meier Estimator 
library(lme4)        # Linear Mixed Models (LMM) 
library(coda)        # MCMC 

print("21BDS0085 JVNGANESH") 
# 1. Ridge Regression (Advanced Regression) 
X <- as.matrix(mtcars[, c("hp", "wt", "qsec")]) 
y <- mtcars$mpg 
ridge_model <- glmnet(X, y, alpha = 0) 
print(ridge_model) 

# 2. Holt-Winters Forecasting (Time Series Analysis) 
airquality_ts <- ts(airquality$Temp, start = c(1973, 5), frequency = 12) 
hw_model <- HoltWinters(airquality_ts) 
plot(hw_model) 

# 3. Canonical Correlation Analysis (Multivariate Analysis) 
X <- mtcars[, c("mpg", "hp", "wt")] 
Y <- mtcars[, c("qsec", "drat", "gear")] 
cca_result <- cancor(X, Y) 
print(cca_result) 

# 4. Bayesian Linear Regression (Bayesian Statistics) 
bayesian_model <- stan_glm(mpg ~ hp + wt, data = mtcars) 
print(summary(bayesian_model)) 

# 5. DBSCAN Clustering (Clustering and Classification) 
X <- iris[, 1:4] 
dbscan_result <- dbscan(X, eps = 0.5, minPts = 5) 
print(dbscan_result) 

# 6. Mann-Whitney U Test (Non-Parametric Test) 
wilcox_test <- wilcox.test(mpg ~ am, data = mtcars) 
print(wilcox_test) 

# 7. Robust Regression (Robust Statistics) 
robust_model <- rlm(mpg ~ hp + wt, data = mtcars) 
summary(robust_model) 

# 8. Path Analysis (Structural Equation Modeling - SEM) 
model <- ' 
  mpg ~ hp + wt 
  hp ~ wt 
' 
sem_fit <- sem(model, data = mtcars) 
summary(sem_fit) 

# 9. Panel Data Analysis (Econometrics) 
mtcars$car <- rownames(mtcars) 
pdata <- pdata.frame(mtcars, index = c("car", "gear")) 
panel_model <- plm(mpg ~ hp + wt, data = pdata, model = "random") 
summary(panel_model) 

# Load the necessary package 
library(plm) 

# Ensure the mtcars dataset has identifiers 
mtcars$car <- rownames(mtcars)  # Add a car identifier 
mtcars$time <- seq_len(nrow(mtcars))  # Create a simple time identifier 

# Convert the dataset into a panel data frame 
pdata <- pdata.frame(mtcars, index = c("car", "time")) 

# Check the structure of the panel data 
print(head(pdata)) 

# Fit a panel data model (Random effects model) 
panel_model <- plm(mpg ~ hp + wt, data = pdata, model = "random") 

# Summarize the model 
summary(panel_model) 


# 10. Kaplan-Meier Estimator (Survival Analysis) 
# Install and load the necessary package 
install.packages("survival") 
library(survival) 

# Load the lung dataset 
data("lung") 

# Check the structure of the lung dataset 
print(head(lung)) 

# Fit a Kaplan-Meier survival curve 
km_fit <- survfit(Surv(time, status) ~ sex, data = lung) 

# Plot the Kaplan-Meier survival curve 
plot(km_fit, col = c("blue", "red"), lty = 1:2, xlab = "Time", ylab = "Survival 
Probability") 
legend("topright", legend = c("Male", "Female"), col = c("blue", "red"), lty = 1:2)