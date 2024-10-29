# 1. Load the CSV file 
airpassengers_data <- read.csv("C:/Users/lenovo/Desktop/airpassengers.csv") 

# 2. Convert the "Month" column to Date type 
airpassengers_data$Month <- as.Date(airpassengers_data$Month, format="%Y-%m") 

# 3. Convert the data to a time series object 
# Extracting the year and month from the "Month" column for proper indexing 
airpassengers_ts <- ts(airpassengers_data$X.Passengers,  
                       start = c(1949, 1),  # Start date from dataset 
                       frequency = 12)  # Monthly data, hence frequency = 12 

# 4. Check for the Structure and the Data type of the time series 
str(airpassengers_ts)  # To check the structure 
class(airpassengers_ts)  # To check the data type (should be "ts") 

# 5. Check for missing values in the dataset 
sum(is.na(airpassengers_ts))  # To check for missing values 

# 6. Check for the Starting date and Ending date 
start(airpassengers_ts)  # To check the starting date 
end(airpassengers_ts)  # To check the ending date 

# 7. Check for the frequency of the dataset 
frequency(airpassengers_ts)  # To check the frequency (monthly data = 12) 

# 8. Check for the summary of the dataset 
summary(airpassengers_ts)  # Summary statistics of the dataset 

# 9. Plot the decomposition of the dataset 
decomposed_data <- decompose(airpassengers_ts)  # Decompose the data into trend, 
seasonal, random 
plot(decomposed_data)  # Plot the decomposed data (trend, seasonal, residuals) 

# 10. Plot the dataset 
plot(airpassengers_ts,  
     main="AirPassengers Data", 
     xlab="Year",  
     ylab="Passengers (1000s)")  # Basic plot of the dataset 

# 11. Plot the time series of the dataset 
plot.ts(airpassengers_ts,  
        main="AirPassengers Time Series", 
        xlab="Year",  
        ylab="Passengers (1000s)")  # Plot as a time series 

# 12. Draw the regressor line for the time series plot 
abline(lm(airpassengers_ts ~ time(airpassengers_ts)),  
       col="red",  
       lwd=2)  # Add a regression line to visualize the trend 

# 13. Print the cycle across the years for the dataset 
print(cycle(airpassengers_ts))  # Display the cycle (months) of the dataset 

# 14. Make the dataset stationary (constant mean and variance) and plot it 
# a. Apply logarithmic transformation to stabilize variance 
log_data <- log(airpassengers_ts) 
plot(log_data,  
     main="Log Transformed AirPassengers Data",  
     ylab="Log(Passengers)") 

# b. Differencing to stabilize the mean 
diff_log_data <- diff(log_data) 
plot(diff_log_data,  
     main="Differenced Log AirPassengers Data",  
     ylab="Differenced Log(Passengers)") 

# 15. Plot boxplot across months for seasonal effect 
boxplot(airpassengers_ts ~ cycle(airpassengers_ts),  
        xlab = "Month",  
        ylab = "Passengers",  
        main = "Monthly Air Passengers Boxplot") 




