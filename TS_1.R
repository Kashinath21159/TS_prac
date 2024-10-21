################### Practical 01 ##############################################

#Q1) 
#years <- 1996:2019
#observations <- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 153.5,
#                 154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 157.8, 158.3, 
#                158.6, 158.6, 159.1, 159.3)

install.packages("forecast")
library(forecast)


# Time series data
years <- 1996:2019
observations <- c(150.3, 150.9, 151.4, 151.9, 152.5, 152.9, 153.2, 153.7, 153.6, 153.5,
                  154.4, 154.9, 155.7, 156.3, 156.6, 156.7, 157, 157.3, 157.8, 158.3, 
                  158.6, 158.6, 159.1, 159.3)

# Create time series object
ts_data <- ts(observations, start=1996, end=2019)
ts_data

# Simple Exponential Smoothing with alpha = 0.3
ses_model <- ses(ts_data, alpha=0.3, initial="simple")
ses_model

# Holt's Exponential Smoothing with alpha = 0.3 and beta = 0.2
holt_model <- holt(ts_data, alpha=0.3, beta=0.2, initial="simple")
holt_model

# Plotting the original series, SES, and Holt's smoothing
plot(ts_data, col="black", lwd=2, ylab="Observations", main="Comparison of Smoothing Methods")
lines(fitted(ses_model), col="blue", lwd=2)
lines(fitted(holt_model), col="red", lwd=2)
legend("topleft", legend=c("Original Series", "SES (α=0.3)", "Holt's (α=0.3, β=0.2)"),
       col=c("black", "blue", "red"), lty=1, lwd=2)



#Characteristics of SES:
#SES is effective for time series data without a trend or seasonality. In this case, 
#it smooths the data using a constant smoothing parameter 
#𝛼=0.3
#The SES line follows the original data closely, capturing the general level of 
#the data but without adapting to any potential trends or changes over time.
#Since SES does not account for trends, its fitted line appears relatively 
#flat and may lag behind the actual data if a trend exists.
#Characteristics of Holt's Exponential Smoothing:
#Holt's method extends SES by including a second smoothing parameter, 
#β, to account for trends in the data.
#With parameters 
#𝛼=0.3
#α=0.3 and β=0.2, the Holt model captures both the level and the trend, allowing it to adjust more dynamically to changes in the data.
#The red line representing Holt's fitted values follows the original series more closely, 
#especially in periods where the data shows an upward trend.
#Comparative Analysis:

#Trend Adaptation: Holt's model outperforms SES in tracking the overall trend of the 
#time series data. As the values are steadily increasing over the years, 
#Holt's model successfully captures this upward movement, while SES may lag behind due to 
#its constant smoothing approach.
#Flexibility: Holt's method is more flexible, making it suitable for data with trends. 
#SES, while simple and effective for stationary data, may not provide accurate forecasts 
#if the underlying data has a clear trend.


#####################
#Q2) 
#Year 	Average CO2 Concentration
#1991	355.62
#1992	356.36
#1993	357.1
#1994	358.86
#1995	360.9
#1996	362.58
#1997	363.84
#1998	366.58
#1999	368.3
#2000	369.47
#2001	371.03
#2002	373.61
#2003	357.61

# Given Data
# Given Data
years <- 1991:2003
co2_concentration <- c(355.62, 356.36, 357.1, 358.86, 360.9, 
                       362.58, 363.84, 366.58, 368.3, 
                       369.47, 371.03, 373.61, 357.61)

# Create a time series object
co2_ts <- ts(co2_concentration, start = 1991, frequency = 1)
co2_ts
# a) Make a time series plot of the given data
plot(co2_ts, type = "o", col = "blue", 
     main = "Atmospheric CO2 Concentration at Mauna Loa (1991-2003)", 
     xlab = "Year", ylab = "CO2 Concentration (ppm)")

# b) Forecast 2004 value by 3-year moving average smoothing method
# Get the last three years of data
last_three_years <- tail(co2_ts, 3)
last_three_years
# Calculate the forecast for 2004 as the average of the last three years
forecast_2004 <- mean(last_three_years)
forecast_2004
# Print the forecasted value
cat("Forecasted CO2 Concentration for 2004:", forecast_2004, "ppm\n")



# Q3) Apply the holt -winters method to AirPassengers data and forecast next 12 months data.

#QUESTION 3
# Load necessary libraries
install.packages("forecast")
library(forecast)

# Load the AirPassengers data
data("AirPassengers")

# Plot the original data
plot(AirPassengers, main = "AirPassengers Data", ylab = "Number of Passengers", xlab = "Year")

# Apply Holt-Winters Multiplicative Model
holt_winters_model <- HoltWinters(AirPassengers, gamma = TRUE, seasonal = "multiplicative")
holt_winters_model
# Forecast the next 12 months
forecasted_values <- forecast(holt_winters_model, h = 12)
forecasted_values

# Plot the forecasts
plot(forecasted_values, main = "Holt-Winters Forecast for AirPassengers", ylab = "Number of Passengers", xlab = "Year")

#seasonal = "multiplicative": 
#This specifies that the seasonal component is multiplicative, 
#meaning that the seasonal effect changes in proportion to the level of the series. 
#In other words, the effect of seasonality increases as the level of the series increases.