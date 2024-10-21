
#Auto regressive Moving Average(ARMA) 
#Q.1
#Time	Values 
#1	100
#2	110
#3	120
#4	130
#5	140
#6	150
#7	160
#8	170
#9	180
#10	190
#11	200
#12	210
#13	220
#14	230
#15	240


# Load necessary libraries
library(forecast)
library(tseries)

# Time series data
time_series <- ts(c(100, 110, 120, 130, 140, 150, 160, 170, 180, 190, 200, 210, 220, 230, 240))

# 1. Log transform the data to stabilize variance (optional but can help in some cases)
log_time_series <- log(time_series)

# 2. Use auto.arima() to automatically fit the best ARIMA model
# This function will choose the appropriate order of AR and MA terms, as well as differencing
auto_model <- auto.arima(log_time_series)

# Print the model summary to check what ARIMA model was fitted
summary(auto_model)

# 3. Diagnostic checks of residuals
checkresiduals(auto_model)  # Check if residuals look like white noise

# 4. Forecast the next 10 values
forecasted_values <- forecast(auto_model, h = 10)

# Print forecasted values (on the log scale)
print(forecasted_values)

# 5. Plot the original time series and the forecasted values
plot(forecasted_values, main = "Original Time Series and Forecasted Values (Log Transformed)")


# Q2 ) 
#1	500
#2	520
#3	540
#4	560
#5	580
#6	600
#7	620
#8	640
#9	660
#10	680
#11	700
#12	720

# Load necessary libraries
library(forecast)
library(tseries)

# Time series data (Sales)
sales_data <- c(500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720)
time_series <- ts(sales_data, frequency = 1)  # Frequency can be set to 1 for quarterly data

# 1. Plot the ACF and PACF of the original time series
par(mfrow = c(1, 2))  # Set up a 1x2 grid
acf(time_series, main = "ACF of Original Series")   # Plot ACF
pacf(time_series, main = "PACF of Original Series") # Plot PACF
par(mfrow = c(1, 1))  # Reset plotting grid

# 2. Fit an ARMA(1,1) model to the data
arma_model <- arima(time_series, order = c(1, 0, 1))  # Fitting ARMA(1,1)
summary(arma_model)  # Print the summary of the model

# 3. Check the residual diagnostics of the fitted model
tsdiag(arma_model)  # Diagnostic plots for residuals (ACF of residuals, etc.)
Box.test(arma_model$residuals, lag = 10, type = "Ljung-Box")  # Ljung-Box test

# 4. Forecast the next 12 values using the fitted model
forecasted_values <- forecast(arma_model, h = 12)

# Print the forecasted values
print(forecasted_values)

# 5. Plot the original time series, fitted values, and forecasted values
plot(forecasted_values, main = "Original Time Series and Forecasted Values", ylab = "Sales", xlab = "Time")
lines(time_series, col = "blue", lwd = 2)  # Add original series in blue

# Add a legend
legend("topleft", legend = c("Original Series", "Forecasted Values"), col = c("blue", "red"), lty = 1)

