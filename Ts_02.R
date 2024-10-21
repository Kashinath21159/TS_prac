
################## PRACTICAL 2 ######################
#AR(1) Process
#The AR(1) process is defined by the equation:
#Xt=0.5Xt−1+ϵt mean 0 and variance 1

# 1 (a) Simulate 100 observations from AR(1) process
set.seed(42)
n <- 100
phi <- 0.5
ar1_process <- arima.sim(model = list(ar = phi), n = n)
ar1_process


# 1 (b) Plot the time series
plot.ts(ar1_process, main = "Simulated AR(1) Process", ylab = "X_t", col = "blue")


# 1 (c) Estimate AR(1) parameter using arima function
ar1_fit <- arima(ar1_process, order = c(1, 0, 0))
phi_estimated <- ar1_fit$coef[1]
phi_estimated  # Print estimated phi

#AR(1) Process
#The AR(1) process is defined by the equation:
#Xt=0.5Xt−1+ϵt mean 0 and variance 1
#i)	  plot acf  pacf of the series 
# 2 (i) Plot ACF and PACF of the series
par(mfrow = c(1, 2))  # Set up a 1x2 plotting grid
acf(ar1_process, main = "ACF of AR(1) Process")
pacf(ar1_process, main = "PACF of AR(1) Process")
par(mfrow = c(1, 1))  # Reset plotting grid

#ii) Discuss the behaviour of acf and pacf For AR(1) process 
#For an AR(1) process, the behavior of the ACF 
#(Autocorrelation Function) and PACF (Partial Autocorrelation Function) 
#follows distinct patterns, which can help identify the process when analyzing time series data.

#ACF (Autocorrelation Function) of AR(1):
#Decay Pattern: In an AR(1) process, the ACF will show 
#an exponentially decaying behavior. This occurs because each observation 
#is correlated with the one before it, but this correlation weakens over time.
#Interpretation: The ACF captures how observations at different time lags are related to each other. 
#For AR(1), the lag-1 autocorrelation will be the highest, and it will decrease as the lag increases. 
#The decay is characteristic of autoregressive models, especially AR(1), 
#and the rate of decay depends on the value of 𝜙
#The closer 𝜙 is to 1, the slower the decay.

#PACF (Partial Autocorrelation Function) of AR(1):
#Cutoff at Lag 1: The PACF of an AR(1) process will have a significant spike at lag 1, 
#and will then drop to zero for all higher lags. 
#This is because the PACF shows the correlation between an observation and 
#its lagged values after removing the effects of any intermediate lags. 
#In an AR(1) process, only the first lag is directly correlated with the current observation, 
#while higher-order lags have no partial autocorrelation.

# 3)
# 3 (i) Fit AR(1) or AR(2) model to the data
ar_fit <- arima(ar1_process, order = c(1, 0, 0))  # AR(1)
ar_fit
ar_fit_2 <- arima(ar1_process, order = c(2, 0, 0))  # AR(2)
ar_fit_2

# 3 (ii) Forecast the next 10 observations and plot the forecast
forecasted_values <- forecast(ar_fit, h = 10)
forecasted_values
plot(forecasted_values, main = "Forecast of AR(1) Process", col = "red")