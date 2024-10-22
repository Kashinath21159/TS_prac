












































#Q1

sales_data <- c(200, 220, 240, 210, 230, 250, 300, 270, 260, 280, 310, 330, 
                220, 230, 250, 240, 260, 280, 320, 300, 290, 310, 330, 350, 
                240, 250, 270, 260, 280, 300, 340, 320, 310, 330, 350, 370, 
                260, 270, 290, 280, 300, 320, 360, 340, 330, 350, 370, 390, 
                280, 290, 310, 300, 320, 340, 380, 360, 350, 370, 390, 410)

#1) Convert to time series object
sales_ts <- ts(sales_data, start = c(2019, 1), frequency = 12)
sales_ts
#Q2
adf_test<-adf.test(sales_data)
adf_test


acf_s<-acf(sales_data)
pacf_s<-pacf(sales_data)

s_diff<-diff(sales_data,lag=12)
acf_d<-acf(s_diff)
pacf_d<-pacf(s_diff)

#Q3
sarima_model<-arima(s_diff,order=c(1,0,1),seasonal = c(1,1,1))
# Fit SARIMA model with identified parameters
sarima_model <- Arima(s_diff, order = c(1, 0, 1), seasonal = list(order=c(1, 1, 1)))


# Check model summary
summary(sarima_model)

# Check residuals
checkresiduals(sarima_model)

#Q5
arima_d<-arima(sales_ts,order=c(1,0,1))

arima_d
aic_ar<-AIC(arima_d)
bic_a<-BIC(arima_d)

sr_a<-AIC(sarima_model)
sr_b<-BIC((sarima_model))

aic_ar
sr_a
#Q6
holiday_effect <- c(rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1, rep(0, 11), 1)

sarima_model <- Arima(s_diff, order = c(1, 0, 1), xreg= holiday_effect,seasonal = list(order=c(0, 1, 1)),p)

sarima_model <- Arima(s_diff, order = c(1, 0, 1), 
                      seasonal = list(order = c(1, 1, 1)),
                      xreg = holiday_effect)

# Check the model summary
summary(sarima_model)


help("arima")
