
#Libraries---------------
install.packages("astsa")
library("astsa")
#Analysis of grapf+plot
x <- co2
frequency(x) #monthly data
plot(x) #highly seasonal, upward trend, variances look same
boxplot(x~cycle(x)) #because of the trend, we do not observe the seasonality well. 
time(x)
cycle(x)
str(x)
time(x)
str(x)

#split data
x <- log(x) #since i use the log form in the linear regression part, i will use the log form again.
xtrain <- window(x, end = c(1995,12))
xtest <- window(x, start = c(1996,1))
xtest

# decompose by loess
dec <- stl(xtrain, s.window = 13) 
str(dec)
class(dec)
plot(dec) #i use the s.window = 13 since the seasonality is ppreety much the same. no need to s.window =7.
#we use s.window=7 if we know that seasonality increases or decreases over time to capture the differneces better.
#but the seasonality is almost the same here, and since it is the monthly data, it is better to use 13, since there are 12 monthy.
#here the stl captures the seasonality better. Sİnce the before one we assume that seasonality is the same for all years.
#However stl calculates seasonality for each year. th,s is better for our forecast.
#remainder look ok. it is jiggly. 

#seasonal component
seas <- dec$time.series[,1] #take the seasonal element, for all years, all months. 
#when we look at the seas carefully, we can see that the seasonal elements for same months but differnet years are different. 
seas

###re-estimating the trend since it is impossible to estimate from the trend, we need to fit a regression line to keep consistency
# define time points (t3 cubic since it was the better one in the regression analysis)
t <- time(xtrain)
t2 <- t^2
t3 <- t^3

# estimate cubic trend 
trend <- dec$time.series[,2]
trend3 <- lm(trend~t+t2+t3, na.action = NULL)


plot(x)
lines(fitted(trend3), col = 2)

# overall estimate: trend + seasonal and add to the plot
x_est <- fitted(trend3) + seas #we take the estimated trend and the seasonal component
lines(x_est, col = 2, lty = 2)

#the estimation looks quite good. İn some years we see overestimation and underestimation.
#there is no constant pattern that we observe under and overestimaton. But as a whole, model fits well. 

# define newdata for forecasting the trend
t_n <- tail(time(x), 24)  #last 24 observations
t_n2 <- t_n^2
t_n3 <- t_n^3
xnew <- data.frame("t"=t_n, "t2"=t_n2, "t3"=t_n3)

# forecast the trend
trend_96 <- predict(trend3, newdata=xnew, interval = "p")
trend_96

trend96 <- ts(trend_96, start = c(1996 ,1), frequency = 12)
trend96

lines(trend96[,1], col = "green") #the forecasted trend in the future
#forecasted trend looks good.


### final forecast (trend + seasonal)
#according to last two year's seasonality
seas96 <- window(seas, start = c(1994,1)) # using the 1994 and 1995's seasonal values
seas96
seas_96 <- ts(seas96, start = c(1996,1), frequency = 12)

xfitted <- trend96[,1] + seas_96
up <- trend96[,3] + seas_96 # incomplete since we do not have error terms
low <- trend96[,2] + seas_96  


lines(xfitted, col = "green", lwd = 1.5)
lines(up, col = 6)
lines(low, col = 6)
#the forecast is almost the same with the actual data. 


#root mean sq. error
sse <- sum((xfitted-xtest)^2)
rmse <- sqrt(sse/length(xtest))
rmse #0.001445857

par(mfrow=c(2,1))

resid <- dec$time.series[,3]
plot(resid) #looks jiggly
acf(resid)
pacf(resid)
qqnorm(resid)
qqline(resid, col = 2) 
#resids could follow an seasonal arima model. 





