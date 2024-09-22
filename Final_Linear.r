
#Libraries---------------
install.packages("astsa")
library("astsa")
rm(list=ls())
#Analysis of graph+plot
x <- co2
frequency(x) #monthly data
plot(x) #highly seasonal, upward trend, variances look same
boxplot(x~cycle(x)) #because of the trend, we do not observe the seasonality well. 
time(x)
cycle(x)
str(x)
time(x)
str(x)

#•split data
x <- log(x) #first i thought that no need for log since the variances look pretty much the same,
#however, when i fit the trend and dum, i saw that with the log form of the data, adjusted r-squared is 0.99 while without log it is 0.987
#teherefore i decided to choose log form.
#pretty much no change in the estimation.
xtrain <- window(x, end = c(1995,12))
xtest <- window(x, start = c(1996,1))
xtest

# define dummy variables for the months
#for seasonality
dum <- as.factor(cycle(xtrain))
dum

#define time points (t2 quadratic)
#for trend
t <- time(xtrain)
t2 <- t^2
t3 <- t^3

#join variables into a dataframe
data <- data.frame("x"= xtrain, "t" = t, "t2"=t2, "t3"= t3, "dum" = dum)
head(data)

### fit a trend+dum model which handles with both trend and seasonality in the xtrain 
#fit linear trend
trend1 <- lm(x~t+dum, data = data, na.action = NULL) ##na.action = NULL retains time series features
summary(trend1) #r-sqaured is 0.987
plot(xtrain-fitted(trend1)) #left is the remainder part
plot(xtrain)
lines(fitted(trend1), col = 2, lty= 2,lwd = 2) #at the beginning and at the end we underestimate 
#while in the middle parts we over estimate. this could be because we should take log form instead. 
plot(resid(trend1)) 
#residuals do not look like a wite noise since we can observe a downward trend until the late 70s. 
#and then an upward trend
#we see the trend more clearly
acf(resid(trend1), 36) #it can be an ar model since it tails off
pacf(resid(trend1)) #by looking at pacf we can say that it could be ar(1) modelsince it cuts off after the 1 lag.

qqnorm(resid(trend1)) #not normally distributed 
qqline(resid(trend1), col = 2)

#fit quadratic trend
trend2 <- lm(x~t+t2+dum, data = data, na.action = NULL)
summary(trend2)
#adjusted r-squared is better, 0.9975. so it is a better model than the first one.
#the coefficients of t and t2 is both significant by looking at the t values, therfore w include both ofthem.
#by looking aat f value, we can say that all variables iin the model jointly significant tooo. And better than the linear trend. 
plot(xtrain)
lines(fitted(trend2), lty=2, lwd=2,col = 4)
#we observe a better estimation, the under estimation at the beginning and over estimation in the middle is almost gone. After
#1990 there are a little misestimation but they can be small enough to ignore. 
plot(resid(trend2)) 
acf(resid(trend2),36) #this can be an ar(1) process too but a better one. since it converges faster. 
#the coefficient should be smaller than the previous one.
pacf(resid(trend2),36) #again since it cuts off after lag one, this could be an ar(1) process.
qqnorm(resid(trend2))
qqline(resid(trend2), col = 2) 
#better distributed according to normal distribution but still not that good. 

#fit cubic trend
trend3 <- lm(x~t+t2+t3+dum, data = data, na.action = NULL)
summary(trend3)

par(mfrow=c(2,1))
#adjusted r-squared is better, 0.9988. maybe overfitting?
#the coefficients of t and t2 and t3 are both significant by looking at the t values, therfore w include both ofthem.
#by looking aat f value, we can say that all variables iin the model jointly significant tooo. And better than the linear trend. 
plot(xtrain)
lines(fitted(trend3), lty=2, lwd=2,col = 4)
#better than before 
plot(resid(trend3))
acf(resid(trend3),36) #acf ,s better than the two of above.
pacf(resid(trend3),36) #pacf is better too, after lag 1 all the pacf's are insignifacnt. in wuadrat,c trend there were one above the dashed line. 

qqnorm(resid(trend3))
qqline(resid(trend3), col = 2) #better normal distribution. the lower part is better but still some problems in the upper part
#overfitting olabilir, trend 2 ve trend3 dene.
plot(resid(trend3)) #residuals look more like a white noise than the other two. More jiggly residual graph we have,
#there are still some little trends but not that much dominant than the other two.
#instead of linear trend, quadratic trend is better in this dataset, so i will use quadratic trend

# define newdata for forecasting
t_n <- tail(time(x), 24) 
t_n2 <- t_n^2

t_n3 <- t_n^3
dum96 <- as.factor(rep(1:12,2)) #create dummy variables for two years, 24 observations
dum96
xnew <- data.frame("t"=t_n, "t2"=t_n2, "t3"= t_n3, "dum"=dum96)
head(xnew,24)


##### forecast the trend1 model
pred1 <- predict(trend1, newdata=xnew, interval = "p")
pred1 #24 prediction, lowerbound and upperbound
str(pred1)
fit1 <- ts(pred1, start = c(1996 ,1), frequency = 12) #making time series
fit1
plot(x)
lines(fitted(trend1), col = 4) #our estimation accoridng to the linear trend, trainset. 
lines(fit1[,1], col = 2) #forecast
lines(fit1[,2], col = 3) #lowerbound
lines(fit1[,3], col = 3) #upperbound

#We can see that not a good estimation. our fit values are underestimated, but still
#the true values are in the prediction intervals. 

#root mean sq. forecast error for trend1 model
sse1 <- sum((fit1[,1]-xtest)^2)
rmse1 <- sqrt(sse1/length(xtest))
rmse1 #0.005122436 low and good. 

##### forecast the trend2 model
pred2 <- predict(trend2, newdata=xnew, interval = "p")
pred2 #24 prediction, lowerbound and upperbound
str(pred2)
fit2 <- ts(pred2, start = c(1996 ,1), frequency = 12) #making time series
fit2
plot(x)
lines(fitted(trend2), col = 4) #our estimation accoridng to the quadratic trend, trainset. 
lines(fit2[,1], col = 2) #forecast
lines(fit2[,2], col = 3) #lowerbound
lines(fit2[,3], col = 3) #upperbound

#our fit values ar overestimated

#root mean sq. forecast error for trend2 model
sse2 <- sum((fit2[,1]-xtest)^2)
rmse2 <- sqrt(sse2/length(xtest))
rmse2 #0.004434085
 
##### forecast the trend3 model
pred3 <- predict(trend3, newdata=xnew, interval = "p")
pred3 #24 prediction, lowerbound and upperbound
str(pred3)
fit3 <- ts(pred3, start = c(1996 ,1), frequency = 12) #making time series
fit3
plot(x)
lines(fitted(trend3), col = 4) #our estimation accoridng to the quadratic trend, trainset. 
lines(fit3[,1], col = 2) #forecast
lines(fit3[,2], col = 3) #lowerbound
lines(fit3[,3], col = 3) #upperbound
#it looks very well


#root mean sq. forecast error for trend2 model
sse3 <- sum((fit3[,1]-xtest)^2)
rmse3 <- sqrt(sse3/length(xtest))
rmse3 #0.001510484 #HARİKA FORECAST.

#rmse is the lowest almost 0. Therefore it is the best model. 


### the seasonal and trend components

s <- coef(trend3)[-c(2,3,4)]
seas1 <- c(s[1], NA)
for (i in 2:12) seas1[i] <- s[i] + s[1]
seas <- seas1 - mean(seas1)
ts.plot(seas)

seas_ts <- ts(rep(seas,37), start = c(1959,1), frequency = 12)
plot(seas_ts)

###deseasonalized series
xtrain_ds <- xtrain-seas_ts
plot(xtrain)
lines(xtrain_ds, lwd=2,col=2)

###random component
remainder <- resid(trend3)

###estimate trend 
trend <- xtrain-seas-remainder

par(mfrow=c(3,1))
plot(xtrain)
lines(trend,col=2, lwd = 2)
plot(seas_ts)
plot(remainder)
par(mfrow=c(1,1))

