#####ARIMAda q=1 ve Q=1 yapınca residuallar genelde white noise oluyor. 
#forecast performanslarına göre compore et. 
rm(list=ls())



library(astsa)
tsplot(co2)
acf2(co2)

#monthly data, seasonality
#by looking at the acf we can say taht there is a trend.the higher autocorrelations could be
#because of the trend. Besides the trend, there could be an ar model since the autocorrelations are high.
#by looking at the pacf, at lag1 there is a high autocorrelation so it can be an ar(1) model.
#in the second lag again pacf cuts the dashed line so it is signifactn, thus it can be an ar(2)
#then after that, it kind of tail off, so there can be an ma process. 
#since there is seasonality and trend, there should be both de-trending element which is the differencing say 1, and
#seasoal differencing, say 1 again. 

x <- co2

#split data
x <- log(x) #since i use the log form in the linear regression part, i will use the log form again.
xtrain <- window(x, end = c(1995,12))
xtest <- window(x, start = c(1996,1))


mod1 <- sarima(xtrain, p = 0, d = 1, q = 0, P = 0, D = 1, Q = 0, S = 12)
#first i will only reduce the trend and seasonality to see what happens. 
mod1
#$AICc
#[1] -10.40959
acf2(resid(mod1$fit))
#residuals are bad. the ljung-box statistics are low, insignificant most of the time.
#acf od the residuals are at some lags significant. Autocorrelation occurs between the residuals.
#moreover, in the standardized residuals graph, there is a strong decrease in 1960. 
#by looking at the resulst, we can say that, this is not a really good model. 

mod2 <- sarima(xtrain, p = 1, d = 1, q = 0, P = 0, D = 1, Q = 0, S = 12)
#here, i will add ar(1) process while keeping same the differencing since i know there is a trend and seasonality. 
#Again residuals are bad. LJUNG-BOX statistics ARE insignificant and we have an autocorrelation issue. 
#$AICc
#[1] -10.70216  
acf2(resid(mod2$fit))
#autocorrelation problem.


mod3 <- sarima(xtrain, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 0, S = 12)
#i will add an ma(1) process since i know the pacf tails off. 
#again very bad model. 
acf2(resid(mod3$fit))

#since i realize the pacf of the residuals tail off,i add an ma process.
mod4 <- sarima(xtrain, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
#now the ljung box statistic is better. and acf looks better. in the first lag there is an autocorrelation problem
acf2(resid(mod4$fit))
#AICc=-11.00773
# the least autocorrelation one both in pacf and acf, moreover it has the lowest AICc. 

mod5 <- sarima(xtrain, p = 2, d = 1, q = 2, P = 2, D = 1, Q = 0, S = 12)
acf2(resid(mod5$fit))


mod6 <- sarima(xtrain, p = 2, d = 1, q =1, P = 0, D = 1, Q = 1, S = 12)
#$AICc-10.82013
#autocorrelations and distribution is better too, comparable with the mod4. 
#since the mod4 has a lower AICc, it is my best model. 


#now i will try to add an external regressor.
### with deterministic trend
t <- 1:length(xtrain) 
t2 <- t^2 
t3 <- t^3

##standardize t
st <- (t-mean(t))/sd(t)
st2 <- st^2
st3 <- st^3
ex <- cbind("t" = st, "t2" = st2, "t3" = st3)
str(ex)
class(ex)



mod_t <- sarima(xtrain,  xreg = ex, p = 0, d = 0, q = 0, P = 0, D = 1, Q = 0, S = 12)
acf2(resid(mod_t$fit))

mod1_t <- sarima(xtrain,  xreg = ex, p = 1, d = 0, q = 1, P = 0, D = 1, Q = 0, S = 12)
acf2(resid(mod1_t$fit))

mod2_t <- sarima(xtrain,  xreg = ex, p = 1, d = 0, q = 1, P = 0, D = 1, Q = 1, S = 12)
acf2(resid(mod2_t$fit))

mod3_t <- sarima(xtrain,  xreg = ex, p = 2, d = 0, q = 1, P = 1, D = 1, Q = 1, S = 12)
acf2(resid(mod3_t$fit))

#best ones are the mod2 and mod3 according to the acf functions but anyway they are bad
#since the aicc for mod2_t is lower, i would choose that one. 
#mod2_t -11.03772
#However, t3 is insignificant,there for i will use the quadratic external regressor. 



#quadratic trend
ex1 <- cbind("t" = st, "t2" = st2)

mod_t1 <- sarima(xtrain,  xreg = ex1, p = 0, d = 0, q = 0, P = 0, D = 1, Q = 0, S = 12)
acf2(resid(mod_t$fit))

mod1_t1 <- sarima(xtrain,  xreg = ex1, p = 1, d = 0, q = 1, P = 0, D = 1, Q = 0, S = 12)
acf2(resid(mod1_t$fit))

mod2_t1 <- sarima(xtrain,  xreg = ex1, p = 1, d = 0, q = 1, P = 0, D = 1, Q = 1, S = 12)
acf2(resid(mod2_t$fit))

mod3_t1 <- sarima(xtrain,  xreg = ex1, p = 2, d = 0, q = 1, P = 1, D = 1, Q = 1, S = 12)
acf2(resid(mod3_t$fit))
#but with quadratic trend, mod2_t1 and mod3_t1 are similar, since the AICc is lower in mod2_t1
#I choose mod2_t1:with AICc
#[1] -11.01982
#$AICc mod3_t1
#[1] -11.01501

######FORECASTING

sarima.for(xdata, n.ahead, p, d, q, P = 0, D = 0, Q = 0, S = -1, 
           tol = sqrt(.Machine$double.eps), no.constant = FALSE,
           plot.all=FALSE, xreg = NULL, newxreg = NULL, fixed=NULL)

mod4 <- sarima(xtrain, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12) #chosen model
for_mod4 <- sarima.for(xtrain, n.ahead = 24, p = 1, d = 1, q = 1, P = 0, D = 1, Q = 1, S = 12)
str(for_mod4)
x96_mod <- for_mod4$pred
par(mfrow=c(1,1))

#root mean sq. error
sse <- sum((x96_mod-xtest)^2)
rmse1 <- sqrt(sse/length(xtest))
rmse1
#0.00101131

t97 <- st[length(st)] + (st[2] - st[1])*(1:24)
t972 <- t97^2
newex <- cbind("t" = t97, "t2" = t972) #since i choose the quadratic trend, no need for the cubic trend

mod3_t1 <- sarima(xtrain,  xreg = ex1, p = 2, d = 0, q = 1, P = 1, D = 1, Q = 1, S = 12) #chosen
for_mod3_t1 <- sarima.for(xtrain, n.ahead = 24,  xreg = ex1, newxreg = newex, p = 2, d = 0, q = 1, P = 1, D = 1, Q = 1, S = 12)

x96_mod3_t <- for_mod3_t1$pred

#root mean sq. error
sse <- sum((x96_mod3_t-xtest)^2)
rmse_t <- sqrt(sse/length(xtest))
rmse_t
#0.002229901




