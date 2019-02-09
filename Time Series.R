library(PerformanceAnalytics)
data("managers")
ma = managers

typeof("managers")

#convert time series object to data frame
library(ggplot2)
df <- fortify(ma)

sapply(df, function(x) sum(is.na(x)))

#Convert Dates into time stamp
dates = as.POSIXct(df$Index, format="%Y-%m-%d")
#Pull out data of return of US 10 year treasury bills and convert the whole dataframe to extensible time-series object(xts).
us_10 = xts(df[,10], order.by=dates)
#Convert Data to a simple  time series object.
us10 <- ts(us_10,start = c(1996,1),frequency = 12)
us10

class(us10)
time(us10)

#view data of specific time frame
us10.january = window(us10,start = c(1996,1),freq = TRUE)
us10.january

plot(us10)
plot(us10.january)
abline(reg = lm(us10~time(us10))) #ploting a regression line

us10_ad = decompose(us10, type = "additive")
plot(us10_ad)
us10_mu = decompose(us10, type = "multiplicative")
plot(us10_mu)


plot(us10,SP500)
ts.plot(us10)

#correleation
acf(us10)
pacf(us10)


acf(SP500)
ts.intersect(us10,SP500) #Calculates time intersection of two  series
acf(ts.intersect(us10,SP500))
ts.union(us10,SP500)
acf(ts.union(us10,SP500))


#HoltWinter
plot(HoltWinters(us10,alpha = 0.001, beta = 1, gamma = 0))
plot(HoltWinters(SP500,alpha = 0.01, beta = .1, gamma = 0))

hw.us10 = HoltWinters(us10)
plot(hw.us10)
hw.SP500 = HoltWinters(SP500)
plot(hw.SP500)

#Simple Prediction using holt-winter data for next two years
us10.predict = predict(hw.us10,n.ahead = 2*12)
ts.plot(us10,us10.predict, lty = 1:2)#lty changes line type

SP500.predict = predict(hw.SP500,n.ahead = 2*12)
ts.plot(SP500,SP500.predict, lty = 1:2)

hw1.us10 = HoltWinters(us10, seasonal = "additive")
us10.predict1 = predict(hw1.us10,n.ahead = 2*12)
ts.plot(us10,us10.predict1, lty = 1:2)#lty changes line type



#random walk
acf(diff(us10)) #1st lagg
acf(diff(SP500))
pacf(us10)


#Auto regression model
us10.ar = ar(us10)
acf(us10.ar$resid[-1],na.action = na.pass)
us10.ar
summary(us10.ar)

#arima model
#MA
us10.ma = arima(us10, order = c(0,0,3)) #MA(3) process

#ARMA
us10.arma = arima(us10, order = c(1,0,1)) #ARMA(1,1) process

#ARIMA
us10.arima = arima(us10, order = c(2,1,2)) #ARIMA (1,2,1) process

aic_comp = c(us10.ma$aic,us10.arma$aic,us10.arima$aic) #compairing aic of various models
names(aic_comp) <- c ("AIC MA(1)","AIC ARMA(1,1)","AIC ARIMA(2,1,2)")

aic_comp
#one can run many models based on values of acf and pacf, AIC value can be used to compare among models

#Unit root test
library("urca")
us10.df <- ur.df(us10,type = "drift")
us10.df


#in the following section we will try to find out relationship between us10 and SP500

attach(Data)
reg <- lm(SP500.TR~us10)
summary(reg)

#here we will deal with the problem of serial correleation i.e. correleation between error terms
library(lmtest)
library(tseries)
library(orcutt)
library(egcm)

#durbin watson test for serial corrilation
#null hipothesis is tthere is no auto or serial correleation among residuals
dwtest(reg)
#this test indicates no auto correleation(or serial correleation)
#for this dataset we are free of auto correleation but if we found autocorreleation
#one way to remove that is to use cochrane.orcutt fumction form the package.
#the form will be like this
#reg1 = cochrane.orcutt(reg)
#dwtest(reg1)


reg <- lm(SP500.TR~us10+HAM3+HAM4)

#we will test stationarity of individual indexes
adf.test(us10)


#alternate hypothesis is stationarity
#p value is very less implies stationarity

Box.test(us10) #Box_Pierce test
#in box pierce test 
#null is that the data is stationary.
#p value is not so small so we accept the null

pp.test(us10)
#alternative hypothesis: stationary
#p value is very small we accept alternate hypothesis
#now if in case the series is non stationary, one of the way is to go with lagged difference
#the syntax will look like this
diff_us10 = diff(us10, differences = 1)
#diff_us10 is the first difference of us10
adf.test(diff_us10)
Box.test(diff_us10)

#Cointeegration test
egcm(us10,SP500.TR)#engel and grienger test of cointegration

#result indicates that nither way us10 and SP500.TR is intigrated

library(zoo)
library('forecast')
library(FinTS)
library(rugarch)
library(tseries)

fit1 <- auto.arima(us10,trace = TRUE, test = "kpss", ic = "bic")
data_for_model = cbind(us10,SP500)
data_for_model

#boxtest furthe tells us that if the data suffers ARCH effect 
#i.e. auto regressive conditional heteroscadasticity
Box.test(fit1$residuals^2,type = "Ljung-Box")
#p value is high indicates that our data is free from ARCH effect.Here null hypothesis 
#is there is ARCH effect.

#if there is ARCH effect, then we have to form GRACH model, auto regressive conditional heteroscadasticity
#first we will create specification
fit2_spec_garch <- ugarchspec(variance.model = list(garchOrder = c(1,1)),mean.model = list(armaOrder = c(0,1)))#1,1 garch model#
fit2_garch = ugarchfit(spec = fit2_spec_garch,data = us10)
fit2_garch

#next we will try to forecast based on the above model
fit2_garch_forc <- ugarchforecast(fit2_garch,n.ahead = 10)
fit2_garch_forc

#Lets do some forecasting
train.data = window(us10,start=c(1996,1),end=c(2004,12))
plot(train.data)
test.data = window(us10, start = c(2005,1))

#We are going to build Autoregressive and Moving Average model
library(forecast)
mod1 <- auto.arima(train.data, trace = TRUE, test = "kpss", ic = "bic")#minimum bic criteria
summary(mod1)
confint(mod1)

plot.ts(mod1$residuals)
Box.test(mod1$residuals,lag = 12, type = "Ljung-Box")
# there is no autocorreleation
Box.test(mod1$residuals^2,lag = 12, type = "Ljung-Box")
# there is no ARCH effect
# Normality of Residuals
# qq plot for studentized resid
qqnorm(mod1$residuals, main="QQ Plot")
qqline(mod1$residuals)
#The points are distributed along the line, therefore we can easily estimate that
#residuals are normally distributed

mod1.forecast = forecast(mod1,h=24)
mod1.forecast
plot(mod1.forecast)

library(TSPred)
plotarimapred(test.data,mod1,xlim = c(1996,2006),range.percent = 0.05)
accuracy(mod1.forecast,test.data)
