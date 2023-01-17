#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Importing dependencies 
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(TSA)
library(rugarch)
library(fGarch)
library(PerformanceAnalytics)
library(rugarch)
library(ggplot2)
library(vioplot)
library(MASS)
library(mcp)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Dow Jones Industrial Avg Data pulled from Yahoo Finance
DJI_df <- getSymbols("^DJI",src="yahoo",from="2010-01-01",to = "2019-01-01", auto.assign = FALSE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Calculating returns
stock_ret <- dailyReturn(DJI_df[, 4], type='log')
#stock_ret <- diff(log(DJI_df[,4]), lag = 1)
plot(stock_ret, main="DJI Returns")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# EDA

# Finding mean 
summary(stock_ret)

# Histogram: stock return distribution 
hist(stock_ret, main = "DJI stock return distribution", probability = TRUE)
lines(density(stock_ret))

# Box and whisker plot: stock return distribution 
boxplot(stock_ret, horizontal=TRUE, main = "DJI stock return Boxplot")

# Violin Plot: stock return distribution 
vioplot(stock_ret, horizontal=TRUE, main = "DJI stock return Violinplot")

# Q-Q plot: stock return distribution
qqnorm(stock_ret)
qqline(stock_ret, col = "red", main = "DJI stock return Q-Q Plot")

# plotting Yt versus Yt-1
plot(y=stock_ret,x=zlag(stock_ret),
     ylab=expression(Y[t]),xlab=expression(Y[t-2]),
     type='p', main = "Plot of Yt versus Yt − 1 ")

# plotting Yt versus Yt-2
plot(y=stock_ret,x=zlag(stock_ret, 2),
     ylab=expression(Y[t]),xlab=expression(Y[t-2]),
     type='p', main = "Plot of Yt versus Yt − 2 ")

# plotting Yt versus Yt-3
plot(y=stock_ret,x=zlag(stock_ret, 3),
     ylab=expression(Y[t]),xlab=expression(Y[t-2]),
     type='p', main = "Plot of Yt versus Yt − 3 ")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ACF/PACF/EACF Functions
par(mfrow = c(1,2))
acf(stock_ret, main = 'ACF Plot', lag.max = 25)
pacf(stock_ret, main = 'PACF Plot', lag.max = 25)
eacf(stock_ret)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## Fitting an ARIMA model  

auto.arima(stock_ret, D=1,approximation = F,allowdrift = T,allowmean = T, trace = T)

model <-auto.arima(stock_ret, D=1,approximation = F,allowdrift = T,allowmean = T)

## Checking the residuals   
checkresiduals(model)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Forecasting
plot(as.ts(ret), main="ARIMA(2,0,2) Model Fit with training data")
lines(fitted(model), col="red")

futurVal <- forecast(model,h=252, level=c(99), main="Forecasts with ARIMA(2,0,2)")
plot(forecast(futurVal))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

stock_ret_sq <- stock_ret^2
plot(stock_ret_sq, main="DJI Returns sqaured")

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

# EDA

# Finding mean 
summary(stock_ret_sq)

# Histogram: stock return distribution 
hist(stock_ret_sq, main = "DJI Returns sqaured distribution", probability = TRUE)
lines(density(stock_ret_sq))

# Box and whisker plot: stock return distribution 
boxplot(stock_ret_sq, horizontal=TRUE, main = "DJI Returns sqaured Boxplot")

# Violin Plot: stock return distribution 
vioplot(stock_ret_sq, horizontal=TRUE, main = "DJI Returns sqaured Violinplot")

# Q-Q plot: stock return distribution
qqnorm(stock_ret_sq)
qqline(stock_ret_sq, col = "red", main = "DJI Returns sqaured Q-Q Plot")

# plotting Yt versus Yt-1
plot(y=stock_ret_sq,x=zlag(stock_ret_sq),
     ylab=expression(Y[t]),xlab=expression(Y[t-2]),
     type='p', main = "Plot of Yt versus Yt − 1 ")

# plotting Yt versus Yt-2
plot(y=stock_ret_sq,x=zlag(stock_ret_sq, 2),
     ylab=expression(Y[t]),xlab=expression(Y[t-2]),
     type='p', main = "Plot of Yt versus Yt − 2 ")

# plotting Yt versus Yt-3
plot(y=stock_ret_sq,x=zlag(stock_ret_sq, 3),
     ylab=expression(Y[t]),xlab=expression(Y[t-2]),
     type='p', main = "Plot of Yt versus Yt − 3 ")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# ACF/PACF/EACF Functions
par(mfrow = c(1,2))
acf(stock_ret_sq, main = 'ACF Plot')
pacf(stock_ret_sq, main = 'PACF Plot')
eacf(stock_ret_sq)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
## Fitting an ARIMA model  

auto.arima(stock_ret_sq, D=1,approximation = F,allowdrift = T,allowmean = T, trace = T)

model_2 <-auto.arima(stock_ret_sq, D=1,approximation = F,allowdrift = T,allowmean = T)

## Checking the residuals   
checkresiduals(model_2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Forecasting
plot(as.ts(stock_ret_sq), main="Model Fit with training data")
lines(fitted(model_2), col="red")

futurVal_2 <- forecast(model_2,h=50, level=c(99), main="Forecasts with ARIMA(2,0,2)")
plot(forecast(futurVal_2))

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#ts.plot(train, type = 'l')
autoplot(stock_ret_sq)
stock_ret_sq %>% BoxCox(lambda = 0) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = .1) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = .2) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = .3) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = .5) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = .7) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = .9) %>% autoplot()
stock_ret_sq %>% BoxCox(lambda = 1) %>% autoplot()
lambda <- BoxCox.lambda(stock_ret_sq)
lambda
series2 <- BoxCox(stock_ret_sq, lambda = lambda)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#                          VOLATILITY CLUSTERING

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


library(fpp)
library(ecdat)
plot(as.ts(stock_ret))

# Rferences: https://www.youtube.com/watch?v=lKBgQ4MxM3Y
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Importing dependencies 
library(quantmod)
library(tseries)
library(timeSeries)
library(forecast)
library(xts)
library(TSA)
library(rugarch)
library(fGarch)
library(PerformanceAnalytics)
library(rugarch)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Dow Jones Industrial Avg Data pulled from Yahoo Finance
DJI_df <- getSymbols("^DJI",src="yahoo",from="2010-01-01",to = "2019-01-01", auto.assign = FALSE)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Calculating returns
ret <- dailyReturn(DJI_df[, 4], type='log')
plot(ret)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Plotting ACF/PACF/EACF
par(mfrow=c(1,2))
acf(ret, main="Return ACF");
pacf(ret, main="Return PACF");
eacf(ret)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
m=mean(ret);s=sd(ret);
par(mfrow=c(1,2))
hist(ret, nclass=40, freq=FALSE, main='Return histogram');curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(ret), main='Return empirical distribution');curve(dnorm(x, mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
par(mfrow=c(1,1))


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# density plots on log-scale
plot(density(ret), xlim=c(-5*s,5*s),log='y', main='Density on log-scale')
curve(dnorm(x, mean=m,sd=s), from=-5*s, to=5*s, log="y", add=TRUE, col="red")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# QQ-plot
qqnorm(ret);qqline(ret);


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Compute the daily standard deviation for the complete sample  
sd(ret)

# Showing two plots on the same figure
par(mfrow=c(1,2)) 

# Compute the rolling 1 month estimate of annualized volatility
chart.RollingPerformance(R = ret, width = 22,
                         FUN = "sd.annualized", scale = 252, main = "One month rolling volatility")

# Compute the rolling 3 months estimate of annualized volatility
chart.RollingPerformance(R = ret, width = 66,
                         FUN = "sd.annualized", scale = 252, main = "Three months rolling volatility")  


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Fitting model 
auto.arima(ret)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Modeling Volatility using GARCH
g1<-ugarchspec(variance.model=list(model="sGARCH",garchOrder=c(1,1)),
               mean.model=list(armaOrder=c(2,2)),distribution.model="std")

# examine the coefficients
garch11<-ugarchfit(g1,data = ret)
garch11

# making a GARCH variance series
vole <- ts(garch11@fit$sigma^2,start=c(2010, 01), end=c(2019, 01), frequency=252)
plot(vole,xlab="",ylab="",main="DJI returns Volatility (GARCH[1,1])")


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Alternative Modeling Volatility using EGARCH
g1e<-ugarchspec(variance.model=list(model="eGARCH",garchOrder=c(1,1)),
                mean.model=list(armaOrder=c(2,2)),distribution.model="std")

# examine the coefficients
garch11e<-ugarchfit(g1e,data = ret)
coef(garch11e)

# making a GARCH variance series
vole2 <- ts(garch11e@fit$sigma^2,start=c(2010, 01), end=c(2019, 01), frequency=252)
plot(vole2,xlab="",ylab="",main="DJI returns Volatility (EGARCH[1,1])")

# comparing correlation btwn models
cor(vole,vole2)
ts.plot(vole,vole2,col=c("green","red"),xlab="")
legend("topright",legend=c("Standard","Exponential"),col=c("green","red"),lty=c(1,1))

# Plotting in 2 measures 
voldif<-vole-vole2
plot(voldif,xlab="")
abline(h=0,col="dark grey",lty=2)

# difference in measures vs. exchange rate returns.
plot(voldif,ret,pch=20,xlab="GARCH - EGARCH")
abline(h=0,lty=2)
abline(v=0,lty=2)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# estimate GARCH(1,1) model of daily changes
GARCH_fit <- garchFit(data = ret, trace = F)

# compute deviations of the changes from their mean
dev_mean_GARCH <- ret - GARCH_fit@fit$coef[1]

# plot deviation of changes from mean
plot(time(ret), dev_mean_GARCH, 
     type = "l", 
     col = "steelblue",
     ylab = "Percent", 
     xlab = "Date",
     main = "Estimated Bands of +- One Conditional Standard Deviation",
     lwd = 0.2)

# add horizontal line at y = 0
abline(0, 0)

# add GARCH(1,1) confidence bands (one standard deviation) to the plot
lines(time(ret), 
      GARCH_fit@fit$coef[1] + GARCH_fit@sigma.t, 
      col = "darkred", 
      lwd = 0.5)

lines(time(ret), 
      GARCH_fit@fit$coef[1] - GARCH_fit@sigma.t, 
      col = "darkred", 
      lwd = 0.5)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
