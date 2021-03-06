library(tseries)
library(forecast)
library(sarima)
# Better Ljung Box Test
library(LSTS)

library(rugarch)

dax_df <- read.csv("dax_30_2015-2017.csv", sep = ";")

# Sanitize data and transform daily closing values into numeric values 
dax_df$Schlusskurs <- gsub("[.]", "", dax_df$Schlusskurs)
dax_df$Schlusskurs <- gsub("[,]", ".", dax_df$Schlusskurs)
dax_df$Schlusskurs <- as.numeric(dax_df$Schlusskurs) 

# Plot Time Series 

plot.ts(dax_df$Schlusskurs, main="Daily Dax 30 Closing Prices", ylab="Closing Price")
summary(dax_df$Schlusskurs)

# Dickey-Fuller test
# Specify lag muss angegeben werden
adf.test(dax_df$Schlusskurs, alternative="stationary", k=0)
# If a root of the process's characteristic equation is larger than 1, then it is called an explosive process
adf.test(dax_df$Schlusskurs, alternative="explosive", k=0)

# Calculate log returns
returns.log <- diff(log(dax_df$Schlusskurs))
plot.ts(returns.log, main="Daily Dax 30 Log Returns", ylab="Log Returns")

###
#  -->High Volatility -> High Volatility + vice versa
###

# Show statistics
summary(returns.log)

# Plot Autocorrelations
returns.acf <- autocorrelations(returns.log)
returns.pacf <- partialAutocorrelations(returns.log)

plot(returns.acf, data = returns.log)
plot(returns.pacf, data = returns.log)


###
# There are two bounds plotted on the graph. The straight red line represents
# the standard bounds under the strong white noise assumption. The second line is under the
# hypothesis that the process is GARCH
# --> Either iid nor garch seem segnificant autocorrelated / partial autocorrelated
###

# Calucalte Arima model 
(fit <- auto.arima(returns.log, ic="aic"))

###
# --> ARIMA(0,0,0) with zero mean
# An ARIMA(0,0,0) model with zero mean is white noise, so it means that the errors are uncorrelated across time.
# This doesn't imply anything about the size of the errors, so no in general it is not an indication of good or bad fit.
###


#### #####
# Portmanteau test (Ljung–Box test) for Autocorrelation
Box.Ljung.Test(fit$residuals, lag = 20)

###
# Small p-values lead to rejection of the null hypothesis at reasonable levels. Rejection of the
# null hypothesis is often taken to mean that the data are autocorrelated
###

# Ljung–Box test whether data suffers from ARCH effects
Box.Ljung.Test(fit$residuals^2, lag = 20)

#par(mfrow=c(1,2))
acf(fit$residuals, main="ACF Resulduals", ylab="Residuals")
acf(fit$residuals^2, main="ACF Resulduals^2", ylab="Residuals^2")
#par(mfrow=c(1,1))

###
# ---> p-value < 0.05 => Suffers from Arch effect -> GARCH model is applicable
###

### ARIMA-GARCH###
# GARCH(1,1) based on ARIMA(0,0,0)
# In that case, the GARCH (p, q) model (where p is the order of the GARCH terms {\displaystyle ~\sigma ^{2}} ~\sigma ^{2} and q is the order of the ARCH terms {\displaystyle ~\epsilon ^{2}} ~\epsilon ^{2} )
# TODO: garch parameter möglichst groß wählen -> etsten was am besten passt
garch.spec <- ugarchspec( variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model     = list(armaOrder = c(0, 0)))
(garch.fit <- ugarchfit(spec = garch.spec, data = returns.log))

###
# Conditional Variance Dynamics 	
#-----------------------------------
# GARCH Model	: sGARCH(1,1)
# Mean Model	: ARFIMA(0,0,0)
# Distribution	: norm 
###

plot(garch.fit)

# Auch analysieren + 1-2 enthalten informatioenen

# Forecast 10 periods
ugarchforecast(garch.fit, n.ahead = 10)

plot(ugarchforecast(garch.fit, n.ahead = 10))

# Sigma etc  (Varianz) analyisiern

