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
adf.test(dax_df$Schlusskurs, alternative="stationary", k=0)
adf.test(dax_df$Schlusskurs, alternative="stationary", k=1)
adf.test(dax_df$Schlusskurs, alternative="stationary", k=2)
adf.test(dax_df$Schlusskurs, alternative="stationary", k=3)
adf.test(dax_df$Schlusskurs, alternative="stationary", k=4)
adf.test(dax_df$Schlusskurs, alternative="stationary", k=5)
# NOTE: If a root of the process's characteristic equation is larger than 1, then it is called an explosive process
adf.test(dax_df$Schlusskurs, alternative="explosive", k=0)
adf.test(dax_df$Schlusskurs, alternative="explosive", k=1)
adf.test(dax_df$Schlusskurs, alternative="explosive", k=2)
adf.test(dax_df$Schlusskurs, alternative="explosive", k=3)
adf.test(dax_df$Schlusskurs, alternative="explosive", k=4)
adf.test(dax_df$Schlusskurs, alternative="explosive", k=5)


# Calculate log returns
returns.log <- diff(log(dax_df$Schlusskurs))
plot.ts(returns.log, main="Daily Dax 30 Log Returns", ylab="Log Returns")

# NOTE: High Volatility -> High Volatility + vice versa

# Show statistics
summary(returns.log)

# Plot Autocorrelations
returns.acf <- autocorrelations(returns.log)
returns.pacf <- partialAutocorrelations(returns.log)
plot(returns.acf, data = returns.log)
plot(returns.pacf, data = returns.log)

###
# NOTE: There are two bounds plotted on the graph. The straight red line represents
# the standard bounds under the strong white noise assumption. The second line is under the
# hypothesis that the process is GARCH
# --> Either iid nor garch seem segnificant autocorrelated / partial autocorrelated
###

# Caluclate Arima model 
(fit <- auto.arima(returns.log, ic="aic"))

###
# --> ARIMA(0,0,0) with zero mean
# NOTE: An ARIMA(0,0,0) model with zero mean is white noise, so it means that the errors are uncorrelated across time.
# This doesn't imply anything about the size of the errors, so no in general it is not an indication of good or bad fit.
###


# Portmanteau test (Ljung–Box test) for Autocorrelation
Box.Ljung.Test(fit$residuals, lag = 20)

###
# NOTE: Small p-values lead to rejection of the null hypothesis at reasonable levels. Rejection of the
# null hypothesis is often taken to mean that the data are autocorrelated
###

# Ljung–Box test whether data suffers from ARCH effects
Box.Ljung.Test(fit$residuals^2, lag = 20)

# AFC and PACF
acf(fit$residuals, main="ACF Residuals", ylab="Residuals")
acf(fit$residuals^2, main="ACF Residuals^2", ylab="Residuals^2")

pacf(fit$residuals, main="PACF Resilduals", ylab="Residuals")
pacf(fit$residuals^2, main="PACF Residuals^2", ylab="Residuals^2")

# ---> p-value < 0.05 => Suffers from Arch effect -> GARCH model is applicable

### ARIMA-GARCH ###
# GARCH(1,1) based on ARIMA(0,0,0)
# In that case, the GARCH (p, q) model (where p is the order of the GARCH terms {\displaystyle ~\sigma ^{2}} ~\sigma ^{2} and q is the order of the ARCH terms {\displaystyle ~\epsilon ^{2}} ~\epsilon ^{2} )
garch.spec <- ugarchspec( variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
                          mean.model     = list(armaOrder = c(0, 0)))
(garch.fit <- ugarchfit(spec = garch.spec, data = returns.log))

plot(garch.fit)

# Forecast 10 periodes
ugarchforecast(garch.fit, n.ahead = 10)

plot(ugarchforecast(garch.fit, n.ahead = 10))

