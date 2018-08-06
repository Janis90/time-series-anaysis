# Import libraries
library(tseries)
library(MuMIn)
library(SciViews)
library(xlsx)

# Setup params
modelfit.arch1.params <- c(0,1)
modelfit.arch2.params <- c(0,2)
modelfit.arch3.params <- c(0,3)
modelfit.arch4.params <- c(0,4)
modelfit.arch5.params <- c(0,5)
modelfit.arch6.params <- c(0,6)
modelfit.arch7.params <- c(0,7)
modelfit.arch8.params <- c(0,8)
modelfit.garch.params <- c(1,1)

#Setup number of order
modelfit.arch1.q <- sum(modelfit.arch1.params)
modelfit.arch2.q <- sum(modelfit.arch2.params)
modelfit.arch3.q <- sum(modelfit.arch3.params)
modelfit.arch4.q <- sum(modelfit.arch4.params)
modelfit.arch5.q <- sum(modelfit.arch5.params)
modelfit.arch6.q <- sum(modelfit.arch6.params)
modelfit.arch7.q <- sum(modelfit.arch7.params)
modelfit.arch8.q <- sum(modelfit.arch8.params)
modelfit.garch.q <- sum(modelfit.garch.params)

#Setup number of params
modelfit.arch1.k <- sum(modelfit.arch1.params)
modelfit.arch2.k <- sum(modelfit.arch2.params)
modelfit.arch3.k <- sum(modelfit.arch3.params)
modelfit.arch4.k <- sum(modelfit.arch4.params)
modelfit.arch5.k <- sum(modelfit.arch5.params)
modelfit.arch6.k <- sum(modelfit.arch6.params)
modelfit.arch7.k <- sum(modelfit.arch7.params)
modelfit.arch8.k <- sum(modelfit.arch8.params)
modelfit.garch.k <- sum(modelfit.garch.params)

# Fit with different model params

# ARCH(q) processes
modelfit.arch1.fit <- garch(x = X, order = modelfit.arch1.params, trace = F)
modelfit.arch2.fit <- garch(x = X, order = modelfit.arch2.params, trace = F)
modelfit.arch3.fit <- garch(x = X, order = modelfit.arch3.params, trace = F)
modelfit.arch4.fit <- garch(x = X, order = modelfit.arch4.params, trace = F)
modelfit.arch5.fit <- garch(x = X, order = modelfit.arch5.params, trace = F)
modelfit.arch6.fit <- garch(x = X, order = modelfit.arch6.params, trace = F)
modelfit.arch7.fit <- garch(x = X, order = modelfit.arch7.params, trace = F)
modelfit.arch8.fit <- garch(x = X, order = modelfit.arch8.params, trace = F)
modelfit.garch.fit <- garch(x = X, order = modelfit.garch.params, trace = F)

# Compute loglikelihood per model fit
modelfit.arch1.loglik <- logLik(modelfit.arch1.fit)
modelfit.arch2.loglik <- logLik(modelfit.arch2.fit)
modelfit.arch3.loglik <- logLik(modelfit.arch3.fit)
modelfit.arch4.loglik <- logLik(modelfit.arch4.fit)
modelfit.arch5.loglik <- logLik(modelfit.arch5.fit)
modelfit.arch6.loglik <- logLik(modelfit.arch6.fit)
modelfit.arch7.loglik <- logLik(modelfit.arch7.fit)
modelfit.arch8.loglik <- logLik(modelfit.arch8.fit)
modelfit.garch.loglik <- logLik(modelfit.garch.fit)

# Compute AIC
modelfit.arch1.AIC <- AIC(modelfit.arch1.fit)
modelfit.arch2.AIC <- AIC(modelfit.arch2.fit)
modelfit.arch3.AIC <- AIC(modelfit.arch3.fit)
modelfit.arch4.AIC <- AIC(modelfit.arch4.fit)
modelfit.arch5.AIC <- AIC(modelfit.arch5.fit)
modelfit.arch6.AIC <- AIC(modelfit.arch6.fit)
modelfit.arch7.AIC <- AIC(modelfit.arch7.fit)
modelfit.arch8.AIC <- AIC(modelfit.arch8.fit)
modelfit.garch.AIC <- AIC(modelfit.garch.fit)

# Compute AICc
modelfit.arch1.AICc <- -2 * modelfit.arch1.loglik + 2 * (modelfit.arch1.k) * N / (N - (modelfit.arch1.k) - 1)
modelfit.arch2.AICc <- -2 * modelfit.arch2.loglik + 2 * (modelfit.arch2.k) * N / (N - (modelfit.arch2.k) - 1)
modelfit.arch3.AICc <- -2 * modelfit.arch3.loglik + 2 * (modelfit.arch3.k) * N / (N - (modelfit.arch3.k) - 1)
modelfit.arch4.AICc <- -2 * modelfit.arch4.loglik + 2 * (modelfit.arch4.k) * N / (N - (modelfit.arch4.k) - 1)
modelfit.arch5.AICc <- -2 * modelfit.arch5.loglik + 2 * (modelfit.arch5.k) * N / (N - (modelfit.arch5.k) - 1)
modelfit.arch6.AICc <- -2 * modelfit.arch6.loglik + 2 * (modelfit.arch6.k) * N / (N - (modelfit.arch6.k) - 1)
modelfit.arch7.AICc <- -2 * modelfit.arch7.loglik + 2 * (modelfit.arch7.k) * N / (N - (modelfit.arch7.k) - 1)
modelfit.arch8.AICc <- -2 * modelfit.arch8.loglik + 2 * (modelfit.arch8.k) * N / (N - (modelfit.arch8.k) - 1)
modelfit.garch.AICc <- -2 * modelfit.garch.loglik + 2 * (modelfit.garch.k) * N / (N - (modelfit.garch.k) - 1)


# Compute BIC
modelfit.arch1.BIC <- ln(N) * (modelfit.arch1.k) -2*modelfit.arch1.loglik
modelfit.arch2.BIC <- ln(N) * (modelfit.arch2.k) -2*modelfit.arch2.loglik
modelfit.arch3.BIC <- ln(N) * (modelfit.arch3.k) -2*modelfit.arch3.loglik
modelfit.arch4.BIC <- ln(N) * (modelfit.arch4.k) -2*modelfit.arch4.loglik
modelfit.arch5.BIC <- ln(N) * (modelfit.arch5.k) -2*modelfit.arch5.loglik
modelfit.arch6.BIC <- ln(N) * (modelfit.arch6.k) -2*modelfit.arch6.loglik
modelfit.arch7.BIC <- ln(N) * (modelfit.arch7.k) -2*modelfit.arch7.loglik
modelfit.arch8.BIC <- ln(N) * (modelfit.arch8.k) -2*modelfit.arch8.loglik
modelfit.garch.BIC <- ln(N) * (modelfit.garch.k) -2*modelfit.garch.loglik

# Setup data frame
orders <- c(modelfit.arch1.k, modelfit.arch2.k, modelfit.arch3.k, modelfit.arch4.k, modelfit.arch5.k, modelfit.arch6.k, modelfit.arch7.k, modelfit.arch8.k, modelfit.garch.k)
params <- list(modelfit.arch1.params, modelfit.arch2.params, modelfit.arch3.params, modelfit.arch4.params, modelfit.arch5.params, modelfit.arch6.params, modelfit.arch7.params, modelfit.arch8.params, modelfit.garch.params)
loglik <- c(modelfit.arch1.loglik, modelfit.arch2.loglik, modelfit.arch3.loglik, modelfit.arch4.loglik, modelfit.arch5.loglik, modelfit.arch6.loglik, modelfit.arch7.loglik, modelfit.arch8.loglik, modelfit.garch.loglik)
AIC <- c(modelfit.arch1.AIC, modelfit.arch2.AIC, modelfit.arch3.AIC, modelfit.arch4.AIC, modelfit.arch5.AIC, modelfit.arch6.AIC, modelfit.arch7.AIC, modelfit.arch8.AIC, modelfit.garch.AIC)
AICc <- c(modelfit.arch1.AICc, modelfit.arch2.AICc, modelfit.arch3.AICc, modelfit.arch4.AICc, modelfit.arch5.AICc, modelfit.arch6.AICc, modelfit.arch7.AICc, modelfit.arch8.AICc, modelfit.garch.AICc)
BIC <- c(modelfit.arch1.BIC, modelfit.arch2.BIC, modelfit.arch3.BIC, modelfit.arch4.BIC, modelfit.arch5.BIC, modelfit.arch6.BIC, modelfit.arch7.BIC, modelfit.arch8.BIC, modelfit.garch.BIC)

informationcriterions <- data.frame(orders, loglik, AIC, AICc, BIC)
print(informationcriterions)
write.xlsx(informationcriterions, "ics.xlsx")
X.model <- garch(x = X, order = params[[which.min(BIC)]])
print(summary(X.model))
print(X.model)

print(Box.test(residuals(X.model)))
