#test
library(tseries)
library(fGarch)

set.seed(42)
n <- 1000
params <- c(0.4, 0.3)
print(sum(params^2)<1/3)
print(sum(params)<1)
a0 <- 1

X1.spec <- garchSpec(model = list(omega = a0, alpha = params, beta = 0))
X1.data <- garchSim (spec = X1.spec, n = n)

X1.fit1 <- garch(x = X1.data, order = c(0,1), trace = F)
print(summary(X1.fit1))
X1.fit2 <- garch(x = X1.data, order = c(0,2), trace = F)
print(summary(X1.fit2))
X1.fit3 <- garch(x = X1.data, order = c(0,3), trace = F)
print(summary(X1.fit3))
X1.fit4 <- garch(x = X1.data, order = c(0,4), trace = F)
print(summary(X1.fit4))

loglik0 <- -0.5 * n * (1 + log(2 * pi * mean(X1.data^2)))
loglik1 <- logLik(X1.fit1)
loglik2 <- logLik(X1.fit2)
loglik3 <- logLik(X1.fit3)
loglik4 <- logLik(X1.fit4)

loglik <- c(loglik0, loglik1, loglik2, loglik3, loglik4)
q <- c(0, 1, 2, 3, 4)
k <- q + 1
aicc <- -2 * loglik  + 2 * k * n / (n - k - 1)
print(data.frame(q, loglik, aicc))

X1.model <- garch(X1.data, c(0, q[which.min(aicc)]), trace=F)
print(summary(X1.model))
print(X1.model)