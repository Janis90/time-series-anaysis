# Generate GARCH(1,1) data
library(fGarch)

N <- 100000
omega <- 1
alpha <- c(0.1)
beta <- c(0.8)
print(sum(alpha^2)<1/3)
print(sum(alpha) + sum(beta) < 1)
print(3*alpha^2 + 2*alpha*beta + beta^2 < 1)

X.spec <- garchSpec(model = list(omega = omega, alpha = alpha, beta = beta), rseed = 42)
X <- garchSim (spec = X.spec, n = N)
plot(X[0:2000], type = "l")
pacf(X^2, lag.max = 30)

source("modelFit.r")