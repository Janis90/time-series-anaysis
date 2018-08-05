# Generate ARCH(3) data
library(fGarch)

N <- 100000
omega <- 1
alpha <- c(0.4, 0.2, 0.1)
beta <- 0
print(sum(alpha^2)<1/3)
print(sum(alpha) + sum(beta) < 1)

X.spec <- garchSpec(model = list(omega = omega, alpha = alpha, beta = beta), rseed = 42)
X <- garchSim (spec = X.spec, n = N)
plot(X[0:2000], type = "l")
pacf(X^2, lag.max = 30)

source("modelFit.r")