# Generate GARCH(1,1) data
library(fGarch)

set.seed(42)
N <- 100000
omega <- 1
alpha <- c(0.1)
beta <- c(0.8)
print(sum(alpha^2)<1/3)
print(sum(alpha) + sum(beta) < 1)
print(3*alpha^2 + 2*alpha*beta + beta^2 < 1)

X.spec <- garchSpec(model = list(omega = omega, alpha = alpha, beta = beta))
X <- garchSim (spec = X.spec, n = N)

source("modelFit.r")