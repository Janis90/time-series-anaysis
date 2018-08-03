# Generate ARCH(3) data
library(fGarch)

set.seed(42)
N <- 1000
omega <- 1
alpha <- c(0.4, 0.2, 0.1)
beta <- 0
print(sum(alpha^2)<1/3)
print(sum(alpha) + sum(beta) < 1)

X.spec <- garchSpec(model = list(omega = omega, alpha = alpha, beta = beta))
X <- garchSim (spec = X.spec, n = N)

source("modelFit.r")