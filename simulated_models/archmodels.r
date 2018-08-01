library(fGarch)

set.seed(42)
n <- 10000

# Set model parameters that meet the requirements: a1 + a2 + a3 < 1 and a1^2 + a2^2 + a3^2 < 1/3 
a0 <- 0.5
a1 <- 0.3
a2 <- 0.1
a3 <- 0.2

X1 <- rnorm(n)
eta <- rnorm(n)
sigma2_1 <- rep(a0, n)

# Model ARCH(1) process (approach without garch data simulation of package fGarch)
for (t in 2:n){sigma2_1[t] <- a0 + a1*X1[t-1]^2; X1[t] <- eta[t] * sqrt(sigma2_1[t]);}
plot(X1, type = "l")
pacf(X1^2)
## Alternative:
# X1_spec <- garchSpec(model = list(omega = a0, alpha = c(a1), beta = 0))
# X1 <- garchSim(spec = X1_spec, n = n)


# Model ARCH(2) process (now with the help of the library)
X2 <- garchSim(spec = garchSpec(model = list(omega = a0, alpha = c(a1, a2), beta = 0)), n = n)
plot(X2, type = "l")
pacf(X2^2)

# Model ARCH(3) process (now with the help of the library)
X3 <- garchSim(spec = garchSpec(model = list(omega = a0, alpha = c(a1, a2, a3), beta = 0)), n = n)
plot(X3, type = "l")
pacf(X3^2)