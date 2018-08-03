#test

set.seed(42)
n <- 600
params <- c(0.5)
print(sum(params^2)<1/3)
print(sum(params)<1)
a0 <- 1

X1.data <- rnorm(n)
eta <- rnorm(n)
sigma2 <- rep(a0, n)

for (t in 2:n){sigma2[t] <- a0 + params[1]*X1.data[t-1]^2; X1.data[t] <- eta[t] * sqrt(sigma2[t]);}

X1.fit1 <- garch(x = X1.data, order = c(0,1), trace = F)
X1.fit2 <- garch(x = X1.data, order = c(0,2), trace = F)
X1.fit3 <- garch(x = X1.data, order = c(0,3), trace = F)
X1.fit4 <- garch(x = X1.data, order = c(0,4), trace = F)

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

X1.model <- 
