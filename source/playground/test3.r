

set.seed(42)
N <- 100000
omega <- 1
alpha <- 0.1
beta <- 0.8
print(sum(alpha^2)<1/3)
print(sum(alpha) + sum(beta) < 1)
print(3*alpha^2 + 2*alpha*beta + beta^2 < 1)

X.spec <- garchSpec(model = list(omega = omega, alpha = alpha, beta = beta))
X <- garchSim (spec = X.spec, n = N)


fit <- garch(x = X, order = c(1,1))
garchFit(formula = ~garch(1,1), data = X)