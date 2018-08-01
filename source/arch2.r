library(tseries)

# Estimation of alpha 0, alpha 1 and aplha 2

# Start with dataset X1
message("Testing X1 with ARCH(2) process")
arch2model1 <- garch(x = X1, order = c(0, 2))
print(arch2model1)
print(summary(arch2model1))

# Start with dataset X2
message("Testing X2 with ARCH(2) process")
arch2model2 <- garch(x = X2, order = c(0, 2))
print(arch2model2)
print(summary(arch2model2))

# Start with dataset X3
message("Testing X3 with ARCH(2) process")
arch2model3 <- garch(x = X3, order = c(0, 2))
print(arch2model3)
print(summary(arch2model3))