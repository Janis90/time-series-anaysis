library(tseries)

# Estimation of alpha 0 and alpha 1

# Start with dataset X1
message("Testing X1 with ARCH(1) process")
arch1model1 <- garch(x = X1, order = c(0, 1))
print(arch1model1)
print(summary(arch1model1))

# Start with dataset X2
message("Testing X2 with ARCH(1) process")
arch1model2 <- garch(x = X2, order = c(0, 1))
print(arch1model2)
print(summary(arch1model2))

# Start with dataset X3
message("Testing X3 with ARCH(1) process")
arch1model3 <- garch(x = X3, order = c(0, 1))
print(arch1model3)
print(summary(arch1model3))