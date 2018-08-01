library(tseries)

# Estimation of alpha 0, alpha 1, aplha 2 and alpha 3

# Start with dataset X1
message("Testing X1 with ARCH(3) process")
arch3model1 <- garch(x = X1, order = c(0, 3))
print(arch3model1)
print(summary(arch3model1))

# Start with dataset X2
message("Testing X2 with ARCH(3) process")
arch3model2 <- garch(x = X2, order = c(0, 3))
print(arch3model2)
print(summary(arch3model2))

# Start with dataset X3
message("Testing X3 with ARCH(3) process")
arch3model3 <- garch(x = X3, order = c(0, 3))
print(arch3model3)
print(summary(arch3model3))