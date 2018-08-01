library(tseries)

# Estimation of alpha 0 and alpha 1

# Start with dataset X1
message("Testing X1 with GARCH(1,1) process")
garch1model1 <- garch(x = X1, order = c(1, 1))
print(garch1model1)
print(summary(garch1model1))

# Start with dataset X2
message("Testing X2 with GARCH(1,1) process")
garch1model2 <- garch(x = X2, order = c(1, 1))
print(garch1model2)
print(summary(garch1model2))

# Start with dataset X3
message("Testing X3 with GARCH(1,1) process")
garch1model3 <- garch(x = X3, order = c(1, 1))
print(garch1model3)
print(summary(garch1model3))