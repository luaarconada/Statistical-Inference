# Estimators
lambda_hat <- function(x, k) {
  mean(xˆk)ˆ(1 / k)
}
I_hat <- function(x, lambda, k) {
  (k / lambda)ˆ2 * mean(((x / lambda)ˆk - 1)ˆ2)
}
# Sample
x <- c(1.66, 0.94, 1.54, 1.27, 1.78, 1.85, 2.34, 1.74, 1.32, 0.6, 0.88, 2.33,
       1.49, 0.7, 0.73, 2.32, 1.92, 2.6, 2.51, 2.09, 3.09, 1.1, 1.76, 2.74, 2.1,
       0.73, 1.47)
n <- length(x)
# Statistic
lambda_0 <- 1
(lamb <- lambda_hat(x = x, k = 2))

(I_0 <- I_hat(x = x, lambda = lambda_0, k = 2)
  
(z <- sqrt(n * I_0) * (lamb - lambda_0))

# p-values
pnorm(z, lower.tail = FALSE)

2 * min(pnorm(z), pnorm(z, lower.tail = FALSE))