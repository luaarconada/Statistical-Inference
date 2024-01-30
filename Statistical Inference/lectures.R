
## Non-convergence of the sample mean of Cauchy

n <- 1e4
M <- 100
x1 <- matrix(rcauchy(n * M), nrow = n, ncol = M)
matplot(apply(x1, 2, cumsum) / (1:n), type = "l", lty = 1)

## Convergence of the sample mean of N(0, 1)

x2 <- matrix(rnorm(n * M), nrow = n, ncol = M)
matplot(apply(x2, 2, cumsum) / (1:n), type = "l", lty = 1)

## Other codes are in each script


# Change
