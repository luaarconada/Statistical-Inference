## Exercise 6.11.

# Define the data
stock1 <- c(38.83, 40.67, 41.68, 37.42, 40.86, 40.96, 39.62, 39.65, 39.63, 39.23, 39.74, 39.09, 39.37, 40.41, 41.52, 40.19)
stock2 <- c(42.76, 43.93, 44.08, 41.71, 44.27, 43.73, 40.12, 41.6, 43.22, 44.83, 41.69, 44.16, 45.42, 44.7, 42.04, 44.21)

# Compute the observed statistic
quasivar1 <- var(stock1)
quasivar2 <- var(stock2)
F_statistic <- quasivar1/quasivar2

# Find the p-value
p1 <- pf(F_statistic, df1=15, df2=15)
p2 <- pf(F_statisitc, df1=15, df2=15, lower.tail = F)
p_value <- 2*min(p1,p2) # As the F distribution is not symmetric and it is a two-sided test
p_value

# Check the results using var.test()
result <- var.test(stock1, stock2, ratio=1, alternative="two.sided")


# Significance level
alpha <- 0.02

# Calculate critical values
f_left <- qf(alpha/2, df1=15, df2=15)
f_right <- qf(1 - alpha/2, df1=15, df2=15)

# Generate a sequence of x values
x <- seq(0, 4, length = 1000)

# Calculate the probability density values and plot the density function
y <- df(x, df1=15, df2=15)
plot(x, y, type = "l", xlab = "F-statistic", ylab = "Density", main = "Two sided F-Test")

# Shade the p-value and critical region
polygon(c(0, x[x <= F_statistic], F_statistic), c(0, y[x <= F_statistic], 0), col = "lightblue")
polygon(c(0, x[x <= f_left], f_left), c(0, y[x <= f_left], 0), col = "blue")
polygon(c(f_right, x[x >= f_right], f_right), c(0, y[x >= f_right], 0), col = "blue")
abline(h = 0, col = "gray")
abline(v = f_left, col = "blue")
abline(v = f_right, col = "blue")

# Draw a dashed vertical line for the statistic value
abline(v = F_statistic,lty = 2, col = "red")

# Display the value of the statistic and alpha and create a legend
text(0.8,0.05,paste("F=",round(F_statistic, 4)), col = "red")
text(3,0.2,"alpha=0.02", col = "blue")
legend("topright", legend = c("Critical Region", "p-value", "F-statistic"), fill = c("blue", "lightblue","red"))

