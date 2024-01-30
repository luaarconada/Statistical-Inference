
### Exercise 5.22

## We first need to create the *sample* for the before-voting-day poll. For
## that, we need to know how many people responded A in that poll

# Sample size and proportion option A
n_1 <- 704
p_A_1 <- 0.535

# Number of people voting A
p_A_1 * n_1

# It is not a natural number, so let's round it
n_A_1 <- round(p_A_1 * n_1)
n_A_1

# The proportions are then:
c(n_A_1 / n_1, 1 - n_A_1 / n_1)
# This seems compatible with a rounding to one digit, as it is reported

# The sample is then (1 = A, 0 = B). Note we do not care about the order of
# the data, only the proportion of each class!
x_1 <- c(rep(1, n_A_1), rep(0, n_1 - n_A_1))

## We do the same for the second sample

# Sample size and proportion option A
n_2 <- 167
n_A_2 <- 77

# The sample
x_2 <- c(rep(1, n_A_2), rep(0, n_2 - n_A_2))

## We now use boot_ci() with the proportion estimator

# Computation of percentile bootstrap confidence intervals. It requires a
# sample x and an estimator theta_hat() (must be a function!).
boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.05, plot_boot = TRUE) {

  # Check that theta_hat is a proper function
  stopifnot(is.function(theta_hat))

  # Creates convenience statistic for boot::boot()
  stat <- function(x, indexes) theta_hat(x[indexes])

  # Perform bootstrap resampling (step 2) with the aid of boot::boot()
  boot_obj <- boot::boot(data = x, statistic = stat, sim = "ordinary", R = B)

  # Extract bootstrapped statistics (theta_hat_star's) from the boot object
  theta_hat_star <- boot_obj$t

  # Confidence intervals
  ci <- quantile(theta_hat_star, probs = c(alpha / 2, 1 - alpha / 2))

  # Plot the distribution of bootstrap estimators and the confidence intervals?
  # Draw also theta_hat, the statistic from the original sample, in boot_obj$t0
  if (plot_boot) {

    hist(theta_hat_star, probability = TRUE, main = "",
         xlab = latex2exp::TeX("$\\hat{\\theta}^*$"))
    rug(theta_hat_star)
    abline(v = ci, col = 2, lwd = 2)
    abline(v = boot_obj$t0, col = 3, lwd = 2)

  }

  # Return confidence intervals
  return(ci)

}

# Fist sample
boot_ci(x = x_1, theta_hat = function(x) mean(x), B = 1e4, alpha = 0.01)

# Second sample
boot_ci(x = x_2, theta_hat = function(x) mean(x), B = 1e4, alpha = 0.01)

# The confidence interval has shifted from "almost sure victory for A" (poll 1)
# to "lead of B with chance for A winning" (poll 2). The news channel can report
# that there appears to be a shift of trend but that it is "too close to call"

# We can play with the confidence level to make this claim more precise:
boot_ci(x = x_1, theta_hat = function(x) mean(x), B = 1e4, alpha = 0.10)
boot_ci(x = x_2, theta_hat = function(x) mean(x), B = 1e4, alpha = 0.10)

# At the 90% confidence, the situation went from "victory for A" (poll 1) to "B ahead, A with
# narrow chances of winning".

# Further precision could be obtained by doing one-sided confidence intervals.
