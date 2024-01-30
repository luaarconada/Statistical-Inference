#exercise 5.21
#1.bOOSTRAP FOR 5.4b

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
         xlab = latex2exp::TeX("$\\hat{\\theta}ˆ*$"))
    rug(theta_hat_star)
    abline(v = ci, col = 2, lwd = 2)
    abline(v = boot_obj$t0, col = 3, lwd = 2)
  }
  # Return confidence intervals
  return(ci)
}
x=c(6.2, 7.3, 5.5, 6.7, 9.0, 7.1, 5.0, 6.3, 7.2, 7.5, 8.0, 7.9, 6.5, 6.1, 7.0)
boot_ci(x,var)

#5.6
x=c(2072, 2726, 2254, 3029, 2283, 2673, 2401, 2463, 2416, 2909,
    2385, 2055, 2139, 2553, 2453, 2621, 2808, 3583, 2662, 2732,
    1996, 2164, 2036, 2915, 2507, 3245, 2337, 2672, 3089, 3012,
    2725, 2467, 2439, 2692, 1793, 3845, 2523, 2585, 1647, 2072,
    1988, 2767, 3679, 2280, 2642, 3112, 2512, 2435, 2820, 2142,
    3197, 3103, 2130, 3213, 2464, 2243, 2958, 2529, 2160, 2484)
theta_hat=function(x){mean(log(x))}
boot_ci(x,theta_hat)
