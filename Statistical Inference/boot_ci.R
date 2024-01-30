#function for making bootstrap CI for alpha=0.01
boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.1, plot_boot = FALSE) {
  
  # Check that theta_hat is a proper function
  stopifnot(is.function(theta_hat))
  
  # Creates convenience statistic for boot::boot()
  stat <- function(x, indexes) theta_hat(x[indexes])
  
  # Perform bootstrap resampling with the aid of boot::boot()
  boot_obj <- boot::boot(data = x, statistic = stat, sim = "ordinary", R = B)
  
  # Extract bootstrapped statistics from the boot object
  theta_hat_star <- boot_obj$t
  
  # Confidence intervals
  ci <- quantile(theta_hat_star, probs = c(alpha / 2, 1 - alpha / 2))
  
  # Plot the distribution of bootstrap estimators and the confidence intervals?
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