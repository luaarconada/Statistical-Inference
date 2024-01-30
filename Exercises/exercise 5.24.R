#exercise 5.24
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
  #EN ESTE CASO QUITO EL DIBUJO PORQUE NO ME INTERESA
  #if (plot_boot) {
  #hist(theta_hat_star, probability = TRUE, main = "",
  #xlab = latex2exp::TeX("$\\hat{\\theta}ˆ*$"))
  #rug(theta_hat_star)
  #abline(v = ci, col = 2, lwd = 2)
  # abline(v = boot_obj$t0, col = 3, lwd = 2)
  # }
  # Return confidence intervals
  return(ci)
}
p_hat=function(x){1/(mean(x^2)/mean(x)-mean(x))}
theta_hat=function(x){mean(x)*p_hat(x)/(1-p_hat(x))}
n=0
#supongo r=10 por ejemplo, p=1/2 
r=10
p=1/2
for (i in 1:100){
x=runif(100,-3,3)
interval=boot_ci(x,theta_hat)
if (interval[1] < 3 && interval[2] > 3) {
  n <- n + 1
}
}
n

#lo ejecuté un par de veces, salió 97, 94
