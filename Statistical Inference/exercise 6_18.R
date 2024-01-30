# Default parameters
sigma_0 <- 1
alpha <- 0.10
n <- 25
mu <- 0

# Power functions, need the qnorm inside the std pnorm as per # example 6.18
omega_a <- function(sigma) {
  pchisq(sigma_0/sigma*qchisq(alpha,df=n-1,lower.tail=F),df=n-1,lower.tail=F)
}

omega_b <- function(sigma) {
  pchisq(sigma_0/sigma*qchisq(1-alpha,df=n-1,lower.tail=F),df=n-1)
}

omega_c <- function(sigma) {
  pchisq(sigma_0/sigma*qchisq(alpha/2,df=n-1,lower.tail=F),df=n-1,lower.tail=F) + 
    pchisq(sigma_0/sigma*qchisq(1-alpha/2,df=n-1,lower.tail=F),df=n-1)
}

# Make data for plot
sigma_values <- seq(-1, 1, by=0.01)
omega_a_values <- sapply(sigma_values, omega_a)
omega_b_values <- sapply(sigma_values, omega_b)
omega_c_values <- sapply(sigma_values, omega_c)

# Plotting
plot(sigma_values, omega_a_values, type="l", col="red", ylim=c(0, 1), 
     xlab=expression(sigma), ylab="Power", 
     main=expression(paste("Power functions - ", omega["a"](sigma), ", ", 
                           omega["b"](sigma), ", and ", omega["c"](sigma))),
     xaxt="n", yaxt="n")

# axis ticks and labels
axis(1, at=seq(-1, 1, by=0.2), labels=seq(-1, 1, by=0.2))
axis(2, at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), las=1)
abline(h=alpha, lty=2, col="black")  # line for alpha
abline(v=mu_0, lty=2, col="black")    # line for mu_0
abline(h=1, lty=2, col="black")  # line for 1
abline(h=0, lty=2, col="black")  # line for 0
legend("right", legend=c(expression(omega["a   "]), expression(omega["b   "]), expression(omega["c   "])),
       col=c("red", "green", "blue"), lty=1, cex=1.25)

