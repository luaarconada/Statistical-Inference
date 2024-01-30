# Default arameters
mu_0 <- 0
alpha <- 0.10
n <- 25
sigma <- 1

# Power functions, need the qnorm inside the std pnorm as per # example 6.18
omega_a <- function(mu) {
  pnorm(qnorm(alpha) + (mu - mu_0) / (sigma/sqrt(n)))
}

omega_b <- function(mu) {
  pnorm(qnorm(alpha) + (mu_0 - mu) / (sigma/sqrt(n)))
}

omega_c <- function(mu) {
  pnorm(qnorm(alpha / 2) + (mu_0 - mu) / (sigma/sqrt(n))) + 
    pnorm(qnorm(alpha / 2) + (mu - mu_0) / (sigma/sqrt(n)))
}

# Make data for plot
mu_values <- seq(-1, 1, by=0.01)
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

# Plotting
plot(mu_values, omega_a_values, type="l", col="red", ylim=c(0, 1), 
     xlab=expression(mu), ylab="Power", 
     main=expression(paste("Power functions - ", omega["a"](mu), ", ", 
                           omega["b"](mu), ", and ", omega["c"](mu))),
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


#===========================================================================

lines(mu_values, omega_b_values, col="green")
lines(mu_values, omega_c_values, col="blue")




#===========================================================================
# PART A
# n = 5
n <- 5

# values for plot
mu_values <- seq(-2, 2, by=0.02)
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

# Plotting
plot(mu_values, omega_a_values, type="l", col="red", ylim=c(0, 1), 
     xlab=expression(mu), ylab="Power", 
     main=expression(paste("Power functions - ", omega["a"](mu), ", ", 
                           omega["b"](mu), ", and ", omega["c"](mu), " - n = 5, 50, 200")),
     xaxt="n", yaxt="n")

abline(h=alpha, lty=2, col="black")  # line for alpha
abline(v=mu_0, lty=2, col="black")    # line for mu_0
abline(h=1, lty=2, col="black")  # line for 1
abline(h=0, lty=2, col="black")  # line for 0
legend("right", legend=c("n = 5", "n = 50", "n = 200"),
       col=c("red", "green", "blue"), lty=1, cex=1.15)

# axis ticks and labels
axis(1, at=seq(-2, 2, by=0.4), labels=seq(-2, 2, by=0.4))
axis(2, at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), las=1)

lines(mu_values, omega_b_values, col="red")
lines(mu_values, omega_c_values, col="red")

#===========================================================================


# n = 50
n <- 50

# values for plot
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

lines(mu_values, omega_a_values, col="green")
lines(mu_values, omega_b_values, col="green")
lines(mu_values, omega_c_values, col="green")

#===========================================================================

# n = 200
n <- 200

# values for plot
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

lines(mu_values, omega_a_values, col="blue")
lines(mu_values, omega_b_values, col="blue")
lines(mu_values, omega_c_values, col="blue")

#===========================================================================
# PART B
# sigma = 0.1
n <- 25
sigma <- 0.1

# values for plot
mu_values <- seq(-3, 3, by=0.02)
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

# Plotting
plot(mu_values, omega_a_values, type="l", col="red", ylim=c(0, 1), 
     xlab=expression(mu), ylab="Power", 
     main=expression(paste("Power functions - ", omega["a"](mu), ", ", 
                           omega["b"](mu), ", and ", omega["c"](mu), " - ", sigma, " = 0.1, 2, 10")),
     xaxt="n", yaxt="n")
lines(mu_values, omega_b_values, col="red")
lines(mu_values, omega_c_values, col="red")
abline(h=alpha, lty=2, col="black")  # line for alpha
abline(v=mu_0, lty=2, col="black")    # line for mu_0
abline(h=1, lty=2, col="black")  # line for 1
abline(h=0, lty=2, col="black")  # line for 0
legend("topright", inset=c(0, 0.05), 
       legend=c(expression(paste(sigma, " = 0.1 ")), expression(paste(sigma, " = 2 ")), 
                expression(paste(sigma, " = 10"))),
       col=c("red", "green", "blue"), lty=1, cex=1)

# axis ticks and labels
axis(1, at=seq(-3, 3, by=0.5), labels=seq(-3, 3, by=0.5))
axis(2, at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), las=1)

#===========================================================================

# sigma = 2
sigma <- 2

# values for plot
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

lines(mu_values, omega_a_values, col="green")
lines(mu_values, omega_b_values, col="green")
lines(mu_values, omega_c_values, col="green")

#===========================================================================

# sigma = 10
sigma <- 10

# values for plot
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

lines(mu_values, omega_a_values, col="blue")
lines(mu_values, omega_b_values, col="blue")
lines(mu_values, omega_c_values, col="blue")

#===========================================================================
# PART C
# mu_0 = -1, 5, 10
mu_0 = -1
sigma <- 1

# values for plot
mu_values <- seq(-2, 12, by=0.02)
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

# Plotting
plot(mu_values, omega_a_values, type="l", col="red", ylim=c(0, 1), 
     xlab=expression(mu), ylab="Power", 
     main=expression(paste("Power functions - ", omega["a"](mu), ", ", 
                           omega["b"](mu), ", and ", omega["c"](mu), " - ", mu["0"], " = -1, 5, 10")),
     xaxt="n", yaxt="n")
lines(mu_values, omega_b_values, col="green")
lines(mu_values, omega_c_values, col="blue")
abline(h=alpha, lty=2, col="black")  # line for alpha
abline(v=mu_0, lty=2, col="black")    # line for mu_0
abline(h=1, lty=2, col="black")  # line for 1
abline(h=0, lty=2, col="black")  # line for 0
legend("right", legend=c(expression(omega["a   "]), expression(omega["b   "]), expression(omega["c   "])),
       col=c("red", "green", "blue"), lty=1, cex=0.75)

# axis ticks and labels
axis(1, at=seq(-2, 12, by=1), labels=seq(-2, 12, by=1))
axis(2, at=seq(0, 1, by=0.1), labels=seq(0, 1, by=0.1), las=1)

#===========================================================================

# mu_0 = 5
mu_0 = 5

# values for plot
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

lines(mu_values, omega_a_values, col="red")
lines(mu_values, omega_b_values, col="green")
lines(mu_values, omega_c_values, col="blue")

abline(v=mu_0, lty=2, col="black")    # line for mu_0

#===========================================================================
# mu_0 = 5
mu_0 = 10

# values for plot
omega_a_values <- sapply(mu_values, omega_a)
omega_b_values <- sapply(mu_values, omega_b)
omega_c_values <- sapply(mu_values, omega_c)

lines(mu_values, omega_a_values, col="red")
lines(mu_values, omega_b_values, col="green")
lines(mu_values, omega_c_values, col="blue")

abline(v=mu_0, lty=2, col="black")    # line for mu_0

