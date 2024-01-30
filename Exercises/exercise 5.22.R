#exercise 5.22
# Computation of percentile bootstrap confidence intervals. It requires a
# sample x and an estimator theta_hat() (must be a function!).
boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.01, plot_boot = TRUE) {
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
#before the voting day
x=rbinom(704,1,0.535) #genero 704 variables bernouilli 
#ESTO NO ESTÁ BIEN. NO ES LA MUESTRA QUE YO HE RECIBIDO
#PARA LA QUE HE RECIBIDO, TENDRÉ QUE HACER EL NUMERO DE LOS QUE VOTAN 
#COMO PROBABILIDAD POR NUMERO DE VOTANTES, Y REDONDEAR PARA QUE SEA UN ENTERO 
theta_hat=function(x){mean(x)}
boot_ci(x,theta_hat)
#at the voting day
x=rbinom(167,1,0.461) #genero 704 variables bernouilli 
theta_hat=function(x){mean(x)}
boot_ci(x,theta_hat)
#LO HAGO BIEN AQUÍ ABAJO: 
#before voting day
#mi muestra está formada por 
#personas que votan A
A_literal=0.535*704
#A=1, B=0
A_entero=round(A_literal)
muestra=c(rep(1,A_literal),rep(0,704-A_literal)) #el orden de los datos nos da igual
#un estimador de p_A cuando tengo X1...Xn bernuilli, es X barra
theta_hat=function(x){mean(x)
}
boot_ci(muestra,theta_hat)
#at voting day, hacemos lo mismo
n_2 <- 167
n_A_2 <- 77

x_2 <- c(rep(1, n_A_2), rep(0, n_2 - n_A_2))
boot_ci(x = x_2, theta_hat = function(x) mean(x), B = 1e4, alpha = 0.01)