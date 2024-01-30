###################################  
######### EJEMPLOS HOJA 5 #########
###################################

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

## Example 2

sample_copy = c(916, 892, 895, 904, 913, 916, 895, 885)
n = length(sample_copy)
desv_t = 12
confidence = 0.05
sample_mean = mean(sample_copy)
up= sample_mean + qnorm(0.975)*(desv_t/sqrt(n))
down= sample_mean - qnorm(0.975)*(desv_t/sqrt(n))

c(down, up)
# Example 3

a = sample_copy - sample_mean
b = a^2
c = sum(b) / 7
c

up= sample_mean + qt(0.975,7)*(c/sqrt(n))
down= sample_mean - qt(0.975,7)*(c/sqrt(n))
c(down, up)
c^2


# Example 4
sample_copy = c(4.1, 5.2, 10.2)
n = length(sample_copy)
a = sample_copy - mean(sample_copy)
b = a^2
c = sum(b) / (length(sample_copy) -1)

up= c*(n-1)/ qchisq(0.95,length(sample_copy)-1)
down= c*(n-1)/ qchisq(0.05,length(sample_copy)-1)
c(down, up)
c^2

# Example 5

x1 = c(32,37,35,28,41,44,35,31,34)
x2 = c(35,31,29,25,34,40,27,32,31)

a = mean(x1) - mean(x2)

up= a + qnorm(0.975)*(sqrt(22)*sqrt(1/length(x1) + 1/length(x1)))
down= a - qnorm(0.975)*(sqrt(22)*sqrt(1/length(x1) + 1/length(x1)))
c(down, up)

# Example 6
x1 = c(32,37,35,28,41,44,35,31,34)
x2 = c(35,31,29,25,34,40,27,32,31)

a = mean(x1) - mean(x2)

cuasi1 = sum((x1 -mean(x1))^2)/(length(x1) - 1)
cuasi2 = sum((x2 -mean(x2))^2)/(length(x2) - 1)

aprox = ((length(x1)-1) * cuasi1 + (length(x2)-1) * cuasi2)/(2*length(x1) -2)
aprox

up= a + qt(0.975, length(x1) +length(x1) -1)*(sqrt(aprox/length(x1) + aprox/length(x1)))
down= a - qt(0.975, length(x1) +length(x1) -1)*(sqrt(aprox/length(x1) + aprox/length(x1)))
c(down, up)

# Example 7
var1 = 52
var2 = 71

c((var1/var2)/qf(0.975, 10,13), (var1/var2)/qf(0.025, 10,13))

# Example 8
n = 64
quasivar = 256
media = 33

up = media + (sqrt(quasivar)/sqrt(n))*qnorm(0.95)
down = media - (sqrt(quasivar)/sqrt(n))*qnorm(0.95)
c(up, down)

# Example 10
n1 = 50
p1 = 0.24
var1 = p1*(1-p1)

n2 = 60
p2 = 0.2
var2 = p2*(1-p2)

diferencia = p1-p2

up = diferencia + qnorm(0.99)*sqrt(var1/n1 + var2/n2)
down = diferencia - qnorm(0.99)*sqrt(var1/n1 + var2/n2)

c(up, down)

# Example 11
set.seed(123456)
lambda_mle = 1 / mean(rexp(100, rate = 2))

up = lambda_mle + lambda_mle*qnorm(0.025, lower.tail = F)/sqrt(100)
down = lambda_mle - lambda_mle*qnorm(0.025, lower.tail = F)/sqrt(100)
c(up, down)

# Example 12
#boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.05, plot_boot = TRUE)
n = 200
x = rexp(n, rate = 2)

lambda_hat = function (x) 1 / mean(x)

boot_ci(x = x, theta_hat = lambda_hat, B = 5e3)

# Example 13
sample_copy = c(916, 892, 895, 904, 913, 916, 895, 885)
ex_hat= function(sample_copy) mean(sample_copy)
boot_ci(x = sample_copy, theta_hat = ex_hat, B = 5e3)
boot_ci(x = sample_copy, theta_hat = mean, B = 5e3)

###################################  
######## EJERCICIOS HOJA 5 ########
###################################

## 1
desv = 16.8
desv = 20
(qnorm(0.025, lower.tail = F) * desv )^2
(qnorm(0.05, lower.tail = F) * desv )^2

## 2
p = 6/120
n = 120
intervalo = p + c(-1,1) * qnorm(0.001/2)*sqrt((p*(1-p))/n)
intervalo

## 3 No R

## 4 
sample_a = c(6.2, 7.3, 5.5, 6.7, 9.0, 7.1, 5.0, 6.3, 7.2, 7.5, 8.0, 7.9, 6.5, 6.1, 7.0)
intervalo = mean(sample_a) + c(-1,1) * qnorm(0.05/2)*sqrt(var(sample_a) / length(sample_a))
intervalo

up = (length(sample_a) - 1) * sqrt(var(sample_a))/qchisq(0.025, 14)
down = (length(sample_a) - 1) * sqrt(var(sample_a))/qchisq(1- 0.025, 14)
c(down,up)

n = 50
media = 6.5
quasides = 1.3
up = media + qt(0.025, 49)*quasi/sqrt(n)
down = media - qt(0.025, 49)*quasi/sqrt(n)
c(down,up)

## 5
n = 20
cuasi_5 = 2.5
up = (n - 1) * cuasi_5^2/qchisq(0.025, n-1)
down = (n - 1) * cuasi_5^2/qchisq(1- 0.025, n-1)
c(down,up)

## 6 

s6 = c(2072, 2726, 2254, 3029, 2283, 2673, 2401, 2463, 2416, 2909, 2385, 2055, 2139, 2553, 2453, 2621, 2808, 3583, 2662, 2732, 1996, 2164, 2036, 2915, 2507, 3245, 2337, 2672, 3089, 3012, 2725, 2467, 2439, 2692, 1793, 3845, 2523, 2585, 1647, 2072, 1988, 2767, 3679, 2280, 2642, 3112, 2512, 2435, 2820, 2142, 3197, 3103, 2130, 3213, 2464, 2243, 2958, 2529, 2160, 2484)
intervalo = mean(s6) + c(-1,1) * qnorm(0.05/2)*sqrt(var(s6) / length(s6))
intervalo

## 7
s7 = 5.4
d7 = 3.1
n = 500
intervalo = s7 + c(-1,1) * qnorm(0.05/2)*d7/sqrt(n)
intervalo

## 8
p = 0.629
n = 6100
intervalo = p + c(-1,1) * qnorm(0.05/2)*sqrt((p*(1-p))/n)
intervalo

## 9 Ni idea

## 10 Ni idea

## 11 No importante ver en otro momento

## 12 
x1 = 43.71
x2 = 39.63
desv1 = 5.88
desv2 = 7.68
n1 = 9
n2 = 7

s = ((n1-1)*desv1 + (n2-1)*desv2)/(n1+n2-2)
intervalo= (x1-x2) +c(-1,1)*qt(0.975,n1+n2-2)*sqrt(s)*(sqrt(1/n1 + 1/n2))
intervalo

## 13
x1 = 3
x2 = 2.8
desv1 = 2.5
desv2 = 2.7
n1 = 20
n2 = 30

down = (desv1/desv2)/qf(0.025,n1-1,n2-2)
up = (desv1/desv2)/qf(0.975,n1-1,n2-2)
c(up,down)

s = ((n1-1)*desv1 + (n2-1)*desv2)/(n1+n2-2)
intervalo= (x1-x2) +c(-1,1)*qt(0.975,n1+n2-2)*sqrt(s)*(sqrt(1/n1 + 1/n2))
intervalo

## 14 No idea
## 15 later
## 16 Not important
## 17

s17 = c(1,2,2,1,0,1,0,0,1,1,0,0,1,1,2,0,1,1,2,1, 0, 0, 0, 0, 2, 0, 2, 0, 0, 1)
n = 30
# Por slutsky se puede asimilar a una normal? O TCL 

intervalo = mean(s17) + c(-1,1) * qnorm(0.05/2)*sqrt(var(s17))/sqrt(n)
intervalo


## 18 teoria
## 19 teoria
## 20 ni idea


#boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.05, plot_boot = TRUE)ç

## 21 
sample = c(6.2, 7.3, 5.5, 6.7, 9.0, 7.1, 5.0, 6.3, 7.2, 7.5, 8.0, 7.9, 6.5, 6.1, 7.0)
boot_ci(sample, theta_hat = mean, alpha = 0.05,B = 10e3)
sample = c(2072, 2726, 2254, 3029, 2283, 2673, 2401, 2463, 2416, 2909, 2385, 2055, 2139, 2553, 2453, 2621, 2808, 3583, 2662, 2732, 1996, 2164, 2036, 2915, 2507, 3245, 2337, 2672, 3089, 3012, 2725, 2467, 2439, 2692, 1793, 3845, 2523, 2585, 1647, 2072, 1988, 2767, 3679, 2280, 2642, 3112, 2512, 2435, 2820, 2142, 3197, 3103, 2130, 3213, 2464, 2243, 2958, 2529, 2160, 2484)
boot_ci(sample, theta_hat = mean, alpha = 0.05,B = 10e3)

## 22
aA0 = rep(1, round(.535* 704))
aA =c(aA0, rep(0, 704 - length(aA0)))

boot_ci(aA, theta_hat = mean, alpha = 0.01,B = 10e3)


bB =c(rep(1, 77), rep(0, 90))
boot_ci(bB, theta_hat = mean, alpha = 0.01,B = 10e3)

## 23

set.seed(2)
teta = 20
random_sample = runif(100, min= -teta, max = teta)

mean(random_sample)

boot_ci(random_sample, theta_hat = mean, alpha = 0.04,B = 10e3)


## 24
suc_trials = 10
p_suc_trial =0.05
taman = 1000
random_data <- rnbinom(taman, size = suc_trials, mu = p_suc_trial)

esperanza_mm = function(x) sum(x) / var(x)


boot_ci(random_data, theta_hat = esperanza_mm, alpha = 0.05,B = 10e3)

esperanza = (p_suc_trial*taman*(1-p_suc_trial))/p_suc_trial
esperanza

