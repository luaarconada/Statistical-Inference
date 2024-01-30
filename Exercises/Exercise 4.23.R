library(ggplot2)   #ggplot2 for plotting
library(pracma)    #pracma for the derivative of the digamma function
library(latex2exp) #latex2exp to write in latex

N <- 1000  #Number of simulations
n <- 1000  #Size of the samples of each simulation

#We set the value of the parameters of the beta distribution
alpha <- 0.5
theta <- 100

#instance function: this function simulates the process of obtaining a srs of size n
#drawn from a beta distribution with the specified parameters and calculating the 
#desired values
instance <- function(){
  #We draw a random sample from a beta distribution
  x <- rbeta(n, alpha, theta)
  
  #We implement the equation whose root is the MLE estimator
  MLE_eq <- function(theta, sample = x){
    n*( digamma(theta + alpha) - digamma(theta)) + sum(log(1 - sample))  
  }
  
  #We calculate the root of the MLE equation, obtaining the MLE estimator
  theta_MLE <- uniroot(MLE_eq, c(0.0001, 1000))[[1]]
  
  #We calculate the required Fisher informations and its parameters
  Fisher_true_theta <- psi(1, theta) - psi(1, alpha + theta)
  Fisher_true_MLE <- psi(1, theta_MLE) - psi(1, alpha + theta_MLE)
  Fisher_aprox_theta <- sum((digamma(theta + alpha) - digamma(theta) + log(1 - x))^2)/n
  Fisher_aprox_MLE <- sum((digamma(theta_MLE + alpha) - digamma(theta_MLE) + log(1 - x))^2)/n
  
  #We calculate the desired value whose distributions we are interested in
  value_true_theta <- sqrt(n*Fisher_true_theta)*(theta_MLE - theta)
  value_true_MLE <- sqrt(n*Fisher_true_MLE)*(theta_MLE - theta)
  value_aprox_theta <- sqrt(n*Fisher_aprox_theta)*(theta_MLE - theta)
  value_aprox_MLE <- sqrt(n*Fisher_aprox_MLE)*(theta_MLE - theta)
  
  #Said values are the result we are looking for
  return(c(value_true_theta, value_true_MLE, value_aprox_theta, value_aprox_MLE))
}

set.seed(666)  #We set a seed to ensure the reproducibility of the results

#We simulate N samples and calculate the desired value for each of them
results <- replicate(N, instance())

#We store said values independently to keep our code readable
dist_true_theta <- results[1,]
dist_true_MLE <- results[2,]
dist_aprox_theta <- results[3,]
dist_aprox_MLE <- results[4,]

#This is the interval in which we will plot the density of the normal distribution
interval <- seq(-4, 4, length.out = 1000)  

#We prepare the datasets with the values of the rvs and the interval
df1 <- data.frame(x = dist_true_theta, int = interval)
df2 <- data.frame(x = dist_true_MLE, int = interval)
df3 <- data.frame(x = dist_aprox_theta, int = interval)
df4 <- data.frame(x = dist_aprox_MLE, int = interval)

#We are now ready to represent de densities of the desired variables
fig1 <- ggplot(data = df1) +
  geom_histogram(aes(x = x, y = ..density..), binwidth = 0.2, col = "black", fill = '#F05F5F', alpha = 0.5) +
  stat_function(fun = dnorm, aes(x = int), lwd = 1, col = '#F50000') +
  xlab('Value') +
  ylab('Density') +
  ggtitle(TeX(r'(Density of $\sqrt{n I(\theta)}(\hat{\theta}_{MLE} - \theta)$)')) +
  theme_classic()

fig2 <- ggplot(data = df2) +
  geom_histogram(aes(x = x, y = ..density..), binwidth = 0.2, col = 'black', fill = '#F05F5F', alpha = 0.5) +
  stat_function(fun = dnorm, aes(x = int), lwd = 1, col = '#F50000') +
  xlab('Value') +
  ylab('Density') +
  ggtitle(TeX(r'(Density of $\sqrt{n \hat{I}(\theta)}(\hat{\theta}_{MLE} - \theta)$)')) +
  theme_classic()

fig3 <- ggplot(data = df3) +
  geom_histogram(aes(x = x, y = ..density..), binwidth = 0.2, col = 'black', fill = '#F05F5F', alpha = 0.5) +
  stat_function(fun = dnorm, aes(x = int), lwd = 1, col = '#F50000') +
  xlab('Value') +
  ylab('Density') +
  ggtitle(TeX(r'(Density of $\sqrt{n I(\hat{\theta}_{MLE})}(\hat{\theta}_{MLE} - \theta)$)')) +
  theme_classic()

fig4 <- ggplot(data = df4) +
  geom_histogram(aes(x = x, y = ..density..), binwidth = 0.2, col = 'black', fill = '#F05F5F', alpha = 0.5) +
  stat_function(fun = dnorm, aes(x = int), lwd = 1, col = '#F50000') +
  xlab('Value') +
  ylab('Density') +
  ggtitle(TeX(r'(Density of $\sqrt{n \hat{I}(\hat{\theta}_{MLE})}(\hat{\theta}_{MLE} - \theta)$)')) +
  theme_classic()

#We can now visualize our result
fig1
fig2
fig3
fig4
