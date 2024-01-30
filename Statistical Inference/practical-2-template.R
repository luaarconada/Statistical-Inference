
## -------------------------------------------------------------------------- ##
## Second practical exercise Statistical Inference 2023/2024, 2023-11-03      ##
##                                                                            ##
## Remember:                                                                  ##
##                                                                            ##
## - You are allowed to **only use the class materials, R/RStudio’s help, and ##
##   your OWN materials from this academic course** (notes, your annotations, ##
##   scripts).                                                                ##
## - **NOT allowed**: any other web resources or applications (ChatGPT,       ##
##   GitHub Copilot, AI tools, Wikipedia, StackOverflow, Googling, messaging  ##
##   apps, etc.).                                                             ##
##                                                                            ##
## -------------------------------------------------------------------------- ##

## -------------------------------------------------------------------------- ##
## Keep a clear and explicit separation between the delivered tasks           ##
## Be sure your script runs before submitting it! Do a test on a fresh R      ##
## session to see if everything runs smoothly before submission               ##
## -------------------------------------------------------------------------- ##

## -------------------------------------------------------------------------- ##
## ----------------------------    Task 1    -------------------------------- ##
## -------------------------------------------------------------------------- ##

#Obtain the data from the R datasets.
data("iris")
irisdata=data.frame(iris)
#extract data of the species
species = split(irisdata, irisdata$Species)
#extract setosa data
setosa=species$setosa 
#extract versicolor data
versicolor=species$versicolor 
#extract virginica data
virginica=species$virginica 


#function for making bootstrap CI for alpha=0.01
boot_ci <- function(x, theta_hat, B = 5e3, alpha = 0.10, plot_boot=TRUE) {
  
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


#Function whose Confidence Interval we want
theta_hat=function(x){sd(x)/abs(mean(x))}


#Let's compute the confidence interval of cv for each species
#For setosa
setosa_length = setosa$Sepal.Length
CVset=boot_ci(setosa_length, theta_hat)
CVset

theta_hat(setosa_length)


#For virginica
virginica_length=virginica$Sepal.Length
CVvir=boot_ci(virginica_length, theta_hat)
CVvir

theta_hat(virginica_length)

#For versicolor
versicolor_length=versicolor$Sepal.Length
CVver=boot_ci(versicolor_length, theta_hat)
CVver

theta_hat(versicolor_length)

#b)
#We can see that in fact, the cv for each species is in the confidence interval.
#I think that what is worth highlighting is how small the confidence intervals
# are, barely 0.03 or so.

## -------------------------------------------------------------------------- ##
## ----------------------------    Task 2    -------------------------------- ##
## -------------------------------------------------------------------------- ##

# Read data
x <- read.table("diameters.txt")$V1

#Define our MLE of theta
theta_mle = function(x) {(-(mean(x)-1)+sqrt((mean(x)-1)^2+8*mean(x)))/(2*mean(x))}

#Define our Fisher Information
Inf_fish = function(theta) {2/(theta^2) - 1/((1+theta)^2)}

#Defining some parameters
alpha=0.05
n=125
z=qnorm(alpha/2, lower.tail = FALSE)


#Asymptotic confidence interval for theta
ACI=theta_mle(x)+c(-1,1)*z/sqrt(n*Inf_fish(theta_mle(x)))
ACI



## -------------------------------------------------------------------------- ##
## ----------------------------    Task 3    -------------------------------- ##
## -------------------------------------------------------------------------- ##


n=25
M=1000
mu0=0
alpha=0.1
sigma=1
mu=seq(-2, 2, l=50)

#code for w_a (alternative=less)
rel_freq1=c(1:length(mu))
for(i in 1:length(mu)){
  count=0
  for (j in 1:M){
    x=rnorm(n, mu[i], sigma)
    t=t.test(x, alternative = "less", mu=mu0)
    if(alpha>=t$p.value){
      count=count+1
    }
  }
  rel_freq1[i]=count/M
}
plot(mu, rel_freq1)
lines(mu, rel_freq1, col="orange")

#code for w_b (alternative=greater)
rel_freq2=c(1:length(mu))
for(i in 1:length(mu)){
  count=0
  for (j in 1:M){
    x=rnorm(n, mu[i], sigma)
    t=t.test(x, alternative = "greater", mu=mu0)
    if(alpha>=t$p.value){
      count=count+1
    }
  }
  rel_freq2[i]=count/M
}
plot(mu, rel_freq2)
lines(mu, rel_freq2, col="purple")

#code for w_c (alternative=two.sided)
rel_freq3=c(1:length(mu))
for(i in 1:length(mu)){
  count=0
  for (j in 1:M){
    x=rnorm(n, mu[i], sigma)
    t=t.test(x, alternative = "two.sided", mu=mu0)
    if(alpha>=t$p.value){
      count=count+1
    }
  }
  rel_freq3[i]=count/M
}
plot(mu, rel_freq3)
lines(mu, rel_freq3, col="black")

library(ggplot2)
#I create a data.frame
results <- data.frame(x = mu, rel_freq1 = rel_freq1, rel_freq2 = rel_freq2, rel_freq3 = rel_freq3)

#To create the figure with the 3 kinds of tests together
ggplot(data = results,aes(x=mu)) +
  geom_line(aes(y = rel_freq1), color = "blue", linetype = "solid") +
  plot(mu, rel_freq1, col="blue")+
  plot(mu,rel_freq2,col='green')+
  plot(mu, rel_freq3, col="red")+
  geom_line(aes(y = rel_freq2), color = "green", linetype = "dashed") +
  geom_line(aes(y = rel_freq3), color = "red", linetype = "dotted") +
  labs(x = "var", y = "Relative Rejection Frequency", title = "Relative Rejection Frequency vs. ratio of variances") +
  theme_minimal()
