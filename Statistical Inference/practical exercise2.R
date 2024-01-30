data=read.table("C:/Users/lopez/OneDrive/Escritorio/Statistical Inference/diameters.txt", quote="\"", comment.char="")

#1############################################################################3
#a)
data("iris")
irisdata=force(iris)
#extract the species
species = split(irisdata, irisdata$Species)
#extract setosa
setosa=species$setosa 
#extract versicolor
versicolor=species$versicolor 
#extract virginica
virginica=species$virginica 

library(latex2exp) 
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
theta_hat=function(x){sd(x)/abs(mean(x))}

#For setosa
CVset=boot_ci((setosa$Sepal.Length), theta_hat)
CVset

#For virginica
CVvir=boot_ci((virginica$Sepal.Length), theta_hat)
CVvir

#For versicolor
CVver=boot_ci((versicolor$Sepal.Length), theta_hat)
CVver

#2###########################################
#b)
#MLE for theta
theta_hatmle=function(x){(-(mean(x)-1)+sqrt((mean(x)-1)^2+8*mean(x)))/(2*mean(x))} 
#Fisher information
Infthet=function(theta){(2/theta^2)-(1/(1+theta)^2)}
#Taking the sample from the data
sample=data$V1
alpha=0.05
n=length(sample)

#Asymptotic confidence interval for theta
ACI=theta_hatmle(sample)+c(-1,1)*qnorm(alpha/2, lower.tail = FALSE)/sqrt(n*Infthet(theta_hatmle(sample)))
ACI

#3#################################################################3
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
lines(mu, rel_freq3, col="black")

