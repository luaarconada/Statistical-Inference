M=1000
n_1=25
n_2=50
mu=0
alpha=0.1
theta0=1
theta=seq(0.1,10,l=25)
sigma1=1

#code for w_a (alternative=less)
rel_freq1=c(1:length(theta))
for(i in 1:length(theta)){
  count=0
  for (j in 1:M){
    x1=rnorm(n_1, mean=mu, sd=sigma1)
    x2=rnorm(n_2, mean=mu, sd=sigma1/sqrt(theta[i]))
    chi=var.test(x1,x2,alternative='greater',ratio=1)
    if(alpha>=chi$p.value){
      count=count+1
    }
  }
  rel_freq1[i]=count/M
}
plot(theta, rel_freq1)
lines(theta, rel_freq1, col="orange")

#code for w_b (alternative=greater)
rel_freq2=c(1:length(theta))
for(i in 1:length(theta)){
  count=0
  for (j in 1:M){
    x1=rnorm(n_1, mean=mu, sd=sigma1)
    x2=rnorm(n_2, mean=mu, sd=sigma1/sqrt(theta[i]))
    chi=var.test(x1,x2,alternative='less',ratio=1)
    if(alpha>=chi$p.value){
      count=count+1
    }
  }
  rel_freq2[i]=count/M
}
plot(theta, rel_freq2)
lines(theta, rel_freq2, col="purple")

#code for w_c (alternative=two.sided)
rel_freq3=c(1:length(theta))
for(i in 1:length(theta)){
  count=0
  for (j in 1:M){
    x1=rnorm(n_1, mean=mu, sd=sigma1)
    x2=rnorm(n_2, mean=mu, sd=sigma1/sqrt(theta[i]))
    chi=var.test(x1,x2,alternative='two.sided',ratio=1)
    if(alpha>=chi$p.value){
      count=count+1
    }
  }
  rel_freq3[i]=count/M
}
plot(theta, rel_freq3)
lines(theta, rel_freq3, col="black")

