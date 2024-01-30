#---------------------#
#        Ex 6.23      #
#---------------------#

lambdaa <- function(srs){exp(0.5*sum(-srs^2 + (srs - mean(srs))^2))}
lambdab <- function(srs){mean(srs)^length(srs)*exp(length(srs)*(1-mean(srs)))}
lambdac <- function(srs1, srs2){ 
  n1 <- length(srs1)
  n2 <- length(srs2)
  
  theta1 <- mean(srs1)
  theta2 <- mean(srs2)
  
  return(((1/(2*theta1))^(n1*theta1))((1/(2*theta2))^(n2*theta2))*exp(-2*n1(1-(theta1 + theta2))))
}

M <- 1000

n1 <- 10
n2 <- 50
n3 <- 100
n4 <- 500

data1a <- replicate(M, {srs <- sample(rnorm(n1))
return(lambdaa(srs))})
data2a <- replicate(M, {srs <- sample(rnorm(n2))
return(lambdaa(srs))})
data3a <- replicate(M, {srs <- sample(rnorm(n3))
return(lambdaa(srs))})
data4a <- replicate(M, {srs <- sample(rnorm(n4))
return(lambdaa(srs))})

data1b <- replicate(M, {srs <- sample(rexp(n1, rate = 1))
return(lambdab(srs))})
data2b <- replicate(M, {srs <- sample(rexp(n2, rate = 1))
return(lambdab(srs))})
data3b <- replicate(M, {srs <- sample(rexp(n3, rate = 1))
return(lambdab(srs))})
data4b <- replicate(M, {srs <- sample(rexp(n4, rate = 1))
return(lambdab(srs))})

data1c <- replicate(M, {srs1 <- sample(rpois(n1, lambda = 0.5))
srs2 <- sample(rpois(n1, lambda = 0.5))
return(lambdac(srs1, srs2))})
data2c <- replicate(M, {srs1 <- sample(rpois(n2, lambda = 0.5))
srs2 <- sample(rpois(n2, lambda = 0.5))
return(lambdac(srs1, srs2))})
data3c <- replicate(M, {srs1 <- sample(rpois(n3, lambda = 0.5))
srs2 <- sample(rpois(n3, lambda = 0.5))
return(lambdac(srs1, srs2))})
data4c <- replicate(M, {srs1 <- sample(rpois(n4, lambda = 0.5))
srs2 <- sample(rpois(n4, lambda = 0.5))
return(lambdac(srs1, srs2))})

t <- seq(0,10,length.out = 1000)

hist(-2*log(data1a), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)
hist(-2*log(data2a), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)
hist(-2*log(data3a), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)
hist(-2*log(data4a), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)

hist(-2*log(data1b), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)
hist(-2*log(data2b), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)
hist(-2*log(data3b), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)
hist(-2*log(data4b), probability = TRUE)
lines(t, dchisq(t, df = 1), col = 'red', lwd = 2)

#Revisar este, no sale :(
t <- seq(-10,30,length.out = 1000)
hist(-2*log(data1c), probability = TRUE)
lines(t, dchisq(t, df = 2), col = 'red', lwd = 2)
t <- seq(-40,40,length.out = 1000)
hist(-2*log(data2c), probability = TRUE)
lines(t, dchisq(t, df = 2), col = 'red', lwd = 2)
hist(-2*log(data3c), probability = TRUE)
lines(t, dchisq(t, df = 2), col = 'red', lwd = 2)
hist(-2*log(data4c), probability = TRUE)
lines(t, dchisq(t, df = 2), col = 'red', lwd = 2)


#---------------------#
#        Ex 6.24      #
#---------------------#

test <- function(srs1, srs2, alfa = 0.05){
  
  n1 <- length(srs1)
  n2 <- length(srs2)
  
  mu1 <- mean(srs1)
  mu2 <- mean(srs2)
  mu <- (n1*mu1 + n2*mu2)/(n1+n2)
  
  S1 <- var(srs1)
  S2 <- var(srs2)
  S <- (n1*S1 + n2*S2)/(n1+n2)
  
  term1 <- -sum((c(srs1,srs2)-mu)^2)/(2*S)
  term2 <- sum((srs1 - mu1)^2)/(2*S1)
  term3 <- sum((srs2 - mu2)^2)/(2*S2)
  
  lambda <- S1^(n1/2)*S2^(n2/2)/(S^((n1+n2)/2))*exp(term1 + term2 + term3)
  k <- exp(-0.5*qchisq(1-alfa, df = 1))
  
  pass <- lambda <= k
  pval <- pchisq(-2*log(lambda), df = 1, lower.tail = FALSE)
  
  return(c(pass, pval, lambda, k))
}

srs1 <- rnorm(100, mean = 10, sd = 10)
srs2 <- rnorm(150, mean = 10, sd = 10)
srs3 <- rnorm(150, mean = 15, sd = 10)
srs4 <- rnorm(150, mean = 10, sd = 15)
srs5 <- rnorm(150, mean = 15, sd = 15)

test1 <- test(srs1, srs2)
test2 <- test(srs1, srs3)
test3 <- test(srs1, srs4)
test4 <- test(srs1, srs5)

test5 <- test(iris$Sepal.Width[iris$Species == "versicolor"], iris$Sepal.Width[iris$Species == "virginica"])

#---------------------#
#        Ex 6.25      #
#---------------------#

test <- function(srs, alfa = 0.05){
  n <- length(srs[,1])
  mu <- c(mean(srs[,1]), mean(srs[,2]))
  sigma <- t((srs - mu))%*%(srs - mu)
  sigma <- sigma/n
  cov <- sigma[1,2]/sqrt(sigma[1,1]*sigma[2,2])
  lambda <- (1 - cov^2)^(-n/2) * exp(-cov/(1-cov^2)*sum((srs[,1] - mu[1])/sqrt(sigma[1,1])*(srs[,2] - mu[2])/sqrt(sigma[2,2])))
  k <- exp(-0.5*qchisq(1-alfa, df = 1))
  pass <- lambda <= k
  pval <- pchisq(-2*log(lambda), df = 1, lower.tail = FALSE)
  return(c(pass, pval, lambda, k))
}

ex <- c(10,12)
sigma1 <- matrix(c(10,0,0,10), byrow = FALSE, ncol = 2)
sigma2 <- matrix(c(10,5,5, 10), byrow = FALSE, ncol = 2)

sample1 <- mvtnorm::rmvnorm(100, mean = ex, sigma = sigma1)
sample2 <- mvtnorm::rmvnorm(100, mean = ex, sigma = sigma2)

test(sample1)
test(sample2)

sample3 <- matrix(data = c(iris$SepalLength, iris$Sepal.Width), ncol = 2)

test(sample3)