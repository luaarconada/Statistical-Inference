#---------------------#
#        Ex 6.1       #
#---------------------#

alpha <- 0.05
srs <- c(785, 805, 790, 793, 802)

s <- sd(srs)
m <- mean(srs)

int <- c(800 + qt(0.025, 4)*s/sqrt(4), 800 + qt(0.975, 4)*s/sqrt(4))


#---------------------#
#        Ex 6.2       #
#---------------------#

x1 <- 0.041
x2 <- 0.026

s1 <- sqrt(0.021)
s2 <- sqrt(0.006)

n1 <- 10
n2 <- 13

alpha <- 0.05
s <- sqrt(((n1-1)*s1^2 + (n2-1)*s2^2)/(n1+n2-2))
c <- qt(1-alpha, n1+n2-1)*s*sqrt(1/n1 + 1/n2)

x1 - x2
c

cb <- 0.01 + c
cb

pval <- 2*pt((x1 - x2 -0.01)/(s*sqrt(1/n1 + 1/n2)), n1 + n2 - 2, lower.tail = FALSE)
pval

#---------------------#
#        Ex 6.3       #
#---------------------#

c <- 33000 + qnorm(0.01)*6250/sqrt(40)


#---------------------#
#        Ex 6.4       #
#---------------------#

srsa <- c(20708, 21630, 15670, 17764, 19308, 19053, 19196, 18341, 19002, 18459, 19577, 19603, 16950, 17254, 17439, 18507, 18421, 16148, 21317, 18076, 21774, 18920, 16742, 15120, 15120, 17610, 19122, 19956, 19876, 17588, 17309, 19843, 16826, 17088, 17740, 20648, 16064, 19990, 15924, 17079)
srsb <- c(35261, 37821, 32167, 41367, 42732, 49535, 38236, 49175, 46720, 34275, 39510, 37475, 42905, 46869, 43089, 30767, 44708, 39449, 40953, 42604, 41246, 39125, 48394, 37166, 44107, 41153, 42399, 45076, 38372, 37798, 36956, 44789, 34909, 44991, 35470)
srs <- c(srsa, srsb)

mw <- 15120
mp <- 42823.34
ap <- 18305.84

alpha <- 0.05

ma <- mean(srsa)
mb <- mean(srsb)
m <- mean(srs)

sa <- sd(srsa)
sb <- sd(srsb)
s <- sd(srs)

na <- length(srsa)
nb <- length(srsb)
n <- length(srs)

za1 <- ap + qnorm(1 - alpha)*sa/sqrt(na)
za2 <- mp + qnorm(1 - alpha)*sa/sqrt(na)
zb1 <- ap + qnorm(1 - alpha)*sb/sqrt(nb)
zb1s <- ap + qnorm(alpha)*sb/sqrt(nb)
zb2 <- mp + qnorm(1 - alpha)*sb/sqrt(nb)
z1 <- ap + qnorm(1 - alpha)*s/sqrt(n)
z1s <- ap + qnorm(alpha)*s/sqrt(n)
z2 <- mp + qnorm(1 - alpha)*s/sqrt(n)

#---------------------#
#        Ex 6.5       #
#---------------------#

n <- 17
s <- 2^2
c2 <- qchisq(0.975, n-1)*s/(n-1)
c1 <- qchisq(0.025, n-1)*s/(n-1)
pval <- 2*min(pchisq(6.1/s*(n-1), n-1, lower.tail = FALSE), pchisq(6.1/s*(n-1), n-1))

#---------------------#
#        Ex 6.6       #
#---------------------#

na <- 20
nb <- 25

alpha <- 0.05

srsa <- c(16.3, 16.6, 14.8, 14, 16.4, 10.9, 16.4, 14.4, 14.4, 12.9, 14.7, 19.1, 15.8)
srsb <- c(16.9, 17.2, 14.7, 13.6, 16.9, 9.3, 17, 14.1, 14.1, 12.1, 14.6, 20.7, 16.2, 16.6, 12.6, 17.6, 12.2, 14)

s1 <- var(srsa)
s2 <- var(srsb)

k1 <- qf(1-alpha, na -1, nb - 1)
k2 <- qf(alpha, na -1, nb - 1)

pval <- 2*min(pf(s1/s2, na-1, nb-1, lower.tail = TRUE), pf(s1/s2, na-1, nb-1, lower.tail = FALSE))

#---------------------#
#        Ex 6.7       #
#---------------------#

nm <- 42
nf <-45

xm <- 3.6
xf <- 3.8

sm <- 0.18
sf <- 0.14
s <- sqrt(((nm-1)*sm + (nf-1)*sf)/(nm+nf-2))

k1 <- qt(0.975, nm+nf-2)*s*sqrt(1/nm + 1/nf)
k2 <- qt(0.025, nm+nf-2)*s*sqrt(1/nm + 1/nf)


#---------------------#
#        Ex 6.8       #
#---------------------#

X1 <- 47.2/100
X2 <- 48.7/100

n1 <- 4030
n2 <- 4104

S1 <- X1*(1-X1)*n1/(n1-1)
S2 <- X2*(1-X2)*n2/(n2-1)

S <- ((n1-1)*S1+(n2-1)*S2)/(n1+n2-2)

inst <- abs(X1 - X2)/(S*sqrt(1/n1 + 1/n2))
pval <- 2*pt(inst, n1+n2-2, lower.tail = FALSE)

#---------------------#
#        Ex 6.9       #
#---------------------#

pval_eq_prop <- function(M, p, n){
  M <- M/100
  p <- p/100
  
  S <- M*(1-M)*n/(n-1)
  
  inst <- abs((M-p)/(S/sqrt(n)))
  pval <- 2*pnorm(inst, lower.tail = FALSE)
  return(pval)
}

p1 <- pval_eq_prop(31.20, 28.25, 3585)
p2 <- pval_eq_prop(20.30, 20.99, 3585)
p3 <- pval_eq_prop(6.60, 15.21, 3585)
p4 <- pval_eq_prop(13.30, 12.97, 3585)
p5 <- pval_eq_prop(6.10, 6.86, 3585)

pval_MAYOR <- function(M, p, n){
  M <- M/100
  p <- p/100
  
  S <- M*(1-M)*n/(n-1)
  
  inst <- (M-p)/(S/sqrt(n))
  pval <- pnorm(inst, lower.tail = FALSE)
  return(pval)
}

pval_MENOR <- function(M, p, n){
  M <- M/100
  p <- p/100
  
  S <- M*(1-M)*n/(n-1)
  
  inst <- (M-p)/(S/sqrt(n))
  pval <- pnorm(inst)
  return(pval)
}

p1M <- pval_MAYOR(31.20, 28.25, 3585)
p2M <- pval_MAYOR(20.30, 20.99, 3585)
p3M <- pval_MAYOR(6.60, 15.21, 3585)
p4M <- pval_MAYOR(13.30, 12.97, 3585)
p5M <- pval_MAYOR(6.10, 6.86, 3585)

p1m <- pval_MENOR(31.20, 28.25, 3585)
p2m <- pval_MENOR(20.30, 20.99, 3585)
p3m <- pval_MENOR(6.60, 15.21, 3585)
p4m <- pval_MENOR(13.30, 12.97, 3585)
p5m <- pval_MENOR(6.10, 6.86, 3585)

c(p1M, p2M, p3M, p4M, p5M)
c(p1m, p2m, p3m, p4m, p5m)

#---------------------#
#        Ex 6.10      #
#---------------------#

nb <- 1736
na <- 21239

db <- 7
da <- 191

propb <- db/nb
propa <- da/na

Sa <- propa*(1-propa)*na/(na-1)
Sb <- propb*(1-propb)*nb/(nb-1)
S <- ((na-1)*Sa + (nb-1)*Sb)/(na+nb-2)
  
pval <- pt((propb - propa)/(S*sqrt(1/na + 1/nb)), na+nb-2, lower.tail = FALSE)

#---------------------#
#        Ex 6.11      #
#---------------------#

srs1 <- c(38.83, 40.67, 41.68, 37.42, 40.86, 40.96, 39.62, 39.65, 39.63, 39.23, 39.74, 39.09, 39.37, 40.41, 41.52, 40.19)
srs2 <- c(42.76, 43.93, 44.08, 41.71, 44.27, 43.73, 40.12, 41.6, 43.22, 44.83, 41.69, 44.16, 45.42, 44.7, 42.04, 44.21)

n1 <- length(srs1)
n2 <- length(srs2)

S1 <- var(srs1)
S2 <- var(srs2)

pval <- 2*min(pf(S1/S2, n1-1, n2-2, lower.tail = FALSE), pf(S1/S2, n1-1, n2-2))
#---------------------#
#        Ex 6.12      #
#---------------------#

S1 <- 0.04
S2 <- 0.03

pvalRT <- pf(S1/S2, 9, 9)
pvalLT <- pf(S1/S2, 9, 9, lower.tail = FALSE)
pval <- 2*min(pvalRT, pvalLT)

#---------------------#
#        Ex 6.13      #
#---------------------#

muestra = c(1.66, 0.94, 1.54, 1.27, 1.78, 1.85, 2.34, 1.74, 1.32, 0.6, 0.88, 2.33, 1.49, 0.7, 0.73, 2.32, 1.92, 2.6, 2.51, 2.09, 3.09, 1.1, 1.76, 2.74, 2.1, 0.73, 1.47)

n = length(muestra)
MLE = sqrt(mean(muestra^2))
IMLE = mean((-2/MLE + 2*muestra^2/MLE^3)^2)

za = qnorm(0.95)
zb1 = qnorm(0.025)
zb2 = qnorm(0.975)

EXTA = 1 + za/(sqrt(n*IMLE))
EXTB1 = 1 + zb1/(sqrt(n*IMLE))
EXTB2 = 1 + zb2/(sqrt(n*IMLE))

PVALA = pnorm(sqrt(n*IMLE)*(MLE - 1), lower.tail = FALSE) 
PVALB = 2*pnorm(sqrt(n*IMLE)*(MLE - 1), lower.tail = FALSE)
´
library(ggplot2)
library(dplyr)

dnRC1 <- function(x){
  y <- dnorm(x)
  y[x < za] <- 0
  return(y)
}

dnRC2 <- function(x){
  y <- dnorm(x)
  y[x < zb2 & x > zb1] <- 0
  return(y)
}

fig1 <- ggplot(data = tibble(x = c(-3.5,3.5)),aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1) +
  stat_function(fun = dnRC1,
                geom = 'area', fill="red", alpha=0.3) +
  geom_vline(xintercept = za, colour = 'red', size = 1, linetype = 'dashed') + 
  geom_vline(xintercept = sqrt(n*IMLE)*(MLE - 1), colour = 'blue', size = 1, linetype = 'dashed') +
  theme_classic() +
  ylab('Probability density') +
  xlab(expression(sqrt(n*hat(I)(hat(theta)))*(hat(theta) - 1))) +
  ggtitle('Distribution of the statistic under the null hypothesis')


fig1

fig2 <- ggplot(data = tibble(x = c(-3.5,3.5)),aes(x)) +
        stat_function(fun = dnorm, args = list(mean = 0, sd = 1), size = 1) +
        stat_function(fun = dnRC2,
                      geom = 'area', fill="red", alpha=0.3) +
        geom_vline(xintercept = zb1, colour = 'red', size = 1, linetype = 'dashed') +
        geom_vline(xintercept = zb2, colour = 'red', size = 1, linetype = 'dashed') +
        geom_vline(xintercept = sqrt(n*IMLE)*(MLE - 1), colour = 'blue', size = 1, linetype = 'dashed') +
        theme_classic() +
        ylab('Probability density') +
        xlab(expression(sqrt(n*hat(I)(hat(theta)))*(hat(theta) - 1))) +
        ggtitle('Distribution of the statistic under the null hypothesis')
                        
                      
fig2

#---------------------#
#        Ex 6.15      #
#---------------------#

srs <- c(1.46, 1.23, 2.34, 1.44, 1.79, 1.4, 2.18, 1.4, 1.79, 1.06, 1.38, 1.31, 1.78, 1.39, 1.41, 1.88, 1.51, 2.42, 2.13, 2.02, 2.32, 1.01, 1.28, 0.93, 1.6, 1.69, 3.37, 2.1, 2, 1.81)
srsn <- log(srs)
n <- length(srs)

M <- mean(srsn)
S <- var(srsn)

EXT1 <- log(2) + qnorm(0.025)*sqrt(S/n)
EXT2 <- log(2) + qnorm(0.975)*sqrt(S/n)

THETAMLE <- M + S/2

#---------------------#
#        Ex 6.18      #
#---------------------#

mu <- 0
alfa <- 0.05
n <- 25

za1 <- qchisq(alfa/2, df = n-1)/(n-1)
za2 <- qchisq(1-alfa/2, df = n-1)/(n-1)
zb <- qchisq(alfa, df = n-1)/(n-1)
zc <- qchisq(1-alfa, df = n-1)/(n-1)

t <- seq(0.25,1.75, length.out = 1000)
plot(t, pchisq((n-1)*zb/t, df = n-1), type = 'l')
lines(t, pchisq((n-1)*zc/t, df = n-1, lower.tail = FALSE), col = 'red')
lines(t, pchisq((n-1)*za1/t, df = n-1) + pchisq((n-1)*za2/t, df = n-1, lower.tail = FALSE), col = 'blue')


#---------------------#
#        Ex 6.19      #
#---------------------#

M <- 1000
n <- 25

alfa <- 0.1

z1 <- qnorm(1 - alfa)
z2 <- qnorm(alfa)
z3i <- qnorm(alfa/2)
z3d <- qnorm(1 - alfa/2)

n2 <- 1000
mu <- seq(-2,2,length.out = n2)
rel_freq_1 <- rep(0, length.out = 100)
rel_freq_2 <- rep(0, length.out = 100)
rel_freq_3 <- rep(0, length.out = 100)

#Vaya puta mierda usa replicate tu
#Que coño es que el primer bucle es un replicate
#El segundo es un apply o algo asi 100%
for(i in 1:n2){
  test_result1 <- rep(0, M)
  test_result2 <- rep(0, M)
  test_result3 <- rep(0, M)
  for(j in 1:M){
    srs <- rnorm(n, mean = mu[i], sd = 1)
    test_result1[j] <- (mean(srs) > z1)
    test_result2[j] <- (mean(srs) < z2)
    test_result3[j] <- (mean(srs) < z3i) | (mean(srs) > z3d)
  }
  rel_freq_1[i] <- sum(test_result1)/M
  rel_freq_2[i] <- sum(test_result2)/M
  rel_freq_3[i] <- sum(test_result3)/M
}

plot(mu, rel_freq_1, type = 'l')
lines(mu, rel_freq_2, col = 'red')
lines(mu, rel_freq_3, col = 'blue')

#---------------------#
#        Ex 6.20      #
#---------------------#

#Same shit as before

#---------------------#
#        Ex 6.21      #
#---------------------#

srsa <- c(0.8, 0.2, 1.8, 1.3, 3.8, 0.4, 1.6, 2, 0.4, 1.4, 0.1, 0.6, 2, 2.3, 0.6, 0.2, 0.7, 1.2, 0.3, 1.8, 0.2)
srsb <- c(1.6, 14, 0, 8.4, 4.9, 1.7, 0.2, 2.6, 0.9, 0.3, 1.6, 1.7, 2.3, 2.2, 5.6, 2.8, 2.6, 0, 2, 2.7, 4)

quota <- function(srs){2^length(srs)*exp(-0.5*sum(srs))}
quotb <- function(srs){2^length(srs)*exp(sum(srs))}

na <- length(srsa)
nb <- length(srsb)

dista <- replicate(10^4, {srs <- rexp(na, rate = 1)
                          quota(srs)})
distb <- replicate(10^4, {srs <- rexp(nb, rate = 2)
                          quotb(srs)})

cdfa <- ecdf(dista)
cdfb <- ecdf(distb)

pvala <- cdfa(quota(srsa))
pvalb <- cdfa(quotb(srsb))

#---------------------#
#        Ex 6.22      #
#---------------------#

set.seed(666)

#:::::::::::::#
#  Section a  #
#:::::::::::::#

#We introduce the data
given_sample = c(45.4, -0.2, 0.5, 0.8, 0.1, 1.3, 4, 0.6, 1.8, 0.3, -0.6, 7.2, -0.5, 0.6, 0.7, 0.2,
                 -3.3, 0.2, 0.5, 0.1, 0.4, -0.3, 0.3, 0.2, 0.2, 0.1, 0.7, 0.5, -0.6, 1, 0, -1, 0, 14)
n = length(given_sample)

s = 0.5
alpha = 0.05

est_a <- function(x){prod((1+4*(x-1)^2)/(1 + 4*x^2))}

theta0 = 0
theta1 = 1

m = 10^4
edlq_a <- replicate(m,{srs <- rcauchy(n, location = theta0, scale = s)
return(est_a(srs))})
ka <- quantile(edlq_a, alpha)
est_a(given_sample) < ka[[1]]
ecdf_a <- ecdf(edlq_a)
pvala = ecdf_a(est_a(given_sample))


#:::::::::::::#
#  Section b  #
#:::::::::::::#

theta1b1 <- 2
theta1b2 <- 3

est <- function(x, theta0, theta1){prod((1 + 4*(x - theta1)^2)/(1+4*(x-theta0)^2))}

edlq_b1 <- replicate(m, {srs <- rcauchy(n, location = theta0, scale = s)
return(est(srs, theta0, theta1b1))} )
edlq_b2 <- replicate(m, {srs <- rcauchy(n, location = theta0, scale = s)
return(est(srs, theta0, theta1b2))} )

kb1 <- quantile(edlq_b1, alpha)
kb2 <- quantile(edlq_b2, alpha)
est(given_sample, theta0, theta1b1) < kb1[[1]]
est(given_sample, theta0, theta1b2) < kb2[[1]]
ecdf_b1 <- ecdf(edlq_b1)
ecdf_b2 <- ecdf(edlq_b2)
pvalb1 = ecdf_b1(est(given_sample, theta0, theta1b1))
pvalb2 =ecdf_b2(est(given_sample, theta0, theta1b2))

print(c(ka, est_a(given_sample), pvala))
print(c(kb1, est(given_sample, theta0, theta1b1), pvalb1))
print(c(kb2, est(given_sample, theta0, theta1b2), pvalb2))


#:::::::::::::#
#  Section c  #
#:::::::::::::#

theta0 = 4

pvalores <- function(theta1){
  edlq <- replicate(m,{srs <- rcauchy(n, location = theta0, scale = s)
  return(est(srs, theta0, theta1))})
  ecdfs <- ecdf(edlq)
  return(ecdfs(est(given_sample, theta0, theta1))) 
}

t = seq(3,6,length.out = 1000)
pvalores2 <- sapply(t, FUN = pvalores)
plot(t, pvalores2)

sombra <- function(x){
  if(pvalores2[which(t == x)] > alpha){ return(0) }
  if(which(t == x) < 500){ return(0) }
  return(pvalores2[which( t == x)])
}
sombras <- sapply(t, FUN = sombra)

df1 = data.frame(x = t, y = pvalores2, area = sombras)

fig1 <- ggplot(data = df1, mapping = aes(x = x)) +
  geom_line(aes(y = y)) + 
  geom_area(aes(y = area), color = 'red', alpha = 0.5) +
  xlab(expression(theta[1])) +
  ylab('p-value') +
  ggtitle('P-values for different values of the parameter') +
  geom_hline(yintercept = 0.05, col = 'red', lty = 2) +
  stat_function(fun  = sombra, geom = 'area')

fig1

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
  
  return(((1/(2*theta1))^(n1*theta1))*((1/(2*theta2))^(n2*theta2))*exp(-2*n1*(1-(theta1 + theta2))))
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
