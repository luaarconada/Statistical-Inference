
## -------------------------------------------------------------------------- ##
## First practical exercise Statistical Inference 2023/2024, 2023-10-11       ##
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

# Import the data
temps <- read.table(file = "all-temps.txt", header = FALSE)
temps <- temps$V1

# Histogram in relative frequency
hist(temps, probability = TRUE, xlab = "Temperatures", main = "", breaks = 50)

## -------------------------------------------------------------------------- ##
## ----------------------------    Task 2    -------------------------------- ##
## -------------------------------------------------------------------------- ##

pdf <- function(x, alpha, beta){
    ((sqrt(alpha/pi))/x) * exp(-alpha*(log(x)-beta)^2)}



for (alpha in seq(1,3,0.5)){
  for (beta in seq(0,4,0.5)){
    x=seq(0.1,150,length.out=100)
    fx= lapply(x,alpha,beta, FUN= pdf)
    lines(x,fx)
}}



## -------------------------------------------------------------------------- ##
## ----------------------------    Task 3    -------------------------------- ##
## -------------------------------------------------------------------------- ##



## -------------------------------------------------------------------------- ##
## ----------------------------    Task 4    -------------------------------- ##
## -------------------------------------------------------------------------- ##



## -------------------------------------------------------------------------- ##
## ----------------------------    Task 5    -------------------------------- ##
## -------------------------------------------------------------------------- ##


mle = function(x){
  n=length(x)
  y=log(x)
  mean=mean(y)
  var=var(y)
  alpha=1/(2*var)
  beta=mean
  out=c(alpha,beta)
}

z= seq(0.1,150,length.out=1000)


mled = mle(z)
mled
fz=lapply(z,mled[1],mled[2], FUN=pdf)
lines(z,fz)

#for (z in seq(0.1,150,length.out=100)){
 # mled=mle(z)
  #fz= lapply(z,mled[1],mled[2], FUN= pdf)
  #lines(z,fz)
  #}

