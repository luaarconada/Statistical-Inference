mu_0=0
n=25
s=1
alpha=0.1

w <- function(alpha,mu,mu_0,s,n){pnorm(-qnorm(alpha,lower.tail = FALSE)+(mu-mu_0)/(s/sqrt(n)))}

mu=seq(-1,1,by=0.001)
plot(mu,w(alpha,mu,mu_0,s,n), type="l",col="red",xlab=expression(mu),ylab="Power",xaxt="n",ylim=c(0,1))
lines(mu,w(alpha,-mu,-mu_0,s,n), type="l",col="green")
lines(mu,w(alpha/2,mu,mu_0,s,n)+w(alpha/2,-mu,-mu_0,s,n), type="l",col="blue")
lines(mu,rep(0,length(mu)),col="black",lty=2)
lines(mu,rep(1,length(mu)),col="black",lty=2)
lines(mu,rep(alpha,length(mu)),col="black",lty=2)
lines(rep(mu_0,2),c(0,1),col="black",lty=2)
axis(2,seq(0,1,0.1))
axis(1,seq(-1,1,0.2))
legend("right",legend=c(expression(omega[a]),expression(omega[b]),
                        expression(omega[c])),lty=1,
                        col=c("red","green","blue"),cex=0.8)

# A

n_values=c(5,50,200)
par(mfrow = c(1, 3))
for (i in n_values){
  plot(mu,w(alpha,mu,mu_0,s,i), type="l",col="red",xlab=expression(mu),ylab="Power",xaxt="n",ylim=c(0,1),main=paste("n =",i))
  lines(mu,w(alpha,-mu,-mu_0,s,i), type="l",col="green")
  lines(mu,w(alpha/2,mu,mu_0,s,i)+w(alpha/2,-mu,-mu_0,s,i), type="l",col="blue")
  lines(mu,rep(0,length(mu)),col="black",lty=2)
  lines(mu,rep(1,length(mu)),col="black",lty=2)
  lines(mu,rep(alpha,length(mu)),col="black",lty=2)
  lines(rep(mu_0,2),c(0,1),col="black",lty=2)
  axis(2,seq(0,1,0.1))
  axis(1,seq(-1,1,0.2))
  legend("right",legend=c(expression(omega[a]),expression(omega[b]),
                          expression(omega[c])),lty=1,
         col=c("red","green","blue"))
}


# B


s_values=c(0.1,2,10)
par(mfrow = c(1, 3))
for (i in s_values){
  plot(mu,w(alpha,mu,mu_0,i,n), type="l",col="red",xlab=expression(mu),ylab="Power",xaxt="n",ylim=c(0,1),main=bquote(sigma == .(i)))
  lines(mu,w(alpha,-mu,-mu_0,i,n), type="l",col="green")
  lines(mu,w(alpha/2,mu,mu_0,i,n)+w(alpha/2,-mu,-mu_0,i,n), type="l",col="blue")
  lines(mu,rep(0,length(mu)),col="black",lty=2)
  lines(mu,rep(1,length(mu)),col="black",lty=2)
  lines(mu,rep(alpha,length(mu)),col="black",lty=2)
  lines(rep(mu_0,2),c(0,1),col="black",lty=2)
  axis(2,seq(0,1,0.1))
  axis(1,seq(-1,1,0.2))
  legend("right",legend=c(expression(omega[a]),expression(omega[b]),
                          expression(omega[c])),lty=1,
         col=c("red","green","blue"))
}

# C


mu_0_values=c(-1,5,10)
par(mfrow = c(1, 3))
for (i in mu_0_values){
  mu=seq(i-1,i+1,0.001)
  plot(mu,w(alpha,mu,i,s,n), type="l",col="red",xlab=expression(mu),ylab="Power",xaxt="n",ylim=c(0,1),main=bquote(mu[0] == .(i)))
  lines(mu,w(alpha,-mu,-i,s,n), type="l",col="green")
  lines(mu,w(alpha/2,mu,i,s,n)+w(alpha/2,-mu,-i,s,n), type="l",col="blue")
  lines(mu,rep(0,length(mu)),col="black",lty=2)
  lines(mu,rep(1,length(mu)),col="black",lty=2)
  lines(mu,rep(alpha,length(mu)),col="black",lty=2)
  lines(rep(i,2),c(0,1),col="black",lty=2)
  axis(2,seq(0,1,0.1))
  axis(1,seq(i-1,i+1,0.2))
  legend("right",legend=c(expression(omega[a]),expression(omega[b]),
                          expression(omega[c])),lty=1,
         col=c("red","green","blue"))
}