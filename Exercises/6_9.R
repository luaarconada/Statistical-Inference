#Exercise 6.9
decl<-c(31.2,20.3,6.6,13.3,6.1)#2023
recv<-c(28.25,20.99,15.21,12.97,6.86)#2019
n<-3585
alpha<-0.05

#H0:p=p0
#H1:p!=p0

#Z=(p_hat-p0)/(sqrt(p0*(1-p0)/n)) converges in distribution to N(0,1) (CLT)
#PSOE
p0<-decl[1]/100
p<-recv[1]/100
Z<-(p-p0)/(sqrt(p0*(1-p0)/n))
z<-qnorm(alpha/2,lower.tail = FALSE)
c1<- -z
c2<-z
#if Z<c1 or Z>z2, we reject H0
Z<c1#this is true so we reject H0,
#WE HAVE ENOUGH EVIDENCE TO SAY THAT THE PORPORTION OF VOTES IS NOT THE SAME 

#PP
p0<-decl[2]/100
p<-recv[2]/100
Z<-(p-p0)/(sqrt(p0*(1-p0)/n))
z<-qnorm(alpha/2,lower.tail = FALSE)
c1<- -z
c2<-z
Z<c1#False
Z>c2#False
#then we dont have enough evidence to reject H0

#VOX
p0<-decl[3]/100
p<-recv[3]/100
Z<-(p-p0)/(sqrt(p0*(1-p0)/n))
z<-qnorm(alpha/2,lower.tail = FALSE)
c1<- -z
c2<-z
Z<c1#False
Z>c2#true
#WE HAVE ENOUGH EVIDENCE TO SAY THAT THE PORPORTION OF VOTES IS NOT THE SAME 

#Podemos-iu
p0<-decl[4]/100
p<-recv[4]/100
Z<-(p-p0)/(sqrt(p0*(1-p0)/n))
z<-qnorm(alpha/2,lower.tail = FALSE)
c1<- -z
c2<-z
Z<c1#False
Z>c2#False
#dont reject H0

#Cs
p0<-decl[5]/100
p<-recv[5]/100
Z<-(p-p0)/(sqrt(p0*(1-p0)/n))
z<-qnorm(alpha/2,lower.tail = FALSE)
c1<- -z
c2<-z
Z<c1#False
Z>c2#False
#dont reject H0
