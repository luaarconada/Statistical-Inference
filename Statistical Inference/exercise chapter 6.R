x= c(785,805,790,793,802)
n=length(x)
mu0=800
mean = mean(x)
sd= var(x)
T=(mean-mu0)/(sqrt(sd)/sqrt(n))
T

-qnorm(0.05, lower.tail=FALSE)

t.test(x, alternative='less', mu=800, paired=FALSE)

-qt(0.05, df=4,lower.tail=FALSE)

qt(0.05,df=21,lower.tail=FALSE)

qchisq(1-0.025,df=16,lower.tail=FALSE)
1-pchisq(24.4,df=16)
2*(1-pchisq(24.4,df=16))


# Exercise 6.6
x=c(16.3, 16.6, 14.8, 14, 16.4, 10.9, 16.4, 14.4, 14.4, 12.9, 14.7, 19.1, 15.8)
y=c(16.9, 17.2, 14.7, 13.6, 16.9, 9.3, 17, 14.1, 14.1, 12.1, 14.6,20.7, 16.2, 16.6, 12.6, 17.6, 12.2, 14.)
n_1=length(x)
n_2=length(y)
var.test(x,y,ratio=1,alternative='greater')
var1=var(x)
var2=var(y)
var1/var2
