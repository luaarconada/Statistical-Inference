# Tema 5

## Exercise 2

sample_copy = c(916, 892, 895, 904, 913, 916, 895, 885)
n = length(sample_copy)
desv_t = 12
confidence = 0.05
sample_mean = mean(sample_copy)
up= sample_mean + qnorm(0.975)*(desv_t/sqrt(n))
down= sample_mean - qnorm(0.975)*(desv_t/sqrt(n))

c(down, up)
# Exercise 3

a = sample_copy - sample_mean
b = a^2
c = sum(b) / 7
c
qt()
up= sample_mean + qt(0.975,7)*(c/sqrt(n))
down= sample_mean - qt(0.975,7)*(c/sqrt(n))
c(down, up)
c^2


# Exercise 4
sample_copy = c(4.1, 5.2, 10.2)
n = length(sample_copy)
a = sample_copy - mean(sample_copy)
b = a^2
c = sum(b) / (length(sample_copy) -1)

up= c*(n-1)/ qchisq(0.95,length(sample_copy)-1)
down= c*(n-1)/ qchisq(0.05,length(sample_copy)-1)
c(down, up)
c^2

# Exercise 5

x1 = c(32,37,35,28,41,44,35,31,34)
x2 = c(35,31,29,25,34,40,27,32,31)

a = mean(x1) - mean(x2)

up= a + qnorm(0.975)*(sqrt(22)*sqrt(1/length(x1) + 1/length(x1)))
down= a - qnorm(0.975)*(sqrt(22)*sqrt(1/length(x1) + 1/length(x1)))
c(down, up)

# Exercise 6
x1 = c(32,37,35,28,41,44,35,31,34)
x2 = c(35,31,29,25,34,40,27,32,31)

a = mean(x1) - mean(x2)

cuasi1 = sum((x1 -mean(x1))^2)/(length(x1) - 1)
cuasi2 = sum((x2 -mean(x2))^2)/(length(x2) - 1)

aprox = ((length(x1)-1) * cuasi1 + (length(x2)-1) * cuasi2)/(2*length(x1) -2)
aprox

up= a + qt(0.975, length(x1) +length(x1) -1)*(sqrt(aprox/length(x1) + aprox/length(x1)))
down= a - qt(0.975, length(x1) +length(x1) -1)*(sqrt(aprox/length(x1) + aprox/length(x1)))
c(down, up)

# Exercise 7
var1 = 52
var2 = 71

c((var1/var2)/qf(0.975, 10,13), (var1/var2)/qf(0.025, 10,13))

# Exercise 8
n = 64
quasivar = 256
media = 33

up = media + (sqrt(quasivar)/sqrt(n))*qnorm(0.95)
down = media - (sqrt(quasivar)/sqrt(n))*qnorm(0.95)
c(up, down)

# Exercise 9
n1 = 50
p1 = 0.24
var1 = p1*(1-p1)

n2 = 60
p2 = 0.2
var2 = p2*(1-p2)

diferencia = p1-p2

up = diferencia + qnorm(0.99)*sqrt(var1/n1 + var2/n2)
down = diferencia - qnorm(0.99)*sqrt(var1/n1 + var2/n2)

c(up, down)

