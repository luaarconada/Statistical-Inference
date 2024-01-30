#input data
avg_pension = 18305.84
max_pension = 42823.34
A <- c(20708, 21630, 15670, 17764, 19308, 19053, 19196, 18341, 19002, 18459, 19577, 19603, 16950, 17254, 17439, 18507, 18421, 16148, 21317, 18076, 21774, 18920, 16742, 15120, 15120, 17610, 19122, 19956, 19876, 17588, 17309, 19843, 16826, 17088, 17740, 20648, 16064, 19990, 15924, 17079)
B <- c(35261, 37821, 32167, 41367, 42732, 49535, 38236, 49175, 46720, 34275, 39510, 37475, 42905, 46869, 43089, 30767, 44708, 39449, 40953, 42604, 41246, 39125, 48394, 37166, 44107, 41153, 42399, 45076, 38372, 37798, 36956, 44789, 34909, 44991, 35470)
C <- c(A,B)

#a.)
n = length(A) #sample size of A
mean(A) #mean of A
sd(A) #quasi-standard deviation of A

#comparison of A to the avg_pension
t_A1 = qt(.05, df = n - 1, lower.tail = FALSE)
k_A1 = (sd(A)/sqrt(n)) * t_A1  + avg_pension
print(c("k_A1 =", k_A1))
print(c("sample mean is", mean(A)))

#comparison of A to the max_pension
t_A2 = qt(.05, df = n - 1, lower.tail = TRUE)
k_A2 = (sd(A)/sqrt(n)) * t_A2  + max_pension
print(c("k_A2 =", k_A2))
print(c("sample mean is", mean(A)))

#b.)
n = length(B) #sample size of B
mean(B) #mean of B
sd(B) #quasi-standard deviation of B

#comparison of B to the avg_pension
t_B1 = qt(.05, df = n - 1, lower.tail = FALSE)
k_B1 = (sd(B)/sqrt(n)) * t_B1  + avg_pension
print(c("k_B1 =", k_B1))
print(c("sample mean of B is", mean(B)))

#comparison of B to the max_pension
t_B2 = qt(.05, df = n - 1, lower.tail = TRUE)
k_B2 = (sd(A)/sqrt(n)) * t_B2  + max_pension
print(c("k_B2 =", k_B2))
print(c("sample mean of B is", mean(B)))

#c.) Define Group C to be Group A and Group B data combined
n = length(C) #sample size of C
mean(C) #mean of C
sd(C) #quasi-standard deviation of C

#comparison of C to the avg_pension
t_C1 = qt(.05, df = n - 1, lower.tail = FALSE)
k_C1 = (sd(C)/sqrt(n)) * t_C1  + avg_pension
print(c("k_C1 =", k_C1))
print(c("sample mean of C is", mean(C)))

#comparison of C to the max_pension
t_C2 = qt(.05, df = n - 1, lower.tail = TRUE)
k_C2 = (sd(A)/sqrt(n)) * t_C2  + max_pension
print(c("k_C2 =", k_C2))
print(c("sample mean of C is", mean(C)))
