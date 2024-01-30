
### Exercise 6.4

## Data

avg_pension <- 18305.84
max_pension <- 42823.34

A <- c(20708, 21630, 15670, 17764, 19308, 19053, 19196, 18341, 19002, 18459, 19577, 19603, 16950, 17254, 17439, 18507, 18421, 16148, 21317, 18076, 21774, 18920, 16742, 15120, 15120, 17610, 19122, 19956, 19876, 17588, 17309, 19843, 16826, 17088, 17740, 20648, 16064, 19990, 15924, 17079)
B <- c(35261, 37821, 32167, 41367, 42732, 49535, 38236, 49175, 46720, 34275, 39510, 37475, 42905, 46869, 43089, 30767, 44708, 39449, 40953, 42604, 41246, 39125, 48394, 37166, 44107, 41153, 42399, 45076, 38372, 37798, 36956, 44789, 34909, 44991, 35470)
C <- c(A, B)

## Questions

# a)

# Are professionals in Group A better paid than the average pension?
t.test(x = A, mu = avg_pension, alternative = "greater")
# There is no evidence in favor of H1: mu > avg_pens
# with a confidence level 95%. The average salary of
# professionals in group A is not larger than the
# average pension

# And worse paid than the maximum pension?
t.test(x = A, mu = max_pension, alternative = "less")
# Yes. There is evidence showing that the average salary
# of professionals in group A is smaller than the maximum
# pension

# b)

# Are professionals in Group B better paid than the average pension?
t.test(x = B, mu = avg_pension, alternative = "greater")
# Yes, they are

# And worse paid than the maximum pension?
t.test(x = B, mu = max_pension, alternative = "less")
# Yes, they are

# c)

# Are professionals in Group C better paid than the average pension?
t.test(x = C, mu = avg_pension, alternative = "greater")

# And worse paid than the maximum pension?
t.test(x = C, mu = max_pension, alternative = "less")

