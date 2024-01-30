library(ggplot2)
mu0 <- 0          
sigma <- 1        
n1 <- 25
n2 <- 50
M <- 1000    
theta = seq(0.1, 10, l = 25)
alpha = 0.1

# Initialize a vector to store relative rejection frequencies
rel_freq_both <- numeric(length(theta))
rel_freq_left <- numeric(length(theta))
rel_freq_right <- numeric(length(theta))
ratio <- numeric(length(theta))

# Perform the simulation for both tails
for (i in 1:length(theta)) {
  #   i = 1
  rejection_count_both <- 0  
  rejection_count_left <- 0
  rejection_count_right <- 0
  
  for (j in 1:M) {
    # Draw a random sample from N(mu_i, sigma^2)
    sample_data1 <- rnorm(n1, mean = 0, sd = sigma)
    sample_data2 <- rnorm(n2, mean = 0, sd = sigma/sqrt(theta[i]))
    
    # Perform a t-test for both tails
    test_result_gre = var.test(sample_data1,sample_data2, ratio = 1, alternative = "greater")
    test_result_les = var.test(sample_data1,sample_data2, ratio = 1, alternative = "less")
    test_result_bot = var.test(sample_data1,sample_data2, ratio = 1, alternative = "two.sided")
    # Check for rejections (you can adjust the significance level as needed)
    
    if (test_result_bot$p.value < alpha) {
      rejection_count_both <- rejection_count_both + 1
    }
    if (test_result_les$p.value  < alpha) {
      rejection_count_left <- rejection_count_left + 1
    }
    if (test_result_gre$p.value  < alpha) {
      rejection_count_right <- rejection_count_right + 1
    }
  }
  
  # Calculate relative rejection frequencies
  
  rel_freq_both[i] <- rejection_count_both / M
  rel_freq_left[i] <- rejection_count_left / M
  rel_freq_right[i] <- rejection_count_right / M
}

# Create a data frame for plotting
results <- data.frame(theta = theta, rel_freq_both = rel_freq_both, rel_freq_left = rel_freq_left, rel_freq_right = rel_freq_right)

# Create a ggplot2 plot with all lines superposed
ggplot(data = results, aes(x = theta)) +
  geom_line(aes(y = rel_freq_both), color = "blue", linetype = "solid") +
  geom_line(aes(y = rel_freq_left), color = "green", linetype = "dashed") +
  geom_line(aes(y = rel_freq_right), color = "red", linetype = "dotted") +
  labs(x = "var", y = "Relative Rejection Frequency", title = "Relative Rejection Frequency vs. ratio of variances") +
  theme_minimal()