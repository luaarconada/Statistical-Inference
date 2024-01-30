# Define the inverse of the RV's CDF
cdf_inv =  function(x, theta){((0 < x & x < 1)*(theta/(1-x))) + 0}

# Define the values of theta
thetas = c(1, 2.5, 5, 8.5)

# Create the sequence of sample sizes
lower_n = 5
upper_n = 500
sample_sizes = seq(lower_n, upper_n, length.out = upper_n - lower_n + 1)

# Define number of rows and columns
m = length(thetas)
n = length(sample_sizes)

# Initialise storage matrix, each row corresponds to a a value of theta 
# and each column to the MLE estimator for a specific sample size
storage_matrix = matrix(data = 0, nrow = m, ncol = n)

# Set seed
set.seed(24)

# Fill the storage matrix
for (i in 1:m) {
  
  # Get the corresponding value of theta
  theta = thetas[i]
  
  for(j in 1:n) {
    
    # Generate samples of the U(0,1) distribution
    uniform_realisations = runif(sample_sizes[j], 0, 1)
    
    # Convert to realisations of the target distribution
    target_realisations = cdf_inv(uniform_realisations, theta)
    
    # Store the value of the MLE estimator
    storage_matrix[i, j] = min(target_realisations)
    
  }
  
}

# Create an empty plot with the corresponding limits and axis labels
par(mfrow = c(1, 1))
plot(NA, , ylim = c(min(thetas)-1, max(storage_matrix)+1), 
     xlim = c(0, max(upper_n)), ylab = "Theta", 
     xlab = "Sample Size", yaxt = "n")

axis(2, at = thetas, labels = thetas)

for (i in 1:m) {
  
  abline(h = thetas[i])
  lines(sample_sizes, storage_matrix[i,], type = "l", col = "blue")
  
}