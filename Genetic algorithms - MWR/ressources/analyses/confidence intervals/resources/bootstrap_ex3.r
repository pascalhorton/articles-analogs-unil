rm(list=ls())
# Simulated data
set.seed(123)
data0 = rgamma(383,5,3)
mean(data0) # Sample mean

hist(data0) # Histogram of the data

library(boot) 

# function to obtain the mean
Bmean <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

# bootstrapping with 1000 replications 
results <- boot(data=data0, statistic=Bmean, R=1000)

# view results
results 
plot(results)

# get 95% confidence interval 
boot.ci(results, type=c("norm", "basic", "perc", "bca"))