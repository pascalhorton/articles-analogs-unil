# Based on http://stats.stackexchange.com/questions/112829/how-do-i-calculate-confidence-intervals-for-a-non-normal-distribution
library(ncdf4)
library(boot) 

rm(list=ls())

#fileName <- "scores_reference.nc"
fileName <- "scores_optimized.nc"

# Open file
ncFile <- nc_open(fileName)

scores <- ncvar_get(
  ncFile,
  varid = "forecast_scores"
)

scoresMean <- mean(scores)

# Function to obtain the mean
Bmean <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

# Bootstrapping with 1000 replications 
results <- boot(data=scores, statistic=Bmean, R=10000)

# View results
results 
plot(results)

# Get 95% confidence interval 
ci <- boot.ci(results, conf = c(0.90, 0.95), type="basic")
print(ci)
