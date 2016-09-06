# Data for the toy example ----
x = c(30,37,36,43,42,43,43,46,41,42)
n = length(x)

# sample mean
xbar = mean(x)

nboot = 20
# Generate 20 bootstrap samples, i.e. an n x 20 array of 
# random resamples from x
tmpdata = sample(x,n*nboot, replace=TRUE)
bootstrap_sample = matrix(tmpdata, nrow=n, ncol=nboot)
bootstrap_sample
# Compute the means $\xbar^*$
bsmeans = colMeans(bootstrap_sample)

# Compute $\delta^*$ for each bootstrap sample
deltastar = bsmeans - xbar

# Sort the results
sorteddeltastar = sort(deltastar)
hist(sorteddeltastar, nclass=6)
sorteddeltastar
       
# Find the .1 and .9 critical values of $\delta^*$
d9 = sorteddeltastar[2]
d1 = sorteddeltastar[18]

# Find and print the 80\% confidence interval for the mean
CI = xbar - c(d1,d9)
print(CI)

#simulating the bootstrap variance----
# Repeatedly sample from exp(2) to find 
# the mean and variance of the median of data
# Repeat this by resampling from one sample and compare
lambda = 2
n = 70
ntrials = 10000
nboot = 10000
meds.true = rep(0,ntrials)
for (j in 1:ntrials)
{
  expdata= rexp(n, lambda)
  meds.true[j] = median(expdata)
}

#Now bootstrap
expdata = rexp(n,lambda)
meds.bs = rep(0,ntrials)
for (j in 1:ntrials)
{
  bsdata= sample(expdata, n, replace=TRUE)
  meds.bs[j] = median(bsdata)
}

mn.true = mean(meds.true)
sd.true = sqrt(var(meds.true))
mn.bs = mean(meds.bs)
sd.bs = sqrt(var(meds.bs))
s = sprintf("True mean of median: %.3f, std dev of median: %.3f", 
              mn.true, sd.true)
print(s)
s = sprintf("BS mean of median: %.3f, std dev of median: %.3f", 
            mn.bs, sd.bs)
print(s)
# true median
log(2)/lambda
