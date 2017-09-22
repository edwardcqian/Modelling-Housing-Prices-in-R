model.final = lm(SALES~IMP*NBHD, data = data.1)

nreps <- 100 # number of replications
n <- nrow(data.1) # total number of observations
ntrain <- ceiling(0.9*n) # size of training set (90% of the data set)
ntest <- n-ntrain # size of test set
sse1 <- rep(NA, nreps) # sum-of-square errors for each CV replication
sse2 <- rep(NA, nreps)
log.likelihood1 <- rep(NA, nreps) # likelihod of of the model for the numerator for each replication
log.likelihood2 <- rep(NA, nreps) # likelihod of of the model for the denominator for each replication
#Start of for loop
for(ii in 1:nreps) {
  if(ii%%100 == 0) message("ii = ", ii)
  # randomly select training observations
  train.ind <- sample(n, ntrain) # training observations
  M1.cv <- update(model.simp, subset = train.ind)
  M2.cv <- update(model.final, subset = train.ind)
  # testing residuals for both models
  # that is, testing data - predictions with training parameters
  M1.res <- data.1$SALES[-train.ind] - predict(M1.cv, newdata = data.1[-train.ind,])
  M2.res <- data.1$SALES[-train.ind] - predict(M2.cv, newdata = data.1[-train.ind,])
  # total sum of square errors
  sse1[ii] <- sum((M1.res)^2)
  sse2[ii] <- sum((M2.res)^2)
  # testing likelihood ratio
  M1.sigma <- sqrt(sum(resid(M1.cv)^2)/ntrain) # MLE of sigma
  M2.sigma <- sqrt(sum(resid(M2.cv)^2)/ntrain)
  log.likelihood1[ii] <- sum(dnorm(M1.res, mean = 0, sd = M1.sigma, log = TRUE))
  log.likelihood2[ii] <- sum(dnorm(M2.res, mean = 0, sd = M2.sigma, log = TRUE))
}

#Comparison of SSE
mean(sse1)
mean(sse2)

mean(log.likelihood1[ii] - log.likelihood2[ii])
