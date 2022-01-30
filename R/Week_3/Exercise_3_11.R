library(quantmod)

# Exercise ----

getSymbols("GOLDPMGBD228NLBM",src="FRED")
gold <- na.omit(GOLDPMGBD228NLBM)
gold <- gold["1979-12-31/2017-12-31"]
names(gold) <- "TR"
# Next, we calculated its daily log returns:
logret <- diff(log(gold))[-1]
# Assuming that daily log returns are normally distributed, we used the following R commands to
# estimate the two parameters of the normal distribution: mean and standard deviation.
round(mean(logret), 6)
round(sd(logret), 6)

# In the lectures, we used 3 simulation methods to find the VaR and ES at the 95% confidence
# level for a 10-day horizon.
# Simulation Method 1:
#   used the “moments” package in R to estimate the parameters of the scaled student-t
# distribution:
  library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, “t”)
round(t.fit$estimate,6)
# To estimate the VaR and ES at the 95% confidence interval for a 10-day horizon, we simulated
# ten 1-day outcomes and add them up. Repeat 100,000 times. Then calculate the VaR and ES for
# these 100,000 outcomes.
alpha <- 0.05
RNGkind(sample.kind = "Rounding")
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
# Simulation Method 2:
#   Instead of simulating from the scaled student-t distribution, the second simulation method
# randomly samples ten 1-day observations from the empirical distribution and add them up.
# Repeat 100,000 times. Then calculate the VaR and ES for these 100,000 outcomes.
alpha <- 0.05
RNGkind(sample.kind = "Rounding")
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
# Simulation Method 3:
#   The third simulation method also samples from the empirical distribution, but draws blocks of
# ten consecutive 1-day outcomes and add them up. Repeat 100,000 times. Then calculate the
# VaR and ES for these 100,000 outcomes.
alpha <- 0.05
RNGkind(sample.kind = "Rounding")
set.seed(123789)
rdat <- as.vector(logret)
rvec <- rep(0,100000)
posn <- seq(from=1,to=length(rdat)-9,by=1)
rpos <- sample(posn,100000,replace=TRUE)
for (i in 1:10) {
  rvec <- rvec+ rdat[rpos]
  rpos <- rpos+1
}
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
