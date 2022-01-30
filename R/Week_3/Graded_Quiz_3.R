
# Graded Quiz 3 ----

library(quantmod)

# Japanese Yen and US Dollars

getSymbols("DEXJPUS", src="FRED")
JPY <- na.omit(DEXJPUS)
JPY <- JPY["1979-12-31/2017-12-31"]
JPY <- 1/JPY
names(JPY) <- "TR"

# We then ran the following sequence of R commands.
# First, run the following R commands to see that the first observation is an “NA”:
logret <- diff(log(JPY))
head(logret,3)

# Second, run the following R commend to see that the “NA” in the first observation is removed:
logret <- diff(log(JPY))[-1]

# In the lectures, we used the “moments” package in R to estimate its skewness, kurtosis, and to
# perform the Jarque-Bera test of normality.
# Skewness: The coefficient of skewness is estimated using the “skewness” function in the “moments”
# package in R.
library(moments)

rvec <- as.vector(logret)

round(skewness(rvec),2)

# Kurtosis: The coefficient of kurtosis is estimated using the “kurtosis” function in the “moments”
# package in R.
library(moments)

rvec <- as.vector(logret)

round(kurtosis(rvec),2)

# Jarque-Bera test of normality: The JB test of normality is performed using the “jarque.test” function in
# the “moments” package in R.
library(moments)

rvec <- as.vector(logret)

jarque.test(rvec)

# In the lectures, we used the “moments” package in R to estimate the parameters of the scaled
# student-t distribution:
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)
# Using the estimated parameters, we estimated the VaR and ES at the 95% confidence level:
alpha <- 0.01
RNGkind(sample.kind = "Rounding")

set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)

# In the lectures, we used 3 simulation methods to find the VaR and ES at the 95% confidence
# level for a 10-day horizon.
# Simulation Method 1: ----
#   used the “moments” package in R to estimate the parameters of the scaled student-t
# distribution:
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)
# To estimate the VaR and ES at the 95% confidence interval for a 10-day horizon, we simulated
# ten 1-day outcomes and add them up. Repeat 100,000 times. Then calculate the VaR and ES for
# these 100,000 outcomes.
alpha <- 0.01
RNGkind(sample.kind = "Rounding")
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
}
VaR_1 <- quantile(rvec,alpha)
ES_1 <- mean(rvec[rvec<VaR_1])

round(VaR_1,6)
round(ES_1,6)

# Simulation Method 2: ----
#   Instead of simulating from the scaled student-t distribution, the second simulation method
# randomly samples ten 1-day observations from the empirical distribution and add them up.
# Repeat 100,000 times. Then calculate the VaR and ES for these 100,000 outcomes.
alpha <- 0.01
RNGkind(sample.kind = "Rounding")
set.seed(123789)
rvec <- rep(0,100000)
for (i in 1:10) {
  rvec <- rvec+ sample(as.vector(logret),100000,replace=TRUE)
}
VaR_2 <- quantile(rvec,alpha)
ES_2 <- mean(rvec[rvec<VaR_2])

round(VaR_2,6)
round(ES_2,6)
# Simulation Method 3: ----
#   The third simulation method also samples from the empirical distribution, but draws blocks of
# ten consecutive 1-day outcomes and add them up. Repeat 100,000 times. Then calculate the
# VaR and ES for these 100,000 outcomes.
alpha <- 0.01
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
VaR_3 <- quantile(rvec,alpha)
ES_3 <- mean(rvec[rvec<VaR_3])

round(VaR_3,6)
round(ES_3,6)
