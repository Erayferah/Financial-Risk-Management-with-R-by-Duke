
# Graded Quiz 2 ----

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
# Answers to quiz questions

# estimate the two parameters of the normal distribution: mean and standard deviation.
round(mean(logret), 6)
round(sd(logret), 6)

mu <- mean(logret)
sig <- sd(logret)

# The VaR at the 99% confidence level for the daily log returns can be calculated using the
# estimated mean (mu) and estimated standard deviation (sig):
var <- qnorm(0.01,mu,sig)

round(var, 6)

# In the lecture, we used an example of a hedge fund investing $1000 million in US equities. We
# can now find the VaR of the daily change in its assets, at the 99% confidence level, using the
# following R command:

HFvar <- 1000 * (exp(var) - 1) # in millions of dollars

round(HFvar, 6)

# The ES at the 95% confidence level for the daily log returns can be calculated using the
# estimated mean (mu) and estimated standard deviation (sig):
es <- mu-sig*dnorm(qnorm(0.01,0,1),0,1)/0.01

# In the lecture, we used an example of a hedge fund investing $1000 million in US equities. We
# can now find the ES of the daily change in its assets, at the 95% confidence level, using the
# following R command:
round(es, 6)

# In the lecture, we used an example of a hedge fund investing $1000 million in US equities. We
# can now find the VaR of the daily change in its assets, at the 95% confidence level, using the
# following R command:
HFvar <- 1000 * ( exp(es)-1 ) # in millions of dollars

round(HFvar, 6)

# Using these estimated parameters, we simulated from the normal distribution with mean mu
# and standard deviation sig for 100,000 outcomes, using the following R command:

RNGkind(sample.kind="Rounding")
set.seed(123789)

rvec <- rnorm(100000,mu,sig)

# Note: we set the seed value here to allow us to reproduce the same result each time. In actual
# practice, we will not set the seed to a given number.
# The VaR at the 99% confidence level is the 1% quantile of these 100,000 outcomes. We used
# the following R command to find VaR:

VaR_1 <- quantile(rvec,0.01)

round(VaR_1, 6)

# The ES as the 99% confidence level is the average of these 100,000 outcomes that are worse
# than the VaR. We used the following R command to find ES:

ES_1 <- mean(rvec[rvec<VaR_1])

round(ES_1, 6)

# Simulation method 2:
#   The second simulation method does not assume that daily log returns are normally distributed.
# Instead, we draw 100,000 outcomes from the empirical distribution, with replacement, using
# the following R command:

RNGkind(sample.kind="Rounding")
set.seed(123789)
rvec <- sample(as.vector(logret),100000,replace=TRUE)

# Note: again, we set the seed value here to allow us to reproduce the same result each time. In
# actual practice, we will not set the seed to a given number.
# The VaR at the 99% confidence level is the 1% quantile of these 100,000 outcomes. We used
# the following R command to find VaR:

VaR_2 <- quantile(rvec,0.01)

round(VaR_2, 6)

# The ES as the 99% confidence level is the average of these 100,000 outcomes that are worse
# than the VaR. We used the following R command to find ES:

ES_2 <- mean(rvec[rvec<VaR_2])

round(ES_2, 6)

HFvar <- 1000 * (exp(ES_2) - 1) # in millions of dollars

round(HFvar, 6)

