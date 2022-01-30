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

mu <- mean(logret)
sig <- sd(logret)

# Using these estimated parameters, we simulated from the normal distribution with mean mu
# and standard deviation sig for 100,000 outcomes, using the following R command:
set.seed(123789)

rvec <- rnorm(100000,mu,sig)

# Note: we set the seed value here to allow us to reproduce the same result each time. In actual
# practice, we will not set the seed to a given number.
# The VaR at the 95% confidence level is the 5% quantile of these 100,000 outcomes. We used
# the following R command to find VaR:

VaR_1 <- quantile(rvec,0.05)

round(VaR_1, 6)

# The ES as the 95% confidence level is the average of these 100,000 outcomes that are worse
# than the VaR. We used the following R command to find ES:

ES_1 <- mean(rvec[rvec<VaR_1])

round(ES_1, 6)

# Simulation method 2:
#   The second simulation method does not assume that daily log returns are normally distributed.
# Instead, we draw 100,000 outcomes from the empirical distribution, with replacement, using
# the following R command:

set.seed(123789)
rvec <- sample(as.vector(logret),100000,replace=TRUE)

# Note: again, we set the seed value here to allow us to reproduce the same result each time. In
# actual practice, we will not set the seed to a given number.
# The VaR at the 95% confidence level is the 5% quantile of these 100,000 outcomes. We used
# the following R command to find VaR:

VaR_2 <- quantile(rvec,0.05)

round(VaR_2, 6)

# The ES as the 95% confidence level is the average of these 100,000 outcomes that are worse
# than the VaR. We used the following R command to find ES:

ES_2 <- mean(rvec[rvec<VaR_2])

round(ES_2, 6)


