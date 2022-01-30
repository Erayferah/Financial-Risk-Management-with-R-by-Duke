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

# The ES at the 95% confidence level for the daily log returns can be calculated using the
# estimated mean (mu) and estimated standard deviation (sig):
es <- mu-sig*dnorm(qnorm(0.05,0,1),0,1)/0.05

# In the lecture, we used an example of a hedge fund investing $1000 million in US equities. We
# can now find the ES of the daily change in its assets, at the 95% confidence level, using the
# following R command:
round(es, 6)

# In the lecture, we used an example of a hedge fund investing $1000 million in US equities. We
# can now find the VaR of the daily change in its assets, at the 95% confidence level, using the
# following R command:
HFvar <- 1000 * ( exp(es)-1 ) # in millions of dollars

round(HFvar, 6)
