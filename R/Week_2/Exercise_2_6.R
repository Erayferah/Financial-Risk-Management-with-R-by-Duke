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
