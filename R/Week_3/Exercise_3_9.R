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
