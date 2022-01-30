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

# In the lectures, we used the “moments” package in R to estimate the parameters of the scaled
# student-t distribution:
library(MASS)
rvec <- as.vector(logret)
t.fit <- fitdistr(rvec, "t")
round(t.fit$estimate,6)
# Using the estimated parameters, we estimated the VaR and ES at the 95% confidence level:
alpha <- 0.05
RNGkind(sample.kind = "Rounding")

set.seed(123789)
library(metRology)
rvec <- rt.scaled(100000,mean=t.fit$estimate[1],sd=t.fit$estimate[2],df=t.fit$estimate[3])
VaR <- quantile(rvec,alpha)
ES <- mean(rvec[rvec<VaR])
round(VaR,6)
round(ES,6)
