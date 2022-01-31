
# Graded Quiz 4 ----

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

# In the lectures, we graphed the autocorrelation function of log returns using the R command:
acf(logret)
# Next we graphed the autocorrelation function of |log returns| using the R command:
acf( abs(logret) )
# To estimate the GARCH(1,1) –t model, we use the “rugarch” package in R:
library(rugarch)
uspec <- ugarchspec( variance.model = list(model = "sGARCH",garchOrder = c(1,1)),
                     mean.model = list(armaOrder = c(0,0), include.mean = TRUE),
                     distribution.model = "std")
fit.garch <- ugarchfit(spec = uspec, data = logret[,1])
# The estimated parameters are in
fit.garch@fit$coef

round(fit.garch@fit$coef[3],6)
round(fit.garch@fit$coef[4],6)
round(fit.garch@fit$coef[5],6)

# The output of the estimation are then saved:
save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "logret", "s", "z" )
# We then examine the acf of the “z” column to check if the GARCH model has captured volatility
# clustering in the data.
acf(save1$z)
acf(abs(save1$z))

# We use the R function “ugarchboot” to simulate 1-day outcomes:
RNGkind(sample.kind = "Rounding")
set.seed(123789) #set seed value
boot.garch <- ugarchboot(fit.garch,
                         method=c("Partial","Full")[1], # ignore parameter uncertainty
                         sampling="raw", # draw from standardized residuals
                         n.ahead=1, # 1-day ahead
                         n.bootpred=100000, # number of simulated outcomes
                         solver="solnp")
# The simulated outcomes are then saved in the vector “rvec”:
rvec <- boot.garch@fseries
# The VaR and ES at the 95% confidence level are calculated as before:
VaR <- quantile(rvec,0.05)
ES <- mean(rvec[rvec<VaR])
