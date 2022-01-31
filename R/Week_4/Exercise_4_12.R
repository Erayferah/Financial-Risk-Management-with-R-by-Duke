library(quantmod)

# Exercise ----

getSymbols("GOLDPMGBD228NLBM",src="FRED")
gold <- na.omit(GOLDPMGBD228NLBM)
gold <- gold["1979-12-31/2017-12-31"]
names(gold) <- "TR"
# Next, we calculated its daily log returns:
logret <- diff(log(gold))[-1]

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
# The output of the estimation are then saved:
save1 <- cbind( logret[,1], fit.garch@fit$sigma, fit.garch@fit$z )
names(save1) <- c( "logret", "s", "z" )
# We then examine the acf of the “z” column to check if the GARCH model has captured volatility
# clustering in the data.
acf(save1$z)
acf(abs(save1$z))
