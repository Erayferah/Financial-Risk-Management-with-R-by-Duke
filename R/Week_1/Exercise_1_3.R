library(quantmod)

getSymbols("WILL5000IND", src="FRED")
wilsh <- na.omit(WILL5000IND)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"

# We then ran the following sequence of R commands.
# First, run the following R commands to see that the first observation is an “NA”:
logret <- diff(log(wilsh))
head(logret,3)

# Second, run the following R commend to see that the “NA” in the first observation is removed:
logret <- diff(log(wilsh))[-1]
round(head(logret,3),6)

# Third, calculate the discrete returns using the log returns:
ret <- exp(logret) - 1
round(head(ret,3),6)

# GOLD Prices -----------------------------------------------------------------

getSymbols("GOLDPMGBD228NLBM", src="FRED")
gold <- na.omit(GOLDPMGBD228NLBM)
gold <- gold["1979-12-31/2017-12-31"]
names(gold) <- "TR"

# We then ran the following sequence of R commands.
# First, run the following R commands to see that the first observation is an “NA”:
logret <- diff(log(gold))
head(logret,3)

# Second, run the following R commend to see that the “NA” in the first observation is removed:
logret <- diff(log(gold))[-1]
# Answers to quiz questions
round(head(logret,3),6)
round(tail(logret,3),6)

# Third, calculate the discrete returns using the log returns:
ret <- exp(logret) - 1
round(head(ret,3),6)














