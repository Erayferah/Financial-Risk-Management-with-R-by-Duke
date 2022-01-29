# Longer Horizon Returns of the Whilshire 5000 Index from FRED
# In the lectures, we ran the following R script to create a data series called “wilsh”:
library(quantmod)

getSymbols("WILL5000IND",src="FRED")
wilsh <- na.omit(WILL5000IND)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
# Next, we calculated its daily log returns:
logret <- diff(log(wilsh))[-1]
# We then used the following R commands to calculate longer horizon log returns:
logret.w <- apply.weekly(logret,sum)
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)
# From these series, we calculated longer horizon discrete returns:
ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1

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

logret.w <- apply.weekly(logret,sum)
logret.m <- apply.monthly(logret,sum)
logret.q <- apply.quarterly(logret,sum)
logret.y <- apply.yearly(logret,sum)

# Answers to the quiz
round(head(logret.w, 3), 6)
round(head(logret.m, 3), 6)


# Third, calculate the discrete returns using the log returns:
ret <- exp(logret) - 1
round(head(ret,3),6)

# From these series, we calculated longer horizon discrete returns:
ret.w <- exp(logret.w)-1
ret.m <- exp(logret.m)-1
ret.q <- exp(logret.q)-1
ret.y <- exp(logret.y)-1

# Answers to the quiz
round(head(ret.q, 3), 6)
round(tail(ret.y, 3), 6)
