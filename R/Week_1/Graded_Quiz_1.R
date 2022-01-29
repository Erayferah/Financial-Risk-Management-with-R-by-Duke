# Graded Quiz 1 ----

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
