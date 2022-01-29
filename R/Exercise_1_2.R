library(quantmod)

wilsh <- getSymbols("WILL5000IND", src = "FRED", auto.assign = FALSE)
wilsh <- na.omit(wilsh)
wilsh <- wilsh["1979-12-31/2017-12-31"]
names(wilsh) <- "TR"
head(wilsh, 3)
tail(wilsh, 3)

gold <- getSymbols("GOLDPMGBD228NLBM", src = "FRED", auto.assign = FALSE)
gold <- na.omit(gold)
gold <- gold["1979-12-31/2017-12-31"]
names(gold) <- "TR"
head(gold,3)
tail(gold,3)
