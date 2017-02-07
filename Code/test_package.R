library(ecp)
library(cpm)

set.seed(250)
period1 <- rnorm(100)
period2 <- rnorm(100, 0, 3)
period3 <- rnorm(100, 2, 1)
period4 <- rnorm(100, 2, 4)
Xnorm <- matrix(c(period1, period2, period3, period4), ncol = 1)
output1 <- e.divisive(Xnorm, R = 499, alpha = 1)
output2 <- e.divisive(Xnorm, R = 499, alpha = 2)

output2$estimates
output1$k.hat
output1$order.found
output1$estimates
output1$considered.last
output1$p.values
output1$permutations

ts.plot(Xnorm, ylab = "Value", main = "Change in a Univariate Gaussian Sequence")
abline(v = c(101, 201, 301), col = "blue")
abline(v = output1$estimates[c(-1, -5)], col = "red", lty = 2)

member <- rep(1:40, each = 10)
output <- e.agglo(X = Xnorm, member = member, alpha = 1)
output$estimates
tail(output$fit, 5)
output$progression[1, 1:10]
output$merged[1:4, ]


set.seed(200)
library("mvtnorm")
mu <- rep(0, 3)
covA <- matrix(c(1, 0, 0, 0, 1, 0, 0, 0, 1), 3, 3)
covB <- matrix(c(1, 0.9, 0.9, 0.9, 1, 0.9, 0.9, 0.9, 1), 3, 3)
period1 <- rmvnorm(250, mu, covA)
period2 <- rmvnorm(250, mu, covB)
period3 <- rmvnorm(250, mu, covA)
Xcov <- rbind(period1, period2, period3)
DivOutput <- e.divisive(Xcov, R = 499, alpha = 1)
DivOutput$estimates

member <- rep(1:15, each = 50)
pen <- function(x) -length(x)
AggOutput1 <- e.agglo(X = Xcov, member = member, alpha = 1)
AggOutput2 <- e.agglo(X = Xcov, member = member, alpha = 1, penalty = pen)
AggOutput1$estimates
AggOutput2$estimates

ts.plot(Xcov, ylab = "Value", main = "Change in a Univariate Gaussian Sequence")


data("ACGH", package = "ecp")
acghData <- ACGH$data

#----------------------------------------------------------------------#
library(trend)
data(maxau)
s <- maxau[,"s"]; Q <- maxau[,"Q"]

plot(s)
plot(Q)
mk.test(s) # reject H0
mk.test(Q) # can't reject H0

plot(ts(nottem))
smk.test(nottem)
partial.mk.test(s,Q)
partial.cor.trend.test(s,Q, "spearman")

#----------------------------------------------------------------------#
library(strucchange) # change in regression
fs <- breakpoints(g2_L16B_new_min$Normalize ~ 1)

g <- g2_L16B_new_min
g$DuProdName <- as.factor(g$DuProdName)
g$`Fdd/Tdd` <- as.factor(g$`Fdd/Tdd`)
g$NumCells <- as.factor(g$NumCells)

fs <- breakpoints(Normalize ~ `Fdd/Tdd` + NumCells + DuProdName, data=g)
fm1 <- lm(g$Normalize ~ breakfactor(fs, breaks = 1))
lines(ts(fitted(fm1), start =0), col = 4)

fs <- breakpoints(g2_L16B_filter_min$`TotCpu%` ~ 1)


#----------------------------------------------------------------------#
library(tsoutliers)
dat.ts <- ts(g2_L16B_new_min$Normalize, frequency=1)
outlier <- tso(dat.ts); outlier
plot(outlier)
# it corrects but we do not want to detect outlier

dat.ts <- ts(g2_L16B_filter_min$`TotCpu%`, frequency=1)
outlier <- tso(dat.ts); outlier
plot(outlier)
# strange

#----------------------------------------------------------------------#
library(changepoint)
ansmeanvar=cpt.meanvar(g2_L16B_new_min$Normalize)
plot(ansmeanvar)
print(ansmeanvar)


#----------------------------------------------------------------------#
library(partykit)
data("treepipit", package = "coin")

