library(depmixS4)
library(ggplot2)
library(gridExtra)

model2 <- depmix(`TotCpu%` ~ 1, data=g2_L16B_min, nstates=3, family=gaussian())

set.seed(12345)
fitted <- fit(model2)
summary(fitted) 

prob <- posterior(fitted) # Compute probability of being in each state
head(prob) # we can see that we now have the probability for each state for everyday as well as the highest probability class.
# rowSums(head(prob)[,2:4]) # Check that probabilities sum to 1

dat <- data.frame(g2_L16B_min, prob$state)
cpu <- ggplot(dat,aes(x=as.numeric(dat[,7]),y=dat[,14])) + geom_line(color="darkgreen") + labs(title="TotCpu%",y="TotCpu%",x="SW")
regime <- ggplot(dat,aes(x=as.numeric(dat[,7]),y=dat[,20])) + geom_line(color="red") + labs(title="Regime",y="Regime",x="SW")
grid.arrange(cpu, regime)

# The probability of each regime separately
dat2 <- data.frame(g2_L16B_min, prob)
regime1 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,21])) + geom_line(color="purple") + labs(title="Regime 1",y="Probability",x="SW")
regime2 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,22])) + geom_line(color="orange") + labs(title="Regime 2",y="Probability",x="v")
regime3 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,23])) + geom_line(color="darkblue") + labs(title="Regime 3",y="Probability",x="SW")
grid.arrange(regime1, regime2, regime3)

layout(1:2)
plot(g2_L16B_min$`TotCpu%`, type='l', main='Regime Detection', xlab='', ylab='cpu')
matplot(prob[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')


#----------------------------------------------------------------------#
model <- depmix(`TotCpu%` ~ 1, data=g2_L16B_filter_min, nstates=3)

set.seed(12345)
fit1 <- fit(model)
summary(fit1)
print(fit1)
# state 1 is the starting state for the process

prob <- posterior(fit1) # Compute probability of being in each state
head(prob)
rowSums(head(prob)[,2:4]) # Check that probabilities sum to 1

par(mfrow=c(2,1))
ts.plot(matrix(g2_L16B_min$`TotCpu%`))
plot(1:nrow(prob), prob[,1], type='l')

dat <- data=g2_L16B_filter_min
p <- prob[,2] # Pick out the first state
dat$p <- p # Put it in the data frame for plotting

# Pick out an interesting subset of the data or plotting and
# reshape the data in a form convenient for ggplot
dat2 <- melt(dat[,c(7,14,18)],id="SW",measure=c("TotCpu%","p"))
dat2 <- melt(dat[,1:3],id="SW",measure=c("value","p"))
#head(df)

# Plot the log return time series along withe the time series of probabilities
qplot(SW,value,data=dat2,geom="line", ylab = "") + facet_grid(variable ~ ., scales="free_y")

#----------------------#


