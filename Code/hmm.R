library(depmixS4)
library(quantmod)
library(ggplot2)
library(gridExtra)
data(speed)

EURUSD1d <- read.csv("C:/Users/EARAEAM/Downloads/EURUSD1d.csv")
Date<-as.character(EURUSD1d[,1])
DateTS<- as.POSIXlt(Date, format = "%Y.%m.%d %H:%M:%S") #create date and time objects
TSData<-data.frame(EURUSD1d[,2:5],row.names=DateTS)
TSData<-as.xts(TSData) #build our time series data set
ATRindicator<-ATR(TSData[,2:4],n=14) #calculate the indicator
ATR<-ATRindicator[,2] #grab just the ATR
LogReturns <- log(EURUSD1d$Close) - log(EURUSD1d$Open) #calculate the logarithmic returns
ModelData<-data.frame(LogReturns,ATR) #create the data frame for our HMM model
ModelData<-ModelData[-c(1:14),] #remove the data where the indicators are being calculated
colnames(ModelData)<-c("LogReturns","ATR") #name our columns

set.seed(1)
HMM<-depmix(list(LogReturns~1,ATR~1),data=ModelData,nstates=3,family=list(gaussian(),gaussian())) #We're setting the LogReturns and ATR as our response variables, using the data frame we just built, want to set 3 different regimes, and setting the response distributions to be gaussian.
HMMfit<-fit(HMM, verbose = FALSE) #fit our model to the data set
print(HMMfit) #we can compare the log Likelihood as well as the AIC and BIC values to help choose our model
summary(HMMfit)

HMMpost<-posterior(HMMfit) #find the posterior odds for each state over our data set
head(HMMpost) #we can see that we now have the probability for each state for everyday as well as the highest probability class.


DFIndicators <- data.frame(DateTS, LogReturns, ATR); 
DFIndicatorsClean <- DFIndicators[-c(1:14), ]

Plot1Data<-data.frame(DFIndicatorsClean, HMMpost$state)

LogReturnsPlot<-ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,2]))+geom_line(color="darkblue")+labs(title="Log Returns",y="Log Returns",x="Date"); LogReturnsPlot
ATRPlot<-ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,3]))+geom_line(color="darkgreen")+labs(title="ATR(14)",y="ATR(14)",x="Date"); ATRPlot
RegimePlot<-ggplot(Plot1Data,aes(x=Plot1Data[,1],y=Plot1Data[,4]))+geom_line(color="red")+labs(title="Regime",y="Regime",x="Date"); RegimePlot

# The probability of each regime separately
Plot2Data<-data.frame("DateTS"=DFIndicatorsClean$DateTS, HMMpost)
Regime1Plot<-ggplot(Plot2Data,aes(x=Plot2Data[,1],y=Plot2Data[,3]))+geom_line(color="purple")+labs(title="Regime 1",y="Probability",x="Date")
Regime2Plot<-ggplot(Plot2Data,aes(x=Plot2Data[,1],y=Plot2Data[,4]))+geom_line(color="orange")+labs(title="Regime 2",y="Probability",x="Date")
Regime3Plot<-ggplot(Plot2Data,aes(x=Plot2Data[,1],y=Plot2Data[,5]))+geom_line(color="darkblue")+labs(title="Regime 3",y="Probability",x="Date")

# regime 3 tends to be times of high volatility and large magnitude moves, regime 2 is characterized by medium volatility, and regime 1 consists of low volatility.

#----------------------#
temp2 <- get_min(g2_L16B, "TotCpu%")
model2 <- depmix(`TotCpu%` ~ 1, data=temp2, nstates=3, family=gaussian())

set.seed(12345)
fitted <- fit(model2)
summary(fitted) 

prob <- posterior(fitted) # Compute probability of being in each state
head(prob) # we can see that we now have the probability for each state for everyday as well as the highest probability class.
# rowSums(head(prob)[,2:4]) # Check that probabilities sum to 1

dat <- data.frame(temp2, prob$state)
cpu <- ggplot(dat,aes(x=as.numeric(dat[,7]),y=dat[,14])) + geom_line(color="darkgreen") + labs(title="TotCpu%",y="TotCpu%",x="SW")
regime <- ggplot(dat,aes(x=as.numeric(dat[,7]),y=dat[,20])) + geom_line(color="red") + labs(title="Regime",y="Regime",x="SW")
grid.arrange(cpu, regime)

# The probability of each regime separately
dat2 <- data.frame(temp2, prob)
regime1 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,21])) + geom_line(color="purple") + labs(title="Regime 1",y="Probability",x="SW")
regime2 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,22])) + geom_line(color="orange") + labs(title="Regime 2",y="Probability",x="v")
regime3 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,23])) + geom_line(color="darkblue") + labs(title="Regime 3",y="Probability",x="SW")
grid.arrange(regime1, regime2, regime3)

layout(1:2)
plot(temp2$`TotCpu%`, type='l', main='Regime Detection', xlab='', ylab='cpu')
matplot(prob[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')

#----------------------------------------------------------------------#
library(quantmod)
library(depmixS4)
library(TTR)

fred.tickers <-c("INDPRO")
getSymbols(fred.tickers,src="FRED")

indpro.1yr <-na.omit(ROC(INDPRO,12))
indpro.1yr.df <-data.frame(indpro.1yr)

model <- depmix(response=INDPRO ~ 1, 
                family = gaussian(), 
                nstates = 2, 
                data = indpro.1yr.df ,
                transition=~1)

set.seed(1)
model.fit <- fit(model, verbose = FALSE)
model.prob <- posterior(model.fit)
prob.rec <-model.prob[,2]
prob.rec.dates <-xts(prob.rec,as.Date(index(indpro.1yr)),
                     order.by=as.Date(index(indpro.1yr)))
prob.rec.d <- data.frame(date=as.Date(index(indpro.1yr)), model.prob)

ggplot(prob.rec.d,aes(x=prob.rec.d[,1],y=prob.rec.d[,2])) + geom_line(color="red")


#----------------------------------------------------------------------#
# try depmixS4
library(depmixS4)
temp <- get_average(g2_L16B_filter,"TotCpu%")
model <- depmix(`TotCpu%` ~ 1, data=g2_L16B_filter, nstates=3)

set.seed(12345)
fit1 <- fit(model)
summary(fit1)
print(fit1)
# state 1 is the starting state for the process

prob <- posterior(fitted) # Compute probability of being in each state
head(prob)
rowSums(head(prob)[,2:4]) # Check that probabilities sum to 1

par(mfrow=c(2,1))
ts.plot(matrix(temp2$`TotCpu%`))
plot(1:nrow(prob), prob[,1], type='l')

dat <- temp
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


