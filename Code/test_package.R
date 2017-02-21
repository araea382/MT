#----------------------------------------------------------------------#
# Change point detection/ Anomaly detection
#----------------------------------------------------------------------#
library(ecp)

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
# No, we do not want to detect trend

#----------------------------------------------------------------------#
# cpm package
# g2 FILTER data L16B
# batch detection
resultsStudent <- detectChangePointBatch(g2_L16B_filter$`TotCpu%`, cpmType = "Student", alpha = 0.05)
resultsMW <- detectChangePointBatch(g2_L16B_filter$`TotCpu%`, cpmType = "Mann-Whitney", alpha = 0.05)
plot(g2_L16B_filter$`TotCpu%`, type = "l", xlab = "Observation", ylab = "x", bty = "l")
if (resultsStudent$changeDetected)
    abline(v = resultsStudent$changePoint, col="red", lty = 2) # student
if (resultsMW$changeDetected)
  abline(v = resultsMW$changePoint, col="blue", lty = 2) # mann-whitney
  
plot(resultsStudent$Ds, type = "l", xlab = "Observation", ylab = expression(D[t]), bty = "l")
abline(h = resultsStudent$threshold, lty = 2)

plot(resultsMW$Ds, type = "l", xlab = "Observation", ylab = expression(D[t]), bty = "l")
abline(h = resultsMW$threshold, lty = 2)

# sequential change detection
resultsStudent <- detectChangePoint(g2_L16B_filter$`TotCpu%`, cpmType = "Student", ARL0 = 500)
resultsMW <- detectChangePoint(g2_L16B_filter$`TotCpu%`, cpmType = "Mann-Whitney", ARL0 = 500)
plot(g2_L16B_filter$`TotCpu%`, type = "l", bty = "l")
if (resultsStudent$changeDetected)
  abline(v = resultsStudent$detectionTime, col = "red") # student
if (resultsMW$changeDetected)
  abline(v = resultsMW$detectionTime, col = "blue") # mann-whitney

# sequences containing multiple change points
res <- processStream(g2_L16B_filter$`TotCpu%`, cpmType = "Mann-Whitney", ARL0 = 500, startup = 20)
plot(g2_L16B_filter$`TotCpu%`, type = "l", xlab = "Observation", ylab = "", bty = "l")
abline(v = res$detectionTimes) # change was detected
abline(v = res$changePoints, lty = 2) # estimated change point locations


#----------------------------------------------------------------------#
# BreakoutDetection
# EDM: E-divisive median
library(BreakoutDetection)
  
data(Scribe)
res = breakout(Scribe, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot
plot(Scribe, type="l")
  
Ediv_scribe <- e.divisive(matrix(Scribe), R=499, alpha=1)
ts.plot(matrix(Scribe))
abline(v=Ediv_scribe$estimates[c(-1,-4)], col="red", lty=2)
  
res2 <- breakout(g2_L16B_new_min$Normalize, method="multi", plot=TRUE)
res2$plot

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
# strange # do not understand the graph ....

#----------------------------------------------------------------------#
library(changepoint)
ansmeanvar=cpt.meanvar(g2_L16B_new_min$Normalize) # mean and variance
plot(ansmeanvar)
print(ansmeanvar)


#----------------------------------------------------------------------#
library(quantmod)
p <- findPeaks(g2_L16B_new_min$Normalize)
plot(g2_L16B_new_min$Normalize, type="l")
points(p, g2_L16B_new_min$Normalize[p])

p <- findPeaks(g2_L16B_filter_min$`TotCpu%`)
plot(g2_L16B_filter_min$`TotCpu%`, type="l")
points(p, g2_L16B_filter_min$`TotCpu%`[p])

p2 <- find_peaks(g2_L16B_new_min$Normalize)
plot(g2_L16B_new_min$Normalize, type="l")
points(p2, g2_L16B_new_min$Normalize[p2])


#----------------------------------------------------------------------#
library(peakPick)
peakhits <- peakpick(g2_L16B_new_min$Normalize, 100)
plot(g2_L16B_new_min$Normalize, type="l")
points((1:length(g2_L16B_new_min))[peakhits], g2_L16B_new_min$Normalize[peakhits], col="red")

spikes <- detect.spikes(matrix(g2_L16B_new_min$Normalize), c(2,nrow(g2_L16B_new_min)-1), 1)


#----------------------------------------------------------------------#
source("C:/Users/EARAEAM/Desktop/MT/Code/peakdet.R")
p <- peakdet(g2_L16B_new_min$Normalize, delta=0.5, c(1:nrow(g2_L16B_new_min))) # small delta more detect
plot(g2_L16B_new_min$Normalize, type="l")
points(p$maxtab[,1],p$maxtab[,2], col="red", pch=16)
points(p$mintab[,1],p$mintab[,2], col="blue", pch=16)

#----------------------------------------------------------------------#
library(wmtsa)






#----------------------------------------------------------------------#
library(qcc)
q <- qcc(g2_L16B_filter_min$`TotCpu%`, type="xbar.one")




#----------------------------------------------------------------------#
library(AnomalyDetection)
data(raw_data)
g <- g2_L16B_min[,c("SW","TotCpu%")]
g$SW <- as.numeric(g$SW)
g$SW <- as.POSIXlt(g$SW, origin="1960-10-01")
res = AnomalyDetectionTs(g, max_anoms=0.02, direction='both', plot=TRUE)
res$plot
# need to change to POSIXlt or timestamp which is not make sense in this case

res2 <- AnomalyDetectionVec(g2_L16B$`TotCpu%`, max_anoms=0.02, period=50, direction='both', plot=TRUE)
res2$plot
# can not get it to work

#----------------------------------------------------------------------#
# Hidden markov model
#----------------------------------------------------------------------#
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

#----------------------------------------------------------------------#
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
# Autoregressive hidden markov model
#----------------------------------------------------------------------#
library(MSwM)
data(example)
mod=lm(y~x,example)
summary(mod)
acf(resid(mod))
mod.mswm=msmFit(mod,k=2,p=1,sw=c(T,T,T,T),control=list(parallel=F))
summary(mod.mswm)

# data(energy)
# model=lm(Price~Oil+Gas+Coal+EurDol+Ibex35+Demand,energy)
# mod=msmFit(model,k=2,sw=rep(TRUE,8))
# summary(mod)

plot(msmResid(mod.mswm), type="l")

plotDiag(mod.mswm, which=1)
plotDiag(mod.mswm, which=2)
plotDiag(mod.mswm, which=3)

plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)
plotReg(mod.mswm,expl="x")

data(traffic)
model=glm(NDead~Temp+Prec,traffic,family="poisson")
summary(model)

m1=msmFit(model,k=2,sw=c(T,T,T),family="poisson",control=list(parallel=F))
summary(m1)
plotDiag(m1, which=1)
plotDiag(m1, which=2)
plotDiag(m1, which=3)

plotProb(m1,which=2)
plotReg(m1)

#----------------------#
df1 <- g2_L16B_filter_min
colnames(df1)[14] <- "TotCpu" # need to rename the variable
mod1 <- lm(TotCpu~1, data=df1)
summary(mod1)

model_mswm <- msmFit(mod1, k=3, p=1, sw=rep(TRUE,3), control=list(parallel=F)) # variable + p + 1
summary(model_mswm)

plot(msmResid(model_mswm), type="l")
acf(msmResid(model_mswm))

plotDiag(model_mswm, which=1)
plotDiag(model_mswm, which=2)
plotDiag(model_mswm, which=3)

plotProb(model_mswm, which=1)
plotProb(model_mswm, which=2)
plotProb(model_mswm, which=3)
plotProb(model_mswm, which=4)

plotReg(model_mswm, regime=1)
plotReg(model_mswm, regime=2)
plotReg(model_mswm, regime=3)

library(TSA)
set.seed(12345)
ar <- arima(df1$TotCpu, order=c(1,0,0))
ar$coef

#----------------------#
# one test case per SW
g2_L16B_min <- get_min(g2_L16B, "TotCpu%")
# g2_L16B_min_extract <- extract_component(g2_L16B_min)
# g2_L16B_max <- get_max(g2_L16B, "TotCpu%")
# g2_L16B_max_extract <- extract_component(g2_L16B_max)
df2 <- g2_L16B_min
colnames(df2)[14] <- "TotCpu" # need to rename the variable

mod2 <- lm(TotCpu~1, data=df2)
summary(mod2)

set.seed(12345)
model_mswm2 <- msmFit(mod2, k=3, p=1, sw=rep(TRUE,length(mod2$coefficients)+2), control=list(parallel=F))
summary(model_mswm2)

plot(msmResid(model_mswm2), type="l")
acf(msmResid(model_mswm2))

plotDiag(model_mswm2, which=1)
plotDiag(model_mswm2, which=2)
plotDiag(model_mswm2, which=3)

plotProb(model_mswm2, which=1)
plotProb(model_mswm2, which=2)
plotProb(model_mswm2, which=3)
plotProb(model_mswm2, which=4)

plotReg(model_mswm2, regime=2)
plotReg(model_mswm2, regime=3)

#----------------------#
mod3 <- lm(TotCpu~RrcConnectionSetupComplete+X2HandoverRequest, data=df2)
summary(mod3)

set.seed(12345)
model_mswm3 <- msmFit(mod3, k=3, p=1, sw=rep(TRUE,5), control=list(parallel=F))
summary(model_mswm3)

plot(msmResid(model_mswm3), type="l")
acf(msmResid(model_mswm3))

plotDiag(model_mswm3, which=1)
plotDiag(model_mswm3, which=2)
plotDiag(model_mswm3, which=3)

plotProb(model_mswm3, which=1)
plotProb(model_mswm3, which=2)
plotProb(model_mswm3, which=3)
plotProb(model_mswm3, which=4)

plotReg(model_mswm3, expl="RrcConnectionSetupComplete")
plotReg(model_mswm3, regime=2)
plotReg(model_mswm3, regime=3)

#----------------------#
# can't use lasso from glmnet() to model in msmFit()
y1 <- as.matrix(df2$TotCpu)
X1 <- as.matrix(subset(df2, select=c(18:ncol(df2))))

set.seed(12345)
lasso_cv1 <- cv.glmnet(X1, y1, alpha=1, family = "gaussian")
plot(lasso_cv1)
penalty1 <- lasso_cv1$lambda.min
fit_lasso1 <- glmnet(X1, y1, alpha=1, lambda=penalty1) 
coef(fit_lasso1)

# model_mswm3 <- msmFit(fit_lasso1, k=3, p=1, sw=rep(TRUE,106), control=list(parallel=F))
# summary(model_mswm3)

#----------------------#
predictor <- c("ErabDrbAllocated","ErabDrbRelease","ErabReleaseInfo","PerBbRbEvent","PerBbUeEventTa","RrcConnectionReconfiguration","RrcUlInformationTransfer","ProcRrcUeCapabilityEnquiry","RrcUeCapabilityEnquiry") # varaible selection from fit_lasso1 # missing value where TRUE/FALSE needed
predictor <- c("RrcConnectionReconfiguration","RrcConnectionReconfigurationComplete","ErabDrbAllocated","ErabDrbRelease") # variable important from randomforest
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","S1InitialUeMessage","PerBbUeEvent","ErabDrbRelease") # can't remember 
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","S1InitialUeMessage","ReEstablishmentAttempt") 
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","S1InitialUeMessage") 

fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))
mod3 <- lm(fmla, data=df2)
summary(mod3)

# df3 <- subset(df2, select=c(14,18:length(df2)))
# df3 <- add_x_name(df3)
# mod3 <- lm(TotCpu ~ ., data=df3)

# fmla <- as.formula(paste("`TotCpu%` ~ ", paste(predictor, collapse= "+")))
# mod3 <- lm(fmla, data=temp3)

set.seed(123)
model_mswm3 <- msmFit(mod3, k=3, p=1, sw=rep(TRUE,length(mod3$coefficients)+2), control=list(parallel=F))
summary(model_mswm3)

# mod3 <- lm(`TotCpu`~., data=df3)
# step3 <- stepAIC(mod3, direction="both")
# model_mswm3 <- msmFit(step3, k=3, p=1, sw=rep(TRUE,length(step3$coefficients)+2), control=list(parallel=F))
# summary(model_mswm3)

plot(msmResid(model_mswm3), type="l")
acf(msmResid(model_mswm3))

plotDiag(model_mswm3, which=1)
plotDiag(model_mswm3, which=2)
plotDiag(model_mswm3, which=3)

plotProb(model_mswm3, which=1)
plotProb(model_mswm3, which=2)
plotProb(model_mswm3, which=3)
plotProb(model_mswm3, which=4)

plotReg(model_mswm3, expl="RrcConnectionSetupComplete")
plotReg(model_mswm3, regime=2)
plotReg(model_mswm3, regime=3)

#----------------------------------------------------------------------#
library(NHMSAR)
data(meteo.data)
data = array(meteo.data$precipitation,c(31,41,1))
k = 40
T = dim(data)[1]
N.samples = dim(data)[2]
d = dim(data)[3]
M = 2
order = 2
theta.init = init.theta.MSAR(data,M=M,order=order,label="HH")
mod.hh = fit.MSAR(data,theta.init,verbose=TRUE,MaxIter=20)
regimes.plot.MSAR(mod.hh,data,ylab="temperatures")
#Y0 = array(data[1:2,sample(1:dim(data)[2],1),],c(2,1,1))
#Y.sim = simule.nh.MSAR(mod.hh$theta,Y0 = Y0,T,N.samples = 1)

# Fit Non Homogeneous MS-AR models
# theta.init = init.theta.MSAR(data,M=M,order=order,label="NH",nh.transitions="gauss")
# attributes(theta.init)
# mod.nh = fit.MSAR(array(data[2:T,,],c(T-1,N.samples,1)),theta.init,verbose=TRUE,MaxIter=50,
# covar.trans=array(data[1:(T-1),,],c(T-1,N.samples,1)))
# regimes.plot.MSAR(mod.nh,data,ex=40,ylab="temperature (deg. C)")

# Fit Non Homogeneous MS-AR models - univariate time series
data(lynx)
T = length(lynx)
data = array(log10(lynx),c(T,1,1))
theta.init = init.theta.MSAR(data,M=2,order=2,label="HH")
mod.lynx.hh = fit.MSAR(data,theta.init,verbose=TRUE,MaxIter=200)
regimes.plot.MSAR(mod.lynx.hh,data,ylab="Captures number")
r <- regimes.plot.MSAR(mod.lynx.hh,data,ylab="Captures number")

plot(log10(lynx), type="l")
plot(mod.lynx.hh$smoothedprob[,1], type="l", lty="dashed", col="red", ylim=c(0,1))
lines(mod.lynx.hh$smoothedprob[,2], lty="dotted", col="blue")

dat <- as.data.frame(mod.lynx.hh$smoothedprob)
dat$regime <- 0
dat$regime <- apply(dat, 1, function(x){
  if(x[1] > x[2]){
    x[3] <- 1
  }else{
    x[x] <- 2
  }
})

#----------------------#
t <- nrow(g2_L16B_filter_min)
dat <- array(g2_L16B_filter_min$`TotCpu%`, c(t,1,1))
theta_init <- init.theta.MSAR(dat, M=3, order=1, label="HH")
mod_hh <- fit.MSAR(dat, theta_init, verbose=TRUE, MaxIter=200)
regimes.plot.MSAR(mod_hh, dat, ylab="Captures number")


g2_L16B_filter_min_extract <- extract_component(g2_L16B_filter_min)
dat <- array(c(g2_L16B_filter_min_extract$`TotCpu%`, g2_L16B_filter_min_extract$RrcConnectionSetupComplete), dim=c(t,2,1))
theta_init <- init.theta.MSAR(dat, M=3, order=1, label="HH")
mod_hh <- fit.MSAR(dat, theta_init, verbose=TRUE, MaxIter=200)
regimes.plot.MSAR(mod_hh, dat, ylab="Captures number")


#----------------------------------------------------------------------#
# Decision tree
#----------------------------------------------------------------------#
library(partykit)
data("treepipit", package = "coin")

#----------------------------------------------------------------------#
library(rpart)
tree <- rpart(`TotCpu%`~., method="anova", data=temp2)

printcp(tree) # display the results 
plotcp(tree) # visualize cross-validation results 
# summary(tree) # detailed summary of splits

plot(tree, uniform=TRUE)
text(tree, use.n=TRUE, all=TRUE, cex=.6)

#----------------------------------------------------------------------#
# try with whole dataset of g2_L16B
library(randomForest)
# random forest can't deal with column name that begin with number
# add "X" in front of it
add_x_name <- function(data){
  nn <- unlist(lapply(colnames(data), function(x){
    if(substr(x,1,1) == "0"){
      x <- paste0("X",x)
    }else{
      x <- x
    }
  }))
  colnames(data) <- nn
  return(data)
}

temp3 <- add_x_name(temp2)
dum1 <- subset(g2_extract, select=c(14,18:length(g2_extract)))
dum1 <- add_x_name(dum1)

set.seed(12345)
random_tree <- randomForest(`TotCpu%`~., data=temp3, importance=TRUE, proximity=TRUE)
print(random_tree) # view results 
var_imp <- importance(random_tree) # importance of each predictor
var_imp[order(var_imp[,1], decreasing=TRUE),]
varImpPlot(random_tree, sort=T, n.var=5)
plot(random_tree)


var.imp <- data.frame(importance(random_tree, type=2))
# make row names as columns
var.imp$Variables <- row.names(var.imp)
var.imp[order(var.imp[,1],decreasing = T),]
