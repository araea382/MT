library(MSwM2)
library(ggplot2)
library(gridExtra)

# one test case per SW
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

###############
# g2 L16B
# 3 states
###############
mod_L16B <- lm(fmla, data=train_g2_L16B)
summary(mod_L16B)

# plot with state area
gen <- function(object,data){
    state <- sapply(1:nrow(data), function(x) which.max(object@Fit@smoProb[x,]))
    state <- factor(state)
    index=seq(1,nrow(data))
    xmin=index-0.5
    xmax=index+0.5
    y=data$TotCpu
    ans <- data.frame(index,xmin,xmax,state,y=y,ymin=min(y),ymax=max(y))
    return(ans)
}

# (4) NYY
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5)] <- FALSE; switch
set.seed(1)
mswm_L16B_NYY <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_NYY)

plotDiag(mswm_L16B_NYY, which=1)
plotDiag(mswm_L16B_NYY, which=2)
plotDiag(mswm_L16B_NYY, which=3)

plotDiag(mswm_L16B_NYY, regime=1)
plotDiag(mswm_L16B_NYY, regime=2)
plotDiag(mswm_L16B_NYY, regime=3)

residPooled <- apply(mswm_L16B_NYY["Fit"]["error"]*mswm_L16B_NYY["Fit"]["smoProb"][-1,],1,sum)
qqplot.data2(residPooled)

d <- data.frame(fit=train_g2_L16B$TotCpu[-1], residPooled)
ggplot(data=d) + geom_point(aes(x=fit,y=residPooled)) + theme_bw() +
  xlab("Fitted value") + ggtitle("Residuals versus the Fitted values")

par(mfrow=c(2,1))
acf(residPooled,ylim=c(-1,1),main="ACF of Residuals")
pacf(residPooled,ylim=c(-1,1),main="PACF of Residuals")


L16B_NYY <- as.data.frame(mswm_L16B_NYY["Fit"]["error"])
p1 <- qqplot.data(L16B_NYY[,1],1)
p2 <- qqplot.data(L16B_NYY[,2],2)
p3 <- qqplot.data(L16B_NYY[,3],3)
grid.arrange(p1, p2, p3, ncol=3, nrow=1)

###############
# g2 L16A
# 3 states
###############
predictor2 <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Fdd.Tdd","NumCells")
fmla2 <- as.formula(paste("TotCpu ~ ", paste(predictor2, collapse= "+")))

mod_L16A <- lm(fmla2, data=train_g2_L16A)
summary(mod_L16A)

# (1) NN
switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(5,6,7)] <- FALSE; switch
set.seed(1)
mswm_L16A_NN <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_NN)

plotDiag(mswm_L16A_NN, which=1)
plotDiag(mswm_L16A_NN, which=2)
plotDiag(mswm_L16A_NN, which=3)

plotDiag(mswm_L16A_NN, regime=1)
plotDiag(mswm_L16A_NN, regime=2)
plotDiag(mswm_L16A_NN, regime=3)

plotDiag(mswm_L16A_NN, regime=1, which=3)
plotDiag(mswm_L16A_NN, regime=2, which=3)
plotDiag(mswm_L16A_NN, regime=3, which=3)

qqplot.data2 <- function(vec){
    # following four lines from base R's qqline()
    y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    d <- data.frame(resids = vec)
    ggplot(d, aes(sample=resids)) + stat_qq() + 
        geom_abline(slope=slope, intercept=int, color="red") +
        theme_bw() + ggtitle("Normal Q-Q Plot Pooled Residuals")
}

residPooled <- apply(mswm_L16A_NN["Fit"]["error"]*mswm_L16A_NN["Fit"]["smoProb"][-1,],1,sum)
qqplot.data2(residPooled)

d <- data.frame(fit=train_g2_L16A$TotCpu[-1], residPooled)
ggplot(data=d) + geom_point(aes(x=fit,y=residPooled)) + theme_bw() +
  xlab("Fitted value") + ggtitle("Residuals versus the Fitted values")

fit <- apply(mswm_L16A_NN["Fit"]["CondMean"]*mswm_L16A_NN["Fit"]["smoProb"][-1,],1,sum)
plot(x=fit, y=residPooled,ylab="Residuals")
plot(fit, ylab="Residuals")

par(mfrow=c(2,1))
acf(residPooled,ylim=c(-1,1),main="ACF of Residuals", lag.max=20)
pacf(residPooled,ylim=c(-1,1),main="PACF of Residuals")

qqplot.data <- function(vec, ind){
    # following four lines from base R's qqline()
    y <- quantile(vec[!is.na(vec)], c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y)/diff(x)
    int <- y[1L] - slope * x[1L]
    d <- data.frame(resids = vec)
    ggplot(d, aes(sample=resids)) + stat_qq() + 
        geom_abline(slope=slope, intercept=int, color="red") +
        theme_bw() + ggtitle(paste("State",ind))
}

L16A_NN <- as.data.frame(mswm_L16A_NN["Fit"]["error"])
p1 <- qqplot.data(L16A_NN[,1],1)
p2 <- qqplot.data(L16A_NN[,2],2)
p3 <- qqplot.data(L16A_NN[,3],3)
grid.arrange(p1, p2, p3, ncol=3, nrow=1)


par(mfrow=c(1,3))
L16A_NN <- as.data.frame(mswm_L16A_NN["Fit"]["error"])
qqnorm(mswm_L16A_NN["Fit"]["error"][,3],main=paste("Normal Q-Q Plot Regime ",i,sep=""))
qqline(mswm_L16A_NN["Fit"]["error"][,3],col=2,lwd=2)





###############
# g2 L17A
# 3 states
###############
mod_L17A <- lm(fmla, data=train_g2_L17A)
summary(mod_L17A)

# (1) NNN
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,6,7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_NNN <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_NNN)

plotDiag(mswm_L17A_NNN, which=1)
plotDiag(mswm_L17A_NNN, which=2)
plotDiag(mswm_L17A_NNN, which=3)

plotDiag(mswm_L17A_NNN, regime=1)
plotDiag(mswm_L17A_NNN, regime=2)
plotDiag(mswm_L17A_NNN, regime=3)

residPooled <- apply(mswm_L17A_NNN["Fit"]["error"]*mswm_L17A_NNN["Fit"]["smoProb"][-1,],1,sum)
qqplot.data2(residPooled)

d <- data.frame(fit=train_g2_L17A$TotCpu[-1], residPooled)
ggplot(data=d) + geom_point(aes(x=fit,y=residPooled)) + theme_bw() +
  xlab("Fitted value") + ggtitle("Residuals versus the Fitted values")

par(mfrow=c(2,1))
acf(residPooled,ylim=c(-1,1),main="ACF of Residuals")
pacf(residPooled,ylim=c(-1,1),main="PACF of Residuals")


L17A_NNN <- as.data.frame(mswm_L17A_NNN["Fit"]["error"])
p1 <- qqplot.data(L17A_NNN[,1],1)
p2 <- qqplot.data(L17A_NNN[,2],2)
p3 <- qqplot.data(L17A_NNN[,3],3)
grid.arrange(p1, p2, p3, ncol=3, nrow=1)
