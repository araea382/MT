library(MSwM2)

data(example)
model=lm(y~x,example)
summary(model)

set.seed(1)
mod.mswm2=MSwM2::msmFit(model,k=2,p=1,sw=c(T,T,T,T),control=list(trace=F,parallel=F))
summary(mod.mswm2)

# data(energy)
# model=lm(Price~Oil+Gas+Coal+EurDol+Ibex35+Demand,energy)
# mod=msmFit(model,k=2,sw=rep(TRUE,8))
# summary(mod)

plot(msmResid(mod.mswm), type="l")

plotDiag(mod.mswm)
plotDiag(mod.mswm, which=1)
plotDiag(mod.mswm, which=2)
plotDiag(mod.mswm, which=3)

plotProb(mod.mswm,which=1)
plotProb(mod.mswm,which=2)
plotProb(mod.mswm,which=3)

plotReg(mod.mswm,expl="x")
plotReg(mod.mswm,expl="x", regime=1) # same
plotReg(mod.mswm,expl="x", regime=2)
plotReg(mod.mswm, regime=2)
plotReg(mod.mswm)

#----------------------#
data("traffic")
model=glm(NDead~Temp+Prec,traffic,family="poisson")
m1=msmFit(model,k=2,sw=c(T,T,T),family="poisson",control=list(parallel=F))


#----------------------#
# one test case per SW
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest")
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

# # scale
# g2_L16B_scale <- subset(g2_L16B, select=c("TotCpu%",predictor))
# g2_L16B_scale <- as.data.frame(scale(g2_L16B_scale))
# train_g2_L16B_scale <- g2_L16B_scale[1:train_num,]
#
# colnames(train_g2_L16B_scale)[1] <- "TotCpu"
#
# mod2 <- lm(fmla, data=train_g2_L16B_scale)
# set.seed(10)
# model_mswm <- msmFit(mod2, k=3, p=1, sw=c(TRUE,TRUE,FALSE,TRUE,TRUE,TRUE), control=list(trace=FALSE, maxiter=500, parallel=FALSE))
# summary(model_mswm)
# it seems that scale or not scale is the same

#----------------------#
# g2 L16B
# 3 states
# switching in all coefficients
mod <- lm(fmla, data=train_g2_L16B)
summary(mod)

set.seed(1)
model_mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(model_mswm)

plot(msmResid(model_mswm), type="l")
acf(msmResid(model_mswm))

plot(model_mswm)

plotDiag(model_mswm, which=1)
plotDiag(model_mswm, which=2)
plotDiag(model_mswm, which=3)

plotProb(model_mswm, which=1)
plotProb(model_mswm, which=2)
plotProb(model_mswm, which=3)
plotProb(model_mswm, which=4)

# can't use with the categorical variables
# plotReg(model_mswm, expl=predictor[1], regime=1)
# plotReg(model_mswm, expl=predictor[2], regime=1)
# plotReg(model_mswm, expl=predictor[3], regime=1)

plotReg(model_mswm, expl=predictor[4], regime=1)
plotReg(model_mswm, expl=predictor[4], regime=2)
plotReg(model_mswm, expl=predictor[4], regime=3)

plotReg(model_mswm, expl=predictor[5], regime=1)
plotReg(model_mswm, expl=predictor[5], regime=2)
plotReg(model_mswm, expl=predictor[5], regime=3)

plotReg(model_mswm, expl=predictor[6], regime=1)
plotReg(model_mswm, expl=predictor[6], regime=2)
plotReg(model_mswm, expl=predictor[6], regime=3)

# predict
newdata <- test_g2_L16B[1,]
predict(model_mswm, newdata)

#----------------------#
# # can't use lasso from glmnet() to model in msmFit()
# y1 <- as.matrix(df2$TotCpu)
# X1 <- as.matrix(subset(df2, select=c(18:ncol(df2))))
#
# set.seed(12345)
# lasso_cv1 <- cv.glmnet(X1, y1, alpha=1, family = "gaussian")
# plot(lasso_cv1)
# penalty1 <- lasso_cv1$lambda.min
# fit_lasso1 <- glmnet(X1, y1, alpha=1, lambda=penalty1)
# coef(fit_lasso1)
#
# # model_mswm3 <- msmFit(fit_lasso1, k=3, p=1, sw=rep(TRUE,106), control=list(parallel=F))
# # summary(model_mswm3)

#----------------------#
# predictor <- c("RrcConnectionReconfiguration","RrcConnectionReconfigurationComplete","ErabDrbAllocated","ErabDrbRelease") # variable important from randomforest
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","S1InitialUeMessage","PerBbUeEvent","ErabDrbRelease") # can't remember
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","S1InitialUeMessage","ReEstablishmentAttempt")
# S1InitialUeMessage too correlated with RrcConnectionSetupComplete (cor=1)

# df3 <- subset(df2, select=c(14,18:length(df2)))
# df3 <- add_x_name(df3)
# mod3 <- lm(TotCpu ~ ., data=df3)
#
# set.seed(123)
# model_mswm3 <- msmFit(mod3, k=3, p=1, sw=rep(TRUE,length(mod3$coefficients)+2), control=list(parallel=F))
# summary(model_mswm3)

# mod3 <- lm(`TotCpu`~., data=df3)
# step3 <- stepAIC(mod3, direction="both")
# model_mswm3 <- msmFit(step3, k=3, p=1, sw=rep(TRUE,length(step3$coefficients)+2), control=list(parallel=F))
# summary(model_mswm3)

#----------------------#
# # g2 filter L16B
# # one test case per SW
# # divide train (80) test (20)
# train_filter_num <- floor(nrow(g2_L16B_filter_min) * 0.8)
# train_g2_L16B_filter_min <- g2_L16B_filter_min[1:train_filter_num,]
# test_g2_L16B_filter_min <- g2_L16B_filter_min[-c(1:train_filter_num),]
#
# colnames(train_g2_L16B_filter_min)[14] <- "TotCpu" # need to rename the variable
#
# predictor <- c("RrcConnectionSetupComplete")
# fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))
# mod_filter <- lm(fmla, data=train_g2_L16B_filter_min)
# summary(mod_filter)
#
# set.seed(12)
# model_mswm_filter <- msmFit(mod_filter, k=3, p=1, sw=rep(TRUE,length(mod_filter$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
# summary(model_mswm_filter)
#
# plotDiag(model_mswm_filter, which=1)
# plotDiag(model_mswm_filter, which=2)
# plotDiag(model_mswm_filter, which=3)
#
# plotProb(model_mswm_filter, which=1)
# plotProb(model_mswm_filter, which=2)
# plotProb(model_mswm_filter, which=3)
# plotProb(model_mswm_filter, which=4)
#
# plotReg(model_mswm_filter, expl=predictor[1], regime=1)

#----------------------#
# plot (applied from plotProb) between regime and ecp
par(mfrow=c(3,1))
ecp <- 153 # from ecp()
for(i in 1:3){
  z=model_mswm@model$model[1]
  y=model_mswm@Fit@smoProb[-1,i]

  par(las=1,yaxt="n")
  plot(0,type="l",xlim=c(1,length(t(z))),ylim=c(min(z),max(z)),main=paste("Regime",i),xlab=paste(names(z),"vs. Smooth Probabilities"),ylab="")
  val=cbind(which(diff(c(0,findInterval(y,0.5)))==1),which(diff(c(findInterval(y,0.5),0))==-1))
  apply(val,1,function(el) rect(el[1],min(z),el[2],max(z),col="light grey",border=NA))
  par(new=T,las=1,bty="o",yaxt="n")
  plot(ts(z),col=1,ylim=c(min(z),max(z)),xlab="",ylab="")
  par(las=3,yaxt="s")
  mtext(names(z),side=2,line=2.5,col=1)

  abline(v=ecp, lty="dashed", col="red")
}
par(mfrow=c(1,1))

#----------------------#
# # forecast from fMarkovSwitching package
# # new_data <- test_g2_L16B
# library(fMarkovSwitching)
# data(dep)
# data(indep)
# dep=as.matrix(dep)
# indep=as.matrix(indep)
#
# S=c(1,0,0)
# distrib<-"Normal"
# k<-2
#
# dep=dep[-nrow(dep)]
# myNewIndep=indep[-nrow(indep),]
# newIndep_For=as.matrix(t(indep[nrow(indep),]))
#
# set.seed(10)
# mod <- lm(dep~myNewIndep-1)
# mswm <- msmFit(mod,k=2,p=0,sw=c(T,F,F,T),control=list(trace=T,parallel=F))
#
#
# nPeriods <- 1
# newIndep <- newIndep_For
# newIndep<-as.matrix(newIndep)
#
# nr <- nrow(mswm["model"]$model[,-1])
# k <- mswm@k
# swi <- mswm["switch"][-length(mswm["switch"])]
# n_S <- sum(swi) # discard the variance
# nIndep <- ncol(as.matrix(mswm["model"]$model[,-1,drop=F]))
# n_nS <- nIndep - n_S
# coeff <- as.matrix(mswm["Coef"])
# Coeff <- list(sigma=mswm@std, indep_nS=matrix(coeff[1,which(!swi)],nrow=sum(!swi)),
#               indep_S=matrix(coeff[,which(swi)],nrow=sum(swi)), p=mswm@transMat)
#
# newIndep_S <- matrix(data = 0 , nrow = 1, ncol = n_S)
# newIndep_nS <- matrix(data = 0, nrow = 1, ncol = n_nS)
#
# count_nS <- 0
# count_S <- 0
#
# for (i in 1:nIndep){
#   if(swi[i]==1){
#     count_S <- count_S + 1
#     newIndep_S[,count_S] <- newIndep[,i]
#   }else{
#     count_nS<-count_nS + 1
#     newIndep_nS[,count_nS] <- newIndep[,i]
#   }
# }
#
# newFiltProb <- mswm@transMat %*% (mswm@Fit@filtProb[nr,]) # this is the filtered probabilities
# # of t+1 conditional on the info in t
#
# condMean <- matrix(0,nPeriods,k) # conditional mean in all states
#
# for (i in 1:nPeriods){
#   for (j in 1:k){
#     condMean[i,j] <- newIndep_nS %*% Coeff$indep_nS + newIndep_S %*% (Coeff$indep_S[,j])
#   }
# }
#
# newCondMean <- condMean %*% newFiltProb # the new conditional mean is the weighted average of the cond means in each state
# newCondStd <- Coeff$sigma %*% newFiltProb # same as cond mean

#----------------------#




