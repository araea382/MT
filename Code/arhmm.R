library(MSwM)
data(example)
mod=lm(y~x,example)
summary(mod)
acf(resid(mod))
mod.mswm=msmFit(mod,k=2,p=1,sw=c(T,T,T,T),control=list(trace=T,parallel=F))
summary(mod.mswm)

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
# g2 L16B
# one test case per SW
# divide train (80) test (20)
train_num <- floor(nrow(g2_L16B_min) * 0.8) 
train_g2_L16B_min <- g2_L16B_min[1:train_num,]
test_g2_L16B_min <- g2_L16B_min[-c(1:train_num),]

colnames(train_g2_L16B_min)[14] <- "TotCpu" # need to rename the variable

predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest") 
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))
mod <- lm(fmla, data=train_g2_L16B_min)
summary(mod)

# 3 states
set.seed(12)
model_mswm <- msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
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

plotReg(model_mswm, expl=predictor[1], regime=1)
plotReg(model_mswm, expl=predictor[2], regime=1)
plotReg(model_mswm, expl=predictor[3], regime=1)

# it seems that scale or not scale is the same

#----------------------#
# 2 states
set.seed(12)
model_mswm2 <- msmFit(mod, k=2, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(model_mswm2)

plot(msmResid(model_mswm2), type="l")
acf(msmResid(model_mswm2))

plot(model_mswm2)

plotDiag(model_mswm2, which=1)
plotDiag(model_mswm2, which=2)
plotDiag(model_mswm2, which=3)

plotProb(model_mswm2, which=1)
plotProb(model_mswm2, which=2)
plotProb(model_mswm2, which=3)

plotReg(model_mswm2, expl=predictor[1], regime=1)
plotReg(model_mswm2, expl=predictor[2], regime=1)
plotReg(model_mswm2, expl=predictor[3], regime=1)

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

# df3 <- subset(df2, select=c(14,18:length(df2)))
# df3 <- add_x_name(df3)
# mod3 <- lm(TotCpu ~ ., data=df3)

set.seed(123)
model_mswm3 <- msmFit(mod3, k=3, p=1, sw=rep(TRUE,length(mod3$coefficients)+2), control=list(parallel=F))
summary(model_mswm3)

# mod3 <- lm(`TotCpu`~., data=df3)
# step3 <- stepAIC(mod3, direction="both")
# model_mswm3 <- msmFit(step3, k=3, p=1, sw=rep(TRUE,length(step3$coefficients)+2), control=list(parallel=F))
# summary(model_mswm3)

#----------------------#
# 
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
# forecast
# new_data <- test_g2_L16B_min
dat <- example[-nrow(example),]
newIndep <- example[nrow(example),]
mod=lm(y~.,dat)
summary(mod)

mod.mswm=msmFit(mod,k=2,p=0,sw=c(T,T,T),control=list(trace=T,parallel=F))
summary(mod.mswm)


nPeriods <- 1
newIndep <- as.matrix(newIndep)

nr <- length(mod.mswm["model"]$model[,-1])
S <- as.numeric(mod.mswm@switch)
S <- S[-length(S)]
k <- mod.mswm@k
Coef <- mod.mswm@Coef
nIndep <- ncol(mod.mswm["model"]$model[,-1,drop=F])
n_S <- sum(S)
n_nS <- nIndep - n_S # negative value ????

newIndep_S <- matrix(data = 0 , nrow = 1, ncol = n_S)
newIndep_nS < -matrix(data = 0, nrow = 1, ncol = n_nS)

count_nS <- 0
count_S <- 0

for (i in 1:nIndep){
  if(S[i]==1){
    count_S <- count_S + 1
    newIndep_S[,count_S] <- newIndep[,i]
  }else{
    count_nS<-count_nS + 1
    newIndep_nS[,count_nS] <- newIndep[,i]
  }
}

newFiltProb <- mod.mswm@transMat %*% (mod.mswm@Fit@filtProb[nr,]) # this is the filtered probabilities 
# of t+1 conditional on the info in t

condMean <- matrix(0,nPeriods,k) # conditional mean in all states

for (i in 1:nPeriods){
  for (j in 1:k){
    condMean[i,j] <- newIndep_nS %*% Coeff$indep_nS + newIndep_S %*% (Coeff$indep_S[,j])
  }
}    

newCondMean <- condMean %*% newFiltProb # the new conditional mean is the weighted average of the cond means in each state
newCondStd <- Coeff$sigma %*% newFiltProb # same as cond mean

forOut <- list(condMean=newCondMean, ncondStd=newCondStd)

