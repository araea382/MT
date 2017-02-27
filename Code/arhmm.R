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

set.seed(12)
model_mswm <- msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
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

plotReg(model_mswm, expl=predictor[1], regime=1)
plotReg(model_mswm, expl=predictor[2], regime=1)
plotReg(model_mswm, expl=predictor[3], regime=1)

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
