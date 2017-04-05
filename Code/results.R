library(MSwM2)
# one test case per SW
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

###############
# g2 L16B
# 3 states
###############
mod_L16B <- lm(fmla, data=train_g2_L16B)
summary(mod_L16B)
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)

set.seed(1)
mswm_L16B_3 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_3)
# 1508.797 1797.259 -721.3983

plot(mswm_L16B_3)

plotDiag(mswm_L16B_3, which=1)
plotDiag(mswm_L16B_3, which=2)
plotDiag(mswm_L16B_3, which=3)

plotProb(mswm_L16B_3, which=1)
plotProb(mswm_L16B_3, which=2)
plotProb(mswm_L16B_3, which=3)
plotProb(mswm_L16B_3, which=4)

plotReg(mswm_L16B_3, expl=predictor[1], regime=1)
plotReg(mswm_L16B_3, expl=predictor[1], regime=2)
plotReg(mswm_L16B_3, expl=predictor[1], regime=3)

plotReg(mswm_L16B_3, expl=predictor[2], regime=1)
plotReg(mswm_L16B_3, expl=predictor[2], regime=2)
plotReg(mswm_L16B_3, expl=predictor[2], regime=3)

plotReg(mswm_L16B_3, expl=predictor[3], regime=1)
plotReg(mswm_L16B_3, expl=predictor[3], regime=2)
plotReg(mswm_L16B_3, expl=predictor[3], regime=3)

# # predict
# newdata <- test_g2_L16B[1,]
# predict(mswm_L16B_3, newdata)

# change the swiching parameter
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
switch[c(5,6)] <- FALSE; switch
set.seed(1)
mswm_L16B_3 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3)

###############
# g2 L16B
# 2 states
###############
set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=rep(TRUE,length(mod_L16B$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_2)
# 1571.198 1763.507 -763.5992

plot(mswm_L16B_2)

plotDiag(mswm_L16B_2, which=1)
plotDiag(mswm_L16B_2, which=2)
plotDiag(mswm_L16B_2, which=3)

plotProb(mswm_L16B_2, which=1)
plotProb(mswm_L16B_2, which=2)
plotProb(mswm_L16B_2, which=3)

plotReg(mswm_L16B_2, expl=predictor[4], regime=1)
plotReg(mswm_L16B_2, expl=predictor[4], regime=2)

plotReg(mswm_L16B_2, expl=predictor[5], regime=1)
plotReg(mswm_L16B_2, expl=predictor[5], regime=2)

plotReg(mswm_L16B_2, expl=predictor[6], regime=1)
plotReg(mswm_L16B_2, expl=predictor[6], regime=2)

# change the swiching parameter
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
switch[c(3,4,5,6,11)] <- FALSE; switch
set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_2)

###############
# g2 L16B
# 4 states
###############
set.seed(1)
mswm_L16B_4 <- MSwM2::msmFit(mod_L16B, k=4, p=1, sw=rep(TRUE,length(mod_L16B$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_4)
# 1433.622 1818.238 -672.811

plot(mswm_L16B_4)

plotDiag(mswm_L16B_4, which=1)
plotDiag(mswm_L16B_4, which=2)
plotDiag(mswm_L16B_4, which=3)

plotProb(mswm_L16B_4, which=1)
plotProb(mswm_L16B_4, which=2)
plotProb(mswm_L16B_4, which=3)

plotReg(mswm_L16B_4, expl=predictor[4], regime=1)
plotReg(mswm_L16B_4, expl=predictor[4], regime=2)

plotReg(mswm_L16B_4, expl=predictor[5], regime=1)
plotReg(mswm_L16B_4, expl=predictor[5], regime=2)

plotReg(mswm_L16B_4, expl=predictor[6], regime=1)
plotReg(mswm_L16B_4, expl=predictor[6], regime=2)

# change the swiching parameter
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
switch[c(3,4,5,6,11)] <- FALSE; switch
set.seed(1)
mswm_L16B_4 <- MSwM2::msmFit(mod_L16B, k=4, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_4)

###############
# g2 L16A
# 3 states
###############
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Fdd.Tdd","NumCells")
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

mod_L16A <- lm(fmla, data=train_g2_L16A)
summary(mod_L16A)
alias(mod_L16A)

set.seed(1)
mswm_L16A_3 <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=rep(TRUE,length(mod_L16A$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_3)
# 272.465 417.6819 -112.2325

plot(mswm_L16A_3)

plotDiag(mswm_L16A_3, which=1)
plotDiag(mswm_L16A_3, which=2)
plotDiag(mswm_L16A_3, which=3)

plotProb(mswm_L16A_3, which=1)
plotProb(mswm_L16A_3, which=2)
plotProb(mswm_L16A_3, which=3)
plotProb(mswm_L16A_3, which=4)

plotReg(mswm_L16A_3, expl=predictor[1], regime=1)
plotReg(mswm_L16A_3, expl=predictor[1], regime=2)
plotReg(mswm_L16A_3, expl=predictor[1], regime=3)

plotReg(mswm_L16A_3, expl=predictor[2], regime=1)
plotReg(mswm_L16A_3, expl=predictor[2], regime=2)
plotReg(mswm_L16A_3, expl=predictor[2], regime=3)

plotReg(mswm_L16A_3, expl=predictor[3], regime=1)
plotReg(mswm_L16A_3, expl=predictor[3], regime=2)
plotReg(mswm_L16A_3, expl=predictor[3], regime=3)


###############
# g2 L16A
# 2 states
###############
set.seed(1)
mswm_L16A_2 <- MSwM2::msmFit(mod_L16A, k=2, p=1, sw=rep(TRUE,length(mod_L16A$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_2)
# 342.8614 439.6726 -155.4307

plot(mswm_L16A_2)

plotDiag(mswm_L16A_2, which=1)
plotDiag(mswm_L16A_2, which=2)
plotDiag(mswm_L16A_2, which=3)

plotProb(mswm_L16A_2, which=1)
plotProb(mswm_L16A_2, which=2)
plotProb(mswm_L16A_2, which=3)

plotReg(mswm_L16A_2, expl=predictor[1], regime=1)
plotReg(mswm_L16A_2, expl=predictor[1], regime=2)

plotReg(mswm_L16A_2, expl=predictor[2], regime=1)
plotReg(mswm_L16A_2, expl=predictor[2], regime=2)

plotReg(mswm_L16A_2, expl=predictor[3], regime=1)
plotReg(mswm_L16A_2, expl=predictor[3], regime=2)


###############
# g2 L17A
# 3 states
###############
mod_L17A <- lm(fmla, data=train_g2_L17A)
summary(mod_L17A)

set.seed(1)
mswm_L17A_3 <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=rep(TRUE,length(mod_L17A$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_3)
# 967.9531 1199.075 -453.9765

plot(mswm_L17A_3)

plotDiag(mswm_L17A_3, which=1)
plotDiag(mswm_L17A_3, which=2)
plotDiag(mswm_L17A_3, which=3)

plotProb(mswm_L17A_3, which=1)
plotProb(mswm_L17A_3, which=2)
plotProb(mswm_L17A_3, which=3)
plotProb(mswm_L17A_3, which=4)

plotReg(mswm_L17A_3, expl=predictor[1], regime=1)
plotReg(mswm_L17A_3, expl=predictor[1], regime=2)
plotReg(mswm_L17A_3, expl=predictor[1], regime=3)

plotReg(mswm_L17A_3, expl=predictor[2], regime=1)
plotReg(mswm_L17A_3, expl=predictor[2], regime=2)
plotReg(mswm_L17A_3, expl=predictor[2], regime=3)

plotReg(mswm_L17A_3, expl=predictor[3], regime=1)
plotReg(mswm_L17A_3, expl=predictor[3], regime=2)
plotReg(mswm_L17A_3, expl=predictor[3], regime=3)


###############
# g2 L17A
# 2 states
###############
set.seed(1)
mswm_L17A_2 <- MSwM2::msmFit(mod_L17A, k=2, p=1, sw=rep(TRUE,length(mod_L17A$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_2)
# 1034.98 1189.061 -497.4898

plot(mswm_L17A_2)

plotDiag(mswm_L17A_2, which=1)
plotDiag(mswm_L17A_2, which=2)
plotDiag(mswm_L17A_2, which=3)

plotProb(mswm_L17A_2, which=1)
plotProb(mswm_L17A_2, which=2)
plotProb(mswm_L17A_2, which=3)

plotReg(mswm_L17A_2, expl=predictor[1], regime=1)
plotReg(mswm_L17A_2, expl=predictor[1], regime=2)

plotReg(mswm_L17A_2, expl=predictor[2], regime=1)
plotReg(mswm_L17A_2, expl=predictor[2], regime=2)

plotReg(mswm_L17A_2, expl=predictor[3], regime=1)
plotReg(mswm_L17A_2, expl=predictor[3], regime=2)

