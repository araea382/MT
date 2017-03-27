library(MSwM2)
# one test case per SW
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

########################################
# g2 L16B
# 3 states
# switching in all coefficients
########################################
mod_L16B <- lm(fmla, data=train_g2_L16B)
summary(mod_L16B)

set.seed(1)
mswm_L16B_3 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=rep(TRUE,length(mod_L16B$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_3)
#  1508.797 1797.259

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


########################################
# g2 L16B
# 2 states
# switching in all coefficients
########################################
set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=rep(TRUE,length(mod_L16B$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_2)

plot(mswm_L16B_2)

plotDiag(mswm_L16B_2, which=1)
plotDiag(mswm_L16B_2, which=2)
plotDiag(mswm_L16B_2, which=3)

plotProb(mswm_L16B_2, which=1)
plotProb(mswm_L16B_2, which=2)
plotProb(mswm_L16B_2, which=3)
plotProb(mswm_L16B_2, which=4)

plotReg(mswm_L16B_2, expl=predictor[4], regime=1)
plotReg(mswm_L16B_2, expl=predictor[4], regime=2)
plotReg(mswm_L16B_2, expl=predictor[4], regime=3)

plotReg(mswm_L16B_2, expl=predictor[5], regime=1)
plotReg(mswm_L16B_2, expl=predictor[5], regime=2)
plotReg(mswm_L16B_2, expl=predictor[5], regime=3)

plotReg(mswm_L16B_2, expl=predictor[6], regime=1)
plotReg(mswm_L16B_2, expl=predictor[6], regime=2)
plotReg(mswm_L16B_2, expl=predictor[6], regime=3)


########################################
# g2 L16A
# 3 states
# switching in all coefficients
########################################
mod_L16A <- lm(fmla, data=train_g2_L16A)
summary(mod_L16A)

set.seed(1)
mswm_L16A_3 <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=rep(TRUE,length(mod_L16A$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_3)

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


########################################
# g2 L17A
# 3 states
# switching in all coefficients
########################################
mod_L17A <- lm(fmla, data=train_g2_L17A)
summary(mod_L17A)

set.seed(1)
mswm_L17A_3 <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=rep(TRUE,length(mod_L17A$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_3)

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
