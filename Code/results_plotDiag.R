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
