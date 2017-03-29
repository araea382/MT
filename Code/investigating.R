library(MSwM2)

library(nlme)
library(parallel)
library(matlib)
g2_L16B <- read.csv("g2_L16B.csv")

colnames(g2_L16B)[14] <- "TotCpu" # need to rename the variable
g2_L16B$DuProdName <- as.factor(g2_L16B$DuProdName)
g2_L16B$Fdd.Tdd <- as.factor(g2_L16B$Fdd.Tdd)
g2_L16B$NumCells <- as.factor(g2_L16B$NumCells)

train_num <- floor(nrow(g2_L16B) * 0.9)
train_g2_L16B <- g2_L16B[1:train_num,]
test_g2_L16B <- g2_L16B[-c(1:train_num),]


##-------------------------------------------------------------------------------------##
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest")
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Srb1SetupReject")
# predictor <- c("DuProdName","Fdd.Tdd","NumCells")
# predictor <- c("DuProdName","Fdd.Tdd","NumCells","RrcConnectionSetupComplete","Paging","X2HandoverRequest")
# predictor <- c("DuProdName","Fdd.Tdd","NumCells","Paging")
# predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","ReEstablishmentAttempt")

predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))
mod <- lm(fmla, data=train_g2_L16B)

k=3
p=1
sw=rep(T,length(mod$coefficients)+p+1)
control=list(trace = T,  maxiter = 500, tol = 1e-8, maxiterInner=10, maxiterOuter=5, parallelization=F)
data <- train_g2_L16B

object <- mod

# run whole .MSM.lm.fit one time first before do this
# also run other function as well
set.seed(12)
object <- ans




mod2=lm(y~x,example)
k=2
p=1
sw=c(T,T,T,F)
control=list(trace = T,  maxiter = 100, tol = 1e-8, maxiterInner=10, maxiterOuter=5, parallelization=F)

set.seed(12)
object <- mod2
data <- example

object <- ans





dat <- data.frame(dep, indep)
mod <- lm(dep~.-1, dat)

k=2
p=0
sw=c(T,F,F,T)
control=list(trace = T,  maxiter = 100, tol = 1e-8, maxiterInner=10, maxiterOuter=5, parallelization=F)

set.seed(10)
object <- mod
data <- dat

object <- ans
