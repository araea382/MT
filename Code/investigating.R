library(nlme)
library(parallel)
g2_L16B_min <- read.csv("g2_L16B_min.csv")

train_num <- floor(nrow(g2_L16B_min) * 0.8) 
train_g2_L16B_min <- g2_L16B_min[1:train_num,]
test_g2_L16B_min <- g2_L16B_min[-c(1:train_num),]

colnames(train_g2_L16B_min)[14] <- "TotCpu" # need to rename the variable

predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest") 
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))
mod <- lm(fmla, data=train_g2_L16B_min)

k=3
p=1
sw=c(TRUE,FALSE,TRUE,FALSE,TRUE,FALSE)
control=list(trace = T,  maxiter = 100, tol = 1e-8, maxiterInner=10, maxiterOuter=5, parallelization=F)

object <- mod
data <- train_g2_L16B_min

# run whole .MSM.lm.fit one time first before do this
# also run other function as well
set.seed(10)
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
