library(MSwM2)
library(ecp)
library(ggplot2)
library(reshape2)

# one test case per SW
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

# plotReg(mswm_L16B_3, expl=predictor[1], regime=1)
# plotReg(mswm_L16B_3, expl=predictor[1], regime=2)
# plotReg(mswm_L16B_3, expl=predictor[1], regime=3)

## use plotSmo() to create plot with smoothprob
## use plotArea() to create plot with state area


###############
# g2 L16B
# 3 states
###############
mod_L16B <- lm(fmla, data=train_g2_L16B)
summary(mod_L16B)

# plot with state area
gen <- function(object,data){
    state <- apply(object@Fit@smoProb,1,which.max)
    # state <- sapply(1:nrow(data), function(x) which.max(object@Fit@smoProb[x,]))
    state <- factor(state)
    index=seq(1,nrow(data))
    xmin=index-0.5
    xmax=index+0.5
    y=data$TotCpu
    ans <- data.frame(index,xmin,xmax,state,y=y,ymin=min(y),ymax=max(y))
    return(ans)
}

#----------------------------------------------------------------#
# 5 = DuProdName
# 6 = Fdd/Tdd
# 7-10 = NumCells

# (1) NNN
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5:10)] <- FALSE; switch
set.seed(1)
mswm_L16B_NNN <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_NNN)
# 1603.962 1787.528 -780.9808
# lower BIC
# higher residual standard error in every state
# one state has r-squared 0.58

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_NNN@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_NNN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_NNN, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16B_NNN") + theme_bw()

#----------------------------------------------------------------#
# (2) NNY
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5,6)] <- FALSE; switch
set.seed(1)
mswm_L16B_NNY <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_NNY)
# 1450.896 1704.393 -696.4481
# lower BIC
# stay in the state long 
# iteration 622

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_NNY@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_NNY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_NNY, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("Software release B_NNY") + theme_bw()

#----------------------------------------------------------------#
# (3) NYN
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5,7:10)] <- FALSE; switch
set.seed(1)
mswm_L16B_NYN <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_NYN)
# 1583.335 1784.384 -768.6675
# lower BIC
# higher residual standard error in every state
# one state has r-squared 0.6
# p_11 is 0

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_NYN@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_NYN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_NYN, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16B_NYN") + theme_bw()

#----------------------------------------------------------------#
# (4) NYY
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5)] <- FALSE; switch
set.seed(1)
mswm_L16B_NYY <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_NYY)
# 1505.122 1776.102 -721.561
# lower BIC
# pretty much the same

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_NYY@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_NYY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank()) 



#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_NYY, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("Software release B_NYY") + theme_bw()

#----------------------------------------------------------------#
# (5) YNN
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(6:10)] <- FALSE; switch
set.seed(1)
mswm_L16B_YNN <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_YNN)
# 1605.335 1806.385 -779.6676 
# higer BIC
# higher residual standard error in every state
# p_11 is 0

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_YNN@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_YNN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_YNN, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16B_YNN") + theme_bw()

#----------------------------------------------------------------#
# (6) YNY
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(6)] <- FALSE; switch
set.seed(1)
mswm_L16B_YNY <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_YNY)
# 1454.886 1725.865 -696.4428
# lower BIC
# iteration 754

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_YNY@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_YNY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_YNY, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16B_YNY") + theme_bw()

#----------------------------------------------------------------#
# (7) YYN
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(7:10)] <- FALSE; switch
set.seed(1)
mswm_L16B_YYN <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_YYN)
# 1585.955 1804.487 -767.9776
# higher BIC
# higher residual standard error in every state
# p_11 is 0

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_YYN@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_YYN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_3 <- gen(mswm_L16B_YYN, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16B_YYN") + theme_bw()


# ###############
# # g2 L16B
# # 2 states
# ###############
# switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
# names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
# switch[c(5:6)] <- FALSE; switch
# set.seed(1)
# mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
# summary(mswm_L16B_2)
# # 1671.193 1811.054 -819.5966
# # higher BIC
# # higher residual standard error in every state
# # one state has r-squared 0.69
# 
# #--------------------------------#
# # smoothed prob plot
# L16B_2 <- as.data.frame(mswm_L16B_2@Fit@smoProb)
# L16B_2 <- cbind(index=seq(1,nrow(L16B_2)),L16B_2)
# colnames(L16B_2) <- c("index","State 1","State 2")
# 
# L16B_2 <- melt(L16B_2, id="index")
# ggplot(data=L16B_2, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("L16B") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# #--------------------------------#
# # plot with state area
# state_L16B_2 <- gen(mswm_L16B_2, train_g2_L16B)
# ggplot(data=state_L16B_2, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_L16B_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("TotCpu") + ggtitle("L16B") + theme_bw()


###############
# g2 L16A
# 3 states
###############
predictor2 <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Fdd.Tdd","NumCells")
fmla2 <- as.formula(paste("TotCpu ~ ", paste(predictor2, collapse= "+")))

mod_L16A <- lm(fmla2, data=train_g2_L16A)
summary(mod_L16A)

#----------------------------------------------------------------#
# 5 = Fdd/Tdd
# 6-7 = NumCells

# (1) NN
switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(5,6,7)] <- FALSE; switch
set.seed(1)
mswm_L16A_NN <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_NN)
# 304.4953 413.408 -134.2477
# lower BIC
# p_33 is 0

#--------------------------------#
# smoothed prob plot
L16A_3 <- as.data.frame(mswm_L16A_NN@Fit@smoProb)
L16A_3 <- cbind(index=seq(1,nrow(L16A_3)),L16A_3)
colnames(L16A_3) <- c("index","State 1","State 2","State 3")

L16A_3 <- melt(L16A_3, id="index")
ggplot(data=L16A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16A_NN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16A_3 <- gen(mswm_L16A_NN, train_g2_L16A)
ggplot(data=state_L16A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("Software release A_NN") + theme_bw()

#----------------------------------------------------------------#
# (2) NY
switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(5)] <- FALSE; switch
set.seed(1)
mswm_L16A_NY <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_NY)
# 305.2553 438.3707 -130.6276
# higer BIC
# p_33 is 0

#--------------------------------#
# smoothed prob plot
L16A_3 <- as.data.frame(mswm_L16A_NY@Fit@smoProb)
L16A_3 <- cbind(index=seq(1,nrow(L16A_3)),L16A_3)
colnames(L16A_3) <- c("index","State 1","State 2","State 3")

L16A_3 <- melt(L16A_3, id="index")
ggplot(data=L16A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16A_NY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16A_3 <- gen(mswm_L16A_NY, train_g2_L16A)
ggplot(data=state_L16A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16A_NY") + theme_bw()

#----------------------------------------------------------------#
# (3) YN
switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(6,7)] <- FALSE; switch
set.seed(1)
mswm_L16A_YN <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_YN)
# 280.2183 401.2324 -120.1092
# lower BIC
# std error for Paging in state 3 is 0 which makes t value equals Inf
# singular Hessian matrix

#--------------------------------#
# smoothed prob plot
L16A_3 <- as.data.frame(mswm_L16A_YN@Fit@smoProb)
L16A_3 <- cbind(index=seq(1,nrow(L16A_3)),L16A_3)
colnames(L16A_3) <- c("index","State 1","State 2","State 3")

L16A_3 <- melt(L16A_3, id="index")
ggplot(data=L16A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16A_YN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16A_3 <- gen(mswm_L16A_YN, train_g2_L16A)
ggplot(data=state_L16A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L16A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16A_YN") + theme_bw()


# ###############
# # g2 L16A
# # 2 states
# ###############
# switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
# names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
# switch[c(5,6,7)] <- FALSE; switch
# set.seed(1)
# mswm_L16A_2 <- MSwM2::msmFit(mod_L16A, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
# summary(mswm_L16A_2)
# # 337.0088 415.668 -155.5044
# # lower BIC
# # pretty much the same
# 
# plot(mswm_L16A_2)
# 
# plotDiag(mswm_L16A_2, which=1)
# plotDiag(mswm_L16A_2, which=2)
# plotDiag(mswm_L16A_2, which=3)
# 
# #--------------------------------#
# # smoothed prob plot
# L16A_2 <- as.data.frame(mswm_L16A_2@Fit@smoProb)
# L16A_2 <- cbind(index=seq(1,nrow(L16A_2)),L16A_2)
# colnames(L16A_2) <- c("index","State 1","State 2")
# 
# L16A_2 <- melt(L16A_2, id="index")
# ggplot(data=L16A_2, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("L16A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# #--------------------------------#
# # plot with state area
# state_L16A_2 <- gen(mswm_L16A_2, train_g2_L16A)
# ggplot(data=state_L16A_2, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_L16A_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) + 
#   ylab("TotCpu") + ggtitle("L16A") + theme_bw()


###############
# g2 L17A
# 3 states
###############
mod_L17A <- lm(fmla, data=train_g2_L17A)
summary(mod_L17A)

#----------------------------------------------------------------#
# 5 = DuProdName
# 6 = Fdd/Tdd
# 7-9 = NumCells

# (1) NNN
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,6,7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_NNN <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_NNN)
# 986.3931 1140.474 -473.1965
# lower BIC
# higher residual standard error in every state

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_NNN@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_NNN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())


#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_NNN, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("Software release C_NNN") + theme_bw()

#----------------------------------------------------------------#
# (2) NNY
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,6)] <- FALSE; switch
set.seed(1)
mswm_L17A_NNY <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_NNY)
# 1003.975 1204.28 -475.9873
# higher BIC
# p_11 is 0
#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_NNY@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_NNY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_NNY, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L17A_NNY") + theme_bw()

#----------------------------------------------------------------#
# (3) NYN
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_NYN <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_NYN)
# 983.2508 1152.74 -469.6254
# lower BIC
#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_NYN@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_NYN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_NYN, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L17A_NYN") + theme_bw()

#----------------------------------------------------------------#
# (4) NYY
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5)] <- FALSE; switch
set.seed(1)
mswm_L17A_NYY <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_NYY)
# 968.9294 1184.643 -456.4647
# lower BIC

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_NYY@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_NYY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_NYY, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L17A_NYY") + theme_bw()


#----------------------------------------------------------------#
# (5) YNN
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(6,7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_YNN <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_YNN)
# 976.5107 1146 -466.2554
# lower BIC

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_YNN@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_YNN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_YNN, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L17A_YNN") + theme_bw()

#----------------------------------------------------------------#
# (6) YNY
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(6)] <- FALSE; switch
set.seed(3)
mswm_L17A_YNY <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_YNY)
# 973.5222 1189.236 -458.7611
# lower BIC

# Note: sometime gets this error
# Error in if (abs(pivot) <= tol) { : argument is of length zero 

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_YNY@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_YNY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_YNY, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L17A_YNY") + theme_bw()

#----------------------------------------------------------------#
# (7) YYN
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_YYN <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_YYN)
# 972.4137 1157.311 -462.2069
# lower BIC

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_YYN@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_YYN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_YYN, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L17A_YYN") + theme_bw()


# ###############
# # g2 L17A
# # 2 states
# ###############
# switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
# names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
# switch[c(5,6,7,8,9)] <- FALSE; switch
# set.seed(1)
# mswm_L17A_2 <- MSwM2::msmFit(mod_L17A, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
# summary(mswm_L17A_2)
# # 1039.543 1155.104 -504.7716
# # lower BIC
# # a bit higher but pretty much the same
# 
# plot(mswm_L17A_2)
# 
# plotDiag(mswm_L17A_2, which=1)
# plotDiag(mswm_L17A_2, which=2)
# plotDiag(mswm_L17A_2, which=3)
# 
# #--------------------------------#
# # smoothed prob plot
# L17A_2 <- as.data.frame(mswm_L17A_2@Fit@smoProb)
# L17A_2 <- cbind(index=seq(1,nrow(L17A_2)),L17A_2)
# colnames(L17A_2) <- c("index","State 1","State 2")
# 
# L17A_2 <- melt(L17A_2, id="index")
# ggplot(data=L17A_2, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("L17A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# #--------------------------------#
# # plot with state area
# state_L17A_2 <- gen(mswm_L17A_2, train_g2_L17A)
# ggplot(data=state_L17A_2, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_L17A_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) + 
#   ylab("TotCpu") + ggtitle("L17A") + theme_bw()

