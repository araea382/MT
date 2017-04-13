library(MSwM2)
library(ecp)
library(ggplot2)
library(reshape2)

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
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5,6,7,8,9,10)] <- FALSE; switch
set.seed(1)
mswm_L16B_3 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3)
# 1603.962 1787.528 -780.9808
# lower BIC
# higher residual standard error in every state
# one state has r-squared 0.58

plotDiag(mswm_L16B_3, which=1)
plotDiag(mswm_L16B_3, which=2)
plotDiag(mswm_L16B_3, which=3)

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_3@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16B") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
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

state_L16B_3 <- gen(mswm_L16B_3, train_g2_L16B)
ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16B") + theme_bw()

###############
# g2 L16B
# 2 states
###############
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5,6,7,8,9,10)] <- FALSE; switch
set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_2)
# 1671.193 1811.054 -819.5966
# higher BIC
# higher residual standard error in every state
# one state has r-squared 0.69

plot(mswm_L16B_2)

plotDiag(mswm_L16B_2, which=1)
plotDiag(mswm_L16B_2, which=2)
plotDiag(mswm_L16B_2, which=3)

#--------------------------------#
# smoothed prob plot
L16B_2 <- as.data.frame(mswm_L16B_2@Fit@smoProb)
L16B_2 <- cbind(index=seq(1,nrow(L16B_2)),L16B_2)
colnames(L16B_2) <- c("index","State 1","State 2")

L16B_2 <- melt(L16B_2, id="index")
ggplot(data=L16B_2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16B") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16B_2 <- gen(mswm_L16B_2, train_g2_L16B)
ggplot(data=state_L16B_2, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16B_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16B") + theme_bw()


###############
# g2 L16A
# 3 states
###############
predictor2 <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Fdd.Tdd","NumCells")
fmla2 <- as.formula(paste("TotCpu ~ ", paste(predictor2, collapse= "+")))

mod_L16A <- lm(fmla2, data=train_g2_L16A)
summary(mod_L16A)

switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(5,6,7)] <- FALSE; switch
set.seed(1)
mswm_L16A_3 <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_3)
# 304.4953 413.408 -134.2477
# lower BIC
# p_33 is 0

plot(mswm_L16A_3)

plotDiag(mswm_L16A_3, which=1)
plotDiag(mswm_L16A_3, which=2)
plotDiag(mswm_L16A_3, which=3)

#--------------------------------#
# smoothed prob plot
L16A_3 <- as.data.frame(mswm_L16A_3@Fit@smoProb)
L16A_3 <- cbind(index=seq(1,nrow(L16A_3)),L16A_3)
colnames(L16A_3) <- c("index","State 1","State 2","State 3")

L16A_3 <- melt(L16A_3, id="index")
ggplot(data=L16A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16A_3 <- gen(mswm_L16A_3, train_g2_L16A)
ggplot(data=state_L16A_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16A") + theme_bw()

###############
# g2 L16A
# 2 states
###############
switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1)
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(5,6,7)] <- FALSE; switch
set.seed(1)
mswm_L16A_2 <- MSwM2::msmFit(mod_L16A, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_2)
# 337.0088 415.668 -155.5044
# lower BIC
# pretty much the same

plot(mswm_L16A_2)

plotDiag(mswm_L16A_2, which=1)
plotDiag(mswm_L16A_2, which=2)
plotDiag(mswm_L16A_2, which=3)

#--------------------------------#
# smoothed prob plot
L16A_2 <- as.data.frame(mswm_L16A_2@Fit@smoProb)
L16A_2 <- cbind(index=seq(1,nrow(L16A_2)),L16A_2)
colnames(L16A_2) <- c("index","State 1","State 2")

L16A_2 <- melt(L16A_2, id="index")
ggplot(data=L16A_2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L16A_2 <- gen(mswm_L16A_2, train_g2_L16A)
ggplot(data=state_L16A_2, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16A_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16A") + theme_bw()


###############
# g2 L17A
# 3 states
###############
mod_L17A <- lm(fmla, data=train_g2_L17A)
summary(mod_L17A)

switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,6,7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_3 <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_3)
# 986.3931 1140.474 -473.1965
# lower BIC
# higher residual standard error in every state

plot(mswm_L17A_3)

plotDiag(mswm_L17A_3, which=1)
plotDiag(mswm_L17A_3, which=2)
plotDiag(mswm_L17A_3, which=3)

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_3@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L17A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_3 <- gen(mswm_L17A_3, train_g2_L17A)
ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A") + theme_bw()

###############
# g2 L17A
# 2 states
###############
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1)
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,6,7,8,9)] <- FALSE; switch
set.seed(1)
mswm_L17A_2 <- MSwM2::msmFit(mod_L17A, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_2)
# 1039.543 1155.104 -504.7716
# lower BIC
# a bit higher but pretty much the same

plot(mswm_L17A_2)

plotDiag(mswm_L17A_2, which=1)
plotDiag(mswm_L17A_2, which=2)
plotDiag(mswm_L17A_2, which=3)

#--------------------------------#
# smoothed prob plot
L17A_2 <- as.data.frame(mswm_L17A_2@Fit@smoProb)
L17A_2 <- cbind(index=seq(1,nrow(L17A_2)),L17A_2)
colnames(L17A_2) <- c("index","State 1","State 2")

L17A_2 <- melt(L17A_2, id="index")
ggplot(data=L17A_2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L17A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

#--------------------------------#
# plot with state area
state_L17A_2 <- gen(mswm_L17A_2, train_g2_L17A)
ggplot(data=state_L17A_2, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L17A_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A") + theme_bw()
