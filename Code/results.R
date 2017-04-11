library(MSwM2)
library(ecp)
library(ggplot2)
library(reshape2)

# one test case per SW
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))

predictor_event <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest")
fmla_event <- as.formula(paste("TotCpu ~ ", paste(predictor_event, collapse= "+")))

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

#-------------#
mod_L16B_event <- lm(fmla_event, data=train_g2_L16B)
summary(mod_L16B_event)
switch <- rep(TRUE,length(mod_L16B_event$coefficients)+1+1)

set.seed(1)
mswm_L16B_event_3 <- MSwM2::msmFit(mod_L16B_event, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_event_3)
#-------------#

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
# predict(mswm_L16B_3, newdata)

#--------------------------------#
# change the swiching parameter
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
switch[c(5,6,7,8,9,10)] <- FALSE; switch
set.seed(1)
mswm_L16B_3 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=TRUE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3)

#--------------------------------#
# smoothed prob plot
L16B_3 <- as.data.frame(mswm_L16B_3@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16B") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())


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
set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=rep(TRUE,length(mod_L16B$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_2)
# 1571.198 1763.507 -763.5992

#-------------#
switch <- rep(TRUE,length(mod_L16B_event$coefficients)+1+1)

set.seed(1)
mswm_L16B_event_2 <- MSwM2::msmFit(mod_L16B_event, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_event_2)
#-------------#

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

#--------------------------------#
# change the swiching parameter
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
switch[c(5,6,7,8,9,10)] <- FALSE; switch
set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
summary(mswm_L16B_2)

#--------------------------------#
# smoothed prob plot
L16B_2 <- as.data.frame(mswm_L16B_2@Fit@smoProb)
L16B_2 <- cbind(index=seq(1,nrow(L16B_2)),L16B_2)
colnames(L16B_2) <- c("index","State 1","State 2")

L16B_2 <- melt(L16B_2, id="index")
ggplot(data=L16B_2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16B") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())


state_L16B_2 <- gen(mswm_L16B_2, train_g2_L16B)
ggplot(data=state_L16B_2, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16B_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16B") + theme_bw()

###############
# g2 L16B
# 4 states
###############
# set.seed(1)
# mswm_L16B_4 <- MSwM2::msmFit(mod_L16B, k=4, p=1, sw=rep(TRUE,length(mod_L16B$coefficients)+1+1), control=list(trace=TRUE, maxiter=500, parallel=FALSE))
# summary(mswm_L16B_4)
# # 1433.622 1818.238 -672.811
#
# plot(mswm_L16B_4)
#
# plotDiag(mswm_L16B_4, which=1)
# plotDiag(mswm_L16B_4, which=2)
# plotDiag(mswm_L16B_4, which=3)
#
# plotProb(mswm_L16B_4, which=1)
# plotProb(mswm_L16B_4, which=2)
# plotProb(mswm_L16B_4, which=3)
#
# plotReg(mswm_L16B_4, expl=predictor[4], regime=1)
# plotReg(mswm_L16B_4, expl=predictor[4], regime=2)
#
# plotReg(mswm_L16B_4, expl=predictor[5], regime=1)
# plotReg(mswm_L16B_4, expl=predictor[5], regime=2)
#
# plotReg(mswm_L16B_4, expl=predictor[6], regime=1)
# plotReg(mswm_L16B_4, expl=predictor[6], regime=2)
#
# #--------------------------------#
# # change the swiching parameter
# switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
# switch[c(3,4,5,6,11)] <- FALSE; switch
# set.seed(1)
# mswm_L16B_4 <- MSwM2::msmFit(mod_L16B, k=4, p=1, sw=switch, control=list(trace=TRUE, maxiter=500, parallel=FALSE))
# summary(mswm_L16B_4)


###############
# g2 L16A
# 3 states
###############
predictor2 <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Fdd.Tdd","NumCells")
fmla2 <- as.formula(paste("TotCpu ~ ", paste(predictor2, collapse= "+")))

mod_L16A <- lm(fmla2, data=train_g2_L16A)
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

#--------------------------------#
# smoothed prob plot
L16A_3 <- as.data.frame(mswm_L16A_3@Fit@smoProb)
L16A_3 <- cbind(index=seq(1,nrow(L16A_3)),L16A_3)
colnames(L16A_3) <- c("index","State 1","State 2","State 3")

L16A_3 <- melt(L16A_3, id="index")
ggplot(data=L16A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

state_L16A_3 <- gen(mswm_L16A_3, train_g2_L16A)
ggplot(data=state_L16A_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16A") + theme_bw()

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

#--------------------------------#
# smoothed prob plot
L16A_2 <- as.data.frame(mswm_L16A_2@Fit@smoProb)
L16A_2 <- cbind(index=seq(1,nrow(L16A_2)),L16A_2)
colnames(L16A_2) <- c("index","State 1","State 2")

L16A_2 <- melt(L16A_2, id="index")
ggplot(data=L16A_2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L16A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())


state_L16A_2 <- gen(mswm_L16A_2, train_g2_L16A)
ggplot(data=state_L16A_2, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16A_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16A") + theme_bw()

###
normalized <-function(x){
  ans <- (x-min(x))/(max(x)-min(x))
  return(ans)
} 

L16A <- subset(train_g2_L16A, select=c("TotCpu", predictor))
y_L16A <- normalized(L16A$TotCpu)
dat_L16A <- data.frame(x=seq(1:length(y_L16A)), y_L16A)



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

#--------------------------------#
# smoothed prob plot
L17A_3 <- as.data.frame(mswm_L17A_3@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L17A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

state_L17A_3 <- gen(mswm_L17A_3, train_g2_L17A)
g1 <- ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A") + theme_bw()

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


#--------------------------------#
# smoothed prob plot
L17A_2 <- as.data.frame(mswm_L17A_2@Fit@smoProb)
L17A_2 <- cbind(index=seq(1,nrow(L17A_2)),L17A_2)
colnames(L17A_2) <- c("index","State 1","State 2")

L17A_2 <- melt(L17A_2, id="index")
ggplot(data=L17A_2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + ggtitle("L17A") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  theme_bw() + theme(legend.title = element_blank())

state_L17A_2 <- gen(mswm_L17A_2, train_g2_L17A)
g2 <- ggplot(data=state_L17A_2, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L17A_2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A") + theme_bw()

###############
# ecp
# g2 L16B
###############
set.seed(1)
Ediv_L16B <- e.divisive(matrix(train_g2_L16B$TotCpu), R=499, min.size=5)
Ediv_L16B$k.hat
Ediv_L16B$estimates
out <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))]

dat <- data.frame(index=seq(1,nrow(train_g2_L16B)), TotCpu=train_g2_L16B$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  geom_vline(xintercept=out, colour="red", linetype="dashed") +
  ggtitle("E-divisive L16B") + theme_bw()


###############
# ecp
# g2 L16A
###############
Ediv_L16A <- e.divisive(matrix(train_g2_L16A$TotCpu), R=499, min.size=5)
Ediv_L16A$k.hat
Ediv_L16A$estimates
out <- Ediv_L16A$estimates[c(-1,-length(Ediv_L16A$estimates))]

dat <- data.frame(index=seq(1,nrow(train_g2_L16A)), TotCpu=train_g2_L16A$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() +
  geom_vline(xintercept=out, colour="red", linetype="dashed") +
  ggtitle("E-divisive L16A") + theme_bw()


###############
# ecp
# g2 L17A
###############
set.seed(1)
Ediv_L17A <- e.divisive(matrix(train_g2_L17A$TotCpu), R=499, min.size=5)
Ediv_L17A$k.hat
Ediv_L17A$estimates
out <- Ediv_L17A$estimates[c(-1,-length(Ediv_L17A$estimates))]

dat <- data.frame(index=seq(1,nrow(train_g2_L17A)), TotCpu=train_g2_L17A$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() +
  geom_vline(xintercept=out, colour="red", linetype="dashed") +
  ggtitle("E-divisive L17A") + theme_bw()




###############
# TRY out something
###############
# col <- c("light pink","light blue","light green")
# apply(as.matrix(1:k),1,function(i){
#   
#   y=x["Fit"]["smoProb"][-1,i]
#   val=cbind(which(diff(c(0,findInterval(y,0.5)))==1),which(diff(c(findInterval(y,0.5),0))==-1))
#   apply(val,1,function(el) rect(el[1],min(z),el[2],max(z),col=col[i],border=NA))
#   
# }
# )
# par(new=T,las=1,bty="o",yaxt="n")
# plot(ts(z),col=1,ylim=c(min(z),max(z)),xlab="",ylab="")
# par(las=1,yaxt="n")
# par(las=3,yaxt="s")
# mtext(names(z),side=2,line=2.5,col=1)
# axis(side=4)


# normal plot
x <- mswm_L16B_3
z=x["model"]$model[1]
state <- sapply(1:nrow(train_g2_L16B), function(x) which.max(mswm_L16B_3@Fit@smoProb[x,]))
state <- factor(state)
index=seq(1,nrow(train_g2_L16B))
bf=index-0.5
af=index+0.5
state_L16B_3 <- data.frame(index,bf,af,state,y=train_g2_L16B$TotCpu)
col <- c("light pink","light blue","light green")
plot(0,type="l",xlim=c(1,length(t(z))),ylim=c(min(z),max(z)),xlab=paste(names(z),"vs. Smooth Probabilities"),ylab="")
apply(state_L16B_3,1,function(el) rect(el[2],min(z),el[3],max(z),col=col[el[4]],border=NA))
par(new=T,las=1,bty="o",yaxt="n")
plot(ts(z))



state <- sapply(1:nrow(train_g2_L16B), function(x) which.max(mswm_L16B_3@Fit@smoProb[x,]))
state <- factor(state)
index=seq(1,nrow(train_g2_L16B))
xmin=index-0.5
xmax=index+0.5
y=train_g2_L16B$TotCpu
state_L16B_3 <- data.frame(index,xmin,xmax,state,y=y,ymin=min(y),ymax=max(y))

# ggplot2 with annotation
# col <- c("red","green","blue")
# add_state <- function(x){
#   annotate("rect", xmin=state_L16B_3[x,2], xmax=state_L16B_3[x,3], ymin=min(z), ymax=max(z), alpha=0.2, fill=col[state_L16B_3[x,4]])
# }
# 
# ln <- lapply(1:nrow(state_L16B_3),add_state)
# ggplot(state_L16B_3, aes(x=index, y=y)) + geom_line() + 
#   ylab("TotCpu") + theme_bw() + ln + scale_colour_manual(name="",values=c("red"="red","green"="green","blue"="blue"),labels=c("State 1", "State 2", "State 3"))


ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16B") + theme_bw()

