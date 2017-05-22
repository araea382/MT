library(data.table)
library(MSwM2)
library(ecp)
library(ggplot2)
library(dplyr)
library(reshape2)

# g2_extract
# TC8
g2_extract2 <- dplyr::filter(g2_extract, RrcConnectionSetupComplete > 165 & RrcConnectionSetupComplete < 166)
g2_extract2 <- data.table(g2_extract2)

g2_L16A2 <- get_subset(g2_extract2, "L16A") # 64 -> 36 
g2_L16B2 <- get_subset(g2_extract2, "L16B") # 241 -> 143
g2_L17A2 <- get_subset(g2_extract2, "L17A") # 144 -> 102

#-----------------------------------------------------#
# software release L16B
#-----------------------------------------------------#
# fit linear model
# DuProdName and Fdd/Tdd only has one factor
predictor <- c("RrcConnectionSetupComplete","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))
mod_L16B <- lm(fmla, data=g2_L16B2)
summary(mod_L16B)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1)
# names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
# switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

set.seed(1)
mswm_L16B_3 <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3)

set.seed(1)
mswm_L16B_2 <- MSwM2::msmFit(mod_L16B, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_2)

# plot
plotSmo(mswm_L16B_3)
plotArea(mswm_L16B_3)

plotSmo(mswm_L16B_2)
plotArea(mswm_L16B_2)


# gen <- function(object,data){
#   state <- apply(object@Fit@smoProb,1,which.max)
#   # state <- sapply(1:nrow(data), function(x) which.max(object@Fit@smoProb[x,]))
#   state <- factor(state)
#   index=seq(1,nrow(data))
#   xmin=index-0.5
#   xmax=index+0.5
#   y=data$TotCpu
#   ans <- data.frame(index,xmin,xmax,state,y=y,ymin=50,ymax=300)
#   return(ans)
# }
# 
# state_L16B_3 <- gen(mswm_L16B2, g2_L16B2)
# ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) + 
#   ylab("TotCpu") + theme_bw() + 
#   theme(legend.position="bottom") + coord_cartesian(ylim = c(50, 300)) 

# set.seed(1)
# Ediv_L16B <- e.divisive(matrix(g2_L16B2$TotCpu), R=499, min.size=5)
# Ediv_L16B$estimates
# out_L16B <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))]
# 
# dat <- data.frame(index=seq(1,nrow(g2_L16B2)), TotCpu=g2_L16B2$TotCpu)
# ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   geom_vline(xintercept=out_L16B, colour="red", linetype="longdash") +
#   ggtitle("E-divisive L16B") + theme_bw() + coord_cartesian(ylim = c(50, 300))
# 
# 

#-----------------------------------------------------#
# ONLY CPU
# fit linear model
mod_L16B_cpu <- lm(TotCpu~1, data=g2_L16B2)
summary(mod_L16B_cpu)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L16B_cpu$coefficients)+1+1)
# names(switch) <- c(names(mod_L16B_cpu$coefficients),"AR","var")
# switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

set.seed(1)
mswm_L16B_3_cpu <- MSwM2::msmFit(mod_L16B_cpu, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3_cpu)

set.seed(1)
mswm_L16B_2_cpu <- MSwM2::msmFit(mod_L16B_cpu, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_2_cpu)

# plot
plotSmo(mswm_L16B_3_cpu)
plotArea(mswm_L16B_3_cpu)

plotSmo(mswm_L16B_2_cpu)
plotArea(mswm_L16B_2_cpu)


set.seed(1)
Ediv_L16B <- e.divisive(matrix(g2_L16B2$TotCpu), R=499, min.size=5)
Ediv_L16B$estimates
out_L16B <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))]
# 10  64 136

dat <- data.frame(index=seq(1,nrow(g2_L16B2)), TotCpu=g2_L16B2$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    geom_vline(xintercept=out_L16B, colour="red", linetype="longdash") +
    ggtitle("E-divisive L16B") + theme_bw() + coord_cartesian(ylim = c(50, 300)) 


pred_state_L16B_cpu <- sapply(1:nrow(g2_L16B2), function(x) which.max(mswm_L16B_3_cpu@Fit@smoProb[x,]))
chg_mswm_L16B_cpu <- which(diff(pred_state_L16B_cpu) != 0) + 1
# 3   4  10  64 137


#-----------------------------------------------------#
# ONLY RrcConnectionSetupComplete
# fit linear model
mod_L16B_rrc <- lm(TotCpu~RrcConnectionSetupComplete, data=g2_L16B2)
summary(mod_L16B_rrc)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L16B_rrc$coefficients)+1+1)
# names(switch) <- c(names(mod_L16B_rrc$coefficients),"AR","var")
# switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

set.seed(1)
mswm_L16B_3_rrc <- MSwM2::msmFit(mod_L16B_rrc, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3_rrc)

set.seed(1)
mswm_L16B_2_rrc <- MSwM2::msmFit(mod_L16B_rrc, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_2_rrc)

# plot
plotSmo(mswm_L16B_3_rrc)
plotArea(mswm_L16B_3_rrc)

plotSmo(mswm_L16B_2_rrc)
plotArea(mswm_L16B_2_rrc)


pred_state_L16B_rrc <- sapply(1:nrow(g2_L16B2), function(x) which.max(mswm_L16B_3_rrc@Fit@smoProb[x,]))
chg_mswm_L16B_rrc <- which(diff(pred_state_L16B_rrc) != 0) + 1


