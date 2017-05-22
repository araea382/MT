# using g2_extract data from R-code.R
# filter only for the TC8 (165 < RrcConnectionSetupComplete < 166)
g2_extract2 <- dplyr::filter(g2_extract, RrcConnectionSetupComplete > 165 & RrcConnectionSetupComplete < 166)
g2_extract2 <- data.table(g2_extract2)

g2_L16A2 <- get_subset(g2_extract2, "L16A") # 64 -> 36 
g2_L16B2 <- get_subset(g2_extract2, "L16B") # 241 -> 143
g2_L17A2 <- get_subset(g2_extract2, "L17A") # 144 -> 102


#----------------------------------------------------------------------#
# Markov switching model
#----------------------------------------------------------------------#
#-----------------------------------------------------#
# software release L16A
#-----------------------------------------------------#
mod_L16A_cpu <- lm(TotCpu~1, data=g2_L16A2)
summary(mod_L16A_cpu)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L16A_cpu$coefficients)+1+1)
# names(switch) <- c(names(mod_L16A_cpu$coefficients),"AR","var")
# switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

# three-state model
set.seed(1)
mswm_L16A_3_cpu <- MSwM2::msmFit(mod_L16A_cpu, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16A_3_cpu)

# two-state model
set.seed(1)
mswm_L16A_2_cpu <- MSwM2::msmFit(mod_L16A_cpu, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16A_2_cpu)

# plot
plotSmo(mswm_L16A_3_cpu)
plotArea(mswm_L16A_3_cpu)

plotSmo(mswm_L16A_2_cpu)
plotArea(mswm_L16A_2_cpu)



#-----------------------------------------------------#
# software release L16B
#-----------------------------------------------------#
# fit linear model
mod_L16B_cpu <- lm(TotCpu~1, data=g2_L16B2)
summary(mod_L16B_cpu)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L16B_cpu$coefficients)+1+1)
# names(switch) <- c(names(mod_L16B_cpu$coefficients),"AR","var")
# switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

# three-state model
set.seed(1)
mswm_L16B_3_cpu <- MSwM2::msmFit(mod_L16B_cpu, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3_cpu)

# two-state model
set.seed(1)
mswm_L16B_2_cpu <- MSwM2::msmFit(mod_L16B_cpu, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_2_cpu)

# plot
plotSmo(mswm_L16B_3_cpu)
plotArea(mswm_L16B_3_cpu)

plotSmo(mswm_L16B_2_cpu)
plotArea(mswm_L16B_2_cpu)


pred_state_L16B_cpu <- sapply(1:nrow(g2_L16B2), function(x) which.max(mswm_L16B_3_cpu@Fit@smoProb[x,]))
chg_mswm_L16B_cpu <- which(diff(pred_state_L16B_cpu) != 0) + 1
# 3   4  10  64 137


#-----------------------------------------------------#
# software release L17A
#-----------------------------------------------------#
# fit linear model
mod_L17A_cpu <- lm(TotCpu~1, data=g2_L17A2)
summary(mod_L17A_cpu)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L17A_cpu$coefficients)+1+1)
# names(switch) <- c(names(mod_L17A_cpu$coefficients),"AR","var")
# switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

# three-state model
set.seed(1)
mswm_L17A_3_cpu <- MSwM2::msmFit(mod_L17A_cpu, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L17A_3_cpu)

# two-state model
set.seed(1)
mswm_L17A_2_cpu <- MSwM2::msmFit(mod_L17A_cpu, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L17A_2_cpu)

# plot
plotSmo(mswm_L17A_3_cpu)
plotArea(mswm_L17A_3_cpu)

plotSmo(mswm_L17A_2_cpu)
plotArea(mswm_L17A_2_cpu)


#----------------------------------------------------------------------#
# E-divisive method
#----------------------------------------------------------------------#
#-----------------------------------------------------#
# software release L16B
#-----------------------------------------------------#
set.seed(1)
Ediv_L16B <- e.divisive(matrix(g2_L16B2$TotCpu), R=499, min.size=5)
Ediv_L16B$estimates
out_L16B <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))]
# 10  64 136

dat <- data.frame(index=seq(1,nrow(g2_L16B2)), TotCpu=g2_L16B2$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  geom_vline(xintercept=out_L16B, colour="red", linetype="longdash") +
  ggtitle("E-divisive L16B") + theme_bw() + coord_cartesian(ylim = c(50, 300)) 


