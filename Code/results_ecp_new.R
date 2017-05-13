###############
# ecp
# g2 L16B
###############
set.seed(1)
Ediv_L16B <- e.divisive(matrix(train_g2_L16B$TotCpu), R=499, min.size=5)
Ediv_L16B$k.hat
Ediv_L16B$estimates
out_L16B <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))]

dat <- data.frame(index=seq(1,nrow(train_g2_L16B)), TotCpu=train_g2_L16B$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  geom_vline(xintercept=out_L16B, colour="red", linetype="longdash") +
  ggtitle("E-divisive L16B") + theme_bw()

#---------------------------------------------------------------------------#
# compare
L16B_3 <- as.data.frame(mswm_L16B_NYY@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
g <- ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_NYY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

g + geom_vline(xintercept=out_L16B, color="black", size=0.6, linetype="longdash")


state_L16B_3 <- gen(mswm_L16B_NYY, train_g2_L16B)
g2 <- ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16B_NYY") + theme_bw()

g2 + geom_vline(xintercept=out_L16B, color="red", size=0.6, linetype="longdash")


#--------------------------------------#
pred_state_L16B <- sapply(1:nrow(train_g2_L16B), function(x) which.max(mswm_L16B_NYY@Fit@smoProb[x,]))
chg_mswm_L16B <- which(diff(pred_state_L16B) != 0) + 1

ind <- nrow(train_g2_L16B)
method <- c(rep("Markov switching model",ind),rep("E-divisive",ind))
changePoints <- data.frame(changeP=c(chg_mswm_L16B, out_L16B), method=c(rep("Markov switching model",length(chg_mswm_L16B)), rep("E-divisive",length(out_L16B))))
temp <- data.frame(index=rep(1:ind,2),y=rep(train_g2_L16B$TotCpu,2), method)
temp$method <- factor(temp$method, levels=c("Markov switching model","E-divisive"))
ggplot(data=temp, aes(x=index,y=y)) + geom_line() +
  facet_grid(method ~ ., scales = 'free_y') + theme_bw() +
  ggtitle("Software release B") +
  theme(panel.spacing = unit(0.2, "lines")) +
  geom_vline(aes(xintercept=changeP), data=changePoints, linetype="longdash", colour=c(rep("cyan3",length(chg_mswm_L16B)),rep("orangered",length(out_L16B))))



###############
# ecp
# g2 L16A
###############
set.seed(1)
Ediv_L16A <- e.divisive(matrix(train_g2_L16A$TotCpu), R=499, min.size=5)
Ediv_L16A$k.hat
Ediv_L16A$estimates
out_L16A <- Ediv_L16A$estimates[c(-1,-length(Ediv_L16A$estimates))]

dat <- data.frame(index=seq(1,nrow(train_g2_L16A)), TotCpu=train_g2_L16A$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() +
  geom_vline(xintercept=out, colour="red", linetype="dashed") +
  ggtitle("E-divisive L16A") + theme_bw()

#---------------------------------------------------------------------------#
# compare
pred_state_L16A <- sapply(1:nrow(train_g2_L16A), function(x) which.max(mswm_L16A_NN@Fit@smoProb[x,]))
chg_mswm_L16A <- which(diff(pred_state_L16A) != 0) + 1

ind <- nrow(train_g2_L16A)
method <- c(rep("Markov switching model",ind),rep("E-divisive",ind))
changePoints <- data.frame(changeP=c(chg_mswm_L16A, out_L16A), method=c(rep("Markov switching model",length(chg_mswm_L16A)), rep("E-divisive",length(out_L16A))))
temp <- data.frame(index=rep(1:ind,2),y=rep(train_g2_L16A$TotCpu,2), method)
temp$method <- factor(temp$method, levels=c("Markov switching model","E-divisive"))
ggplot(data=temp, aes(x=index,y=y)) + geom_line() +
    facet_grid(method ~ ., scales = 'free_y') + theme_bw() +
    ggtitle("Software release A") +
    theme(panel.spacing = unit(0.2, "lines")) +
    geom_vline(aes(xintercept=changeP), data=changePoints, linetype="longdash", colour=c(rep("cyan3",length(chg_mswm_L16A)),rep("orangered",length(out_L16A))))


###############
# ecp
# g2 L17A
###############
set.seed(1)
Ediv_L17A <- e.divisive(matrix(train_g2_L17A$TotCpu), R=499, min.size=5)
Ediv_L17A$k.hat
Ediv_L17A$estimates
out_L17A <- Ediv_L17A$estimates[c(-1,-length(Ediv_L17A$estimates))]

dat <- data.frame(index=seq(1,nrow(train_g2_L17A)), TotCpu=train_g2_L17A$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() +
  geom_vline(xintercept=out_L17A, colour="red", linetype="dashed") +
  ggtitle("E-divisive L17A") + theme_bw()

#---------------------------------------------------------------------------#
# compare
L17A_3 <- as.data.frame(mswm_L17A_NNN@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
g <- ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_NNN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

g + geom_vline(xintercept=out_L17A, color="black", size=0.6, linetype="longdash")


state_L17A_3 <- gen(mswm_L17A_NNN, train_g2_L17A)
g2 <- ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A_NNN") + theme_bw()


g2 + geom_vline(xintercept=out_L17A, color="red", size=0.6, linetype="dashed")


#--------------------------------------#
pred_state_L17A <- sapply(1:nrow(train_g2_L17A), function(x) which.max(mswm_L17A_NNN@Fit@smoProb[x,]))
chg_mswm_L17A <- which(diff(pred_state_L17A) != 0) + 1

ind <- nrow(train_g2_L17A)
method <- c(rep("Markov switching model",ind),rep("E-divisive",ind))
changePoints <- data.frame(changeP=c(chg_mswm_L17A, out_L17A), method=c(rep("Markov switching model",length(chg_mswm_L17A)), rep("E-divisive",length(out_L17A))))
temp <- data.frame(index=rep(1:ind,2),y=rep(train_g2_L17A$TotCpu,2), method)
temp$method <- factor(temp$method, levels=c("Markov switching model","E-divisive"))
ggplot(data=temp, aes(x=index,y=y)) + geom_line() +
  facet_grid(method ~ ., scales = 'free_y') + theme_bw() +
  ggtitle("Software release C") +
  theme(panel.spacing = unit(0.2, "lines")) +
  geom_vline(aes(xintercept=changeP), data=changePoints, linetype="longdash", colour=c(rep("cyan3",length(chg_mswm_L17A)),rep("orangered",length(out_L17A))))




