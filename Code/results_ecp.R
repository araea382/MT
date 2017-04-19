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
  geom_vline(xintercept=out, colour="red", linetype="longdash") +
  ggtitle("E-divisive L16B") + theme_bw()


ecp_L16B <- c(130,135,153,170)

L16B_3 <- as.data.frame(mswm_L16B_NYY@Fit@smoProb)
L16B_3 <- cbind(index=seq(1,nrow(L16B_3)),L16B_3)
colnames(L16B_3) <- c("index","State 1","State 2","State 3")

L16B_3 <- melt(L16B_3, id="index")
g <- ggplot(data=L16B_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L16B_NYY") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

g + geom_vline(xintercept=ecp_L16B, color="black", size=0.6, linetype="longdash")


state_L16B_3 <- gen(mswm_L16B_NYY, train_g2_L16B)
g2 <- ggplot(data=state_L16B_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L16B_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L16B_NYY") + theme_bw()

g2 + geom_vline(xintercept=ecp_L16B, color="red", size=0.6, linetype="longdash")





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


ecp_L17A <- c(9,77,82,105)

L17A_3 <- as.data.frame(mswm_L17A_NNN@Fit@smoProb)
L17A_3 <- cbind(index=seq(1,nrow(L17A_3)),L17A_3)
colnames(L17A_3) <- c("index","State 1","State 2","State 3")

L17A_3 <- melt(L17A_3, id="index")
g <- ggplot(data=L17A_3, aes(x=index, y=value, colour=variable)) + geom_line() +
    ylab("Smoothed Probabilities") + ggtitle("L17A_NNN") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
    theme_bw() + theme(legend.title = element_blank())

g + geom_vline(xintercept=ecp_L17A, color="black", size=0.6, linetype="longdash")


state_L17A_3 <- gen(mswm_L17A_NNN, train_g2_L17A)
g2 <- ggplot(data=state_L17A_3, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_L17A_3, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A_NNN") + theme_bw()


g2 + geom_vline(xintercept=ecp_L17A, color="red", size=0.6, linetype="dashed")

