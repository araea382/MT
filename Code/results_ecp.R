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


