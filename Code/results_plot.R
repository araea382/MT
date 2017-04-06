r <- as.data.frame(mswm_L16B_3@Fit@smoProb)
r <- cbind(index=seq(1,nrow(r)),r)
colnames(r) <- c("index","regime1","regime2","regime3")

plot(r1, type="l", col="red", ylim=c(0,1), ylab="Smoothed Probabilities", xlim=c(0,250), xlab="")
points(r2, type="l", col="green")
points(r3, type="l", col="blue")
legend(x=220,y=0.6,c("regime1","regime2","regime3"))

library(ggplot2)
ggplot(data=r, aes(index)) + geom_line(aes(y=V1, colour="V1")) + geom_line(aes(y=V2, colour="V2")) + geom_line(aes(y=V3, colour="V3"))

library(reshape2)
r <- melt(r, id="index")
ggplot(data=r, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + theme_bw() + theme(legend.title = element_blank())



r2 <- as.data.frame(mswm_L16B_2@Fit@smoProb)
r2 <- cbind(index=seq(1,nrow(r2)),r2)
r2 <- melt(r2, id="index")
ggplot(data=r2, aes(x=index, y=value, colour=variable)) + geom_line() +
  ylab("Smoothed Probabilities") + theme_bw()
