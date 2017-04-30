library(MSwM2)
library(ggplot2)

# simulation data
# three states
set.seed(1)
n <- 500
x1 <- runif(n,50,200)
x2 <- runif(n,0,50)
e1 <- rnorm(n,0,1)
e2 <- rnorm(n,2,0.5)
e3 <- rnorm(n,1,1)
y0 <- 0

y1 <- 10 + 0.6*x1 - 0.9*x2 + e1
y0 <- c(0,y1[-n])
y1 <- y1 + 0.5*y0

y2 <- 2 + 0.8*x1 + e2
y0 <- c(0,y2[-n])
y2 <- y2 + 0.2*y0

y3 <- -12 + 0.7*x1 + 0.2*x2 + e3
y0 <- c(0,y3[-n])
y3 <- y3 - 0.2*y0

plot(y1, type="l", ylim=c(0,300)) # normal
points(y2, type="l", col="red") # bad
points(y3, type="l", col="orange") # good

#--------------------------------------------------------------------#
# Simulated Dataset 1
state <- rep(0,n)
ind_normal <- c(1:50,71:110,161:220,351:370,421:450)
ind_bad <- c(51:70,241:290,371:420,451:491)
ind_good <- c(111:160, 221:240,291:350,491:500)
state[ind_normal] <- "Normal"
state[ind_bad] <- "Bad"
state[ind_good] <- "Good"

st <- rep(0,n)
st[ind_normal] <- 1
st[ind_bad] <- 2
st[ind_good] <- 3

chg <- which(diff(st) != 0) + 1
# 51  71 111 161 221 241 291 351 371 421 451 491

y <- rep(0,n)
y[ind_normal] <- y1[ind_normal]
y[ind_bad] <- y2[ind_bad]
y[ind_good] <- y3[ind_good]

points(y, type="l", col="green")
plot(y, type="l")
abline(v=chg,col="red")

ggplot(data.frame(index=seq(1:n),y), aes(x=index, y=y)) + geom_line() +
  ggtitle("Simulated data")+ theme_bw()

simu_data <- data.frame(x1,x2,y)

ind <- 500*0.8
train <- simu_data[1:ind,]
test <- simu_data[-c(1:ind),]

chg_train <- chg[-which(chg > 400)]

mod <- lm(y~., data=train)
summary(mod)

set.seed(1)
mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
summary(mswm)

plotProb(mswm)
# regime1 = bad (2)
# regime2 = normal (1)
# regime3 = good (3)

pred <- MSwM2::statePredict(mswm,test)
test_state <- state[-c(1:ind)]
test_state[which(test_state == "Bad")] <- 1
test_state[which(test_state == "Normal")] <- 2
test_state[which(test_state == "Good")] <- 3

tab <- table(actual=test_state, predict=pred)

sum(diag(tab))/sum(tab) # overall accuracy
1-sum(diag(tab))/sum(tab) # incorrect classification

library(caret)
confusionMatrix(test_state, pred)

# Y <- as.factor(state)
# Y <- factor(Y,levels(Y)[c(2,3,1)])
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# 
# temp2 <- data.frame(Y,X=seq(1:500))
# ggplot() + geom_point(data=temp2, aes(x=X, y=Y, colour=Y)) +
#   xlab("index") + ylab("") + theme_bw() +
#   theme(legend.title = element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   ggtitle("State of the simulated data") +
#   scale_colour_manual(values=cbPalette)
# 
# p1<-ggplot(data.frame(index=seq(1:n),y), aes(x=index, y=y)) + geom_line() +
#     ggtitle("Simulated Dataset 1")+ theme_bw()
# 
# p2<-ggplot() + geom_bar(data=temp2, aes(x=X, y=Y, fill=Y, color=Y),stat="identity") +
#     xlab("index") + ylab("") + theme_bw() +
#     theme(legend.title = element_blank(),
#           axis.ticks.y=element_blank(),
#           axis.text.y=element_text(colour="white",size=5),
#           legend.position="bottom") +
#     ggtitle("State of the the Dataset 1")
# 
# require(gridExtra)
# grid.arrange(p1,p2,nrow=2)
# 
# #--------------------------------#
# # MSwM2
# # smoothed prob plot
# sim <- as.data.frame(mswm@Fit@smoProb)
# sim <- cbind(index=seq(1,nrow(sim)),sim)
# colnames(sim) <- c("index","State 1","State 2","State 3")
# 
# sim <- melt(sim, id="index")
# ggplot(data=sim, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("Simulated Dataset 1") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# 
# # plot with state area
# gen_sim <- function(object,data){
#   state <- sapply(1:nrow(data), function(x) which.max(object@Fit@smoProb[x,]))
#   state <- factor(state)
#   index=seq(1,nrow(data))
#   xmin=index-0.5
#   xmax=index+0.5
#   y=data$y
#   ans <- data.frame(index,xmin,xmax,state,y=y,ymin=min(y),ymax=max(y))
#   return(ans)
# }

pred_state <- sapply(1:nrow(train), function(x) which.max(mswm@Fit@smoProb[x,]))
chg_mswm <- which(diff(pred_state) != 0) + 1

# state_sim <- gen_sim(mswm, train)
# ggplot(data=state_sim, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_sim, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("y") + ggtitle("Simulated Dataset 1") + theme_bw()
# 
# 
# #--------------------------------#
# # ecp
# set.seed(1)
# Ediv_sim <- e.divisive(matrix(train$y), R=499, min.size=5)
# Ediv_sim$k.hat
# Ediv_sim$estimates
# out <- Ediv_sim$estimates[c(-1,-length(Ediv_sim$estimates))] # 112 162 221 241 285 353
# 
# dat <- data.frame(index=seq(1,nrow(train)), y=train$y)
# ggplot(data=dat, aes(x=index, y=y)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   geom_vline(xintercept=out, colour="red", linetype="longdash") +
#   geom_vline(xintercept=chg_train, colour="purple", linetype="longdash") +
#   ggtitle("E-divisive simulated Dataset 1") + theme_bw()
# 
# 
# #--------------------------------#
# # smo prob
# # quite difficult to see...
# g <- ggplot(data=sim, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("Simulated Dataset 1") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# g + geom_vline(xintercept=out, color="red", size=0.6, linetype="longdash") +
#   geom_vline(xintercept=chg_train, colour="blue", size=0.6, linetype="longdash")
# 
# 
# 
# # state
# # easier to see but there are switches that are missing
# g <- ggplot(data=state_sim, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_sim, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("t") + ggtitle("Simulated Dataset 1") + theme_bw()
# 
# g + geom_vline(xintercept=out, color="red", linetype="longdash") +
#   geom_vline(xintercept=chg_train, colour="blue", linetype="longdash")
# 
# 
# # TRUE state
# # two methods overlap.... ugh.. almost
# temp <- data.frame(index=seq(1, nrow(train)), y=train$y, state=state[1:ind])
# index=seq(1,nrow(train))
# state_sim_train <- data.frame(index,xmin=index-0.5,xmax=index+0.5,state=state[1:ind],y=train$y,ymin=min(train$y),ymax=max(train$y))
# 
# ggplot(data=state_sim_train, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_sim_train, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("y") + ggtitle("Simulated Dataset 1") + theme_bw() +
#   geom_vline(xintercept=out, color="magenta", linetype="longdash") +
#   geom_vline(xintercept=chg_mswm, colour="blue", linetype="longdash")



# three plots at once
# yippie
method <- c(rep("True",ind),rep("Markov switching model",ind),rep("E-divisive",ind))
changePoints <- data.frame(changeP=c(chg_train, chg_mswm, out), method=c(rep("True",length(chg_train)), rep("Markov switching model",length(chg_mswm)), rep("E-divisive",length(out))))
temp <- data.frame(index=rep(1:ind,3),y=rep(train$y,3), method)
ggplot(data=temp, aes(x=index,y=y)) + geom_line() +
  facet_grid(method ~ ., scales = 'free_y') + theme_bw() +
  ggtitle("Simulated Dataset 1") +
  theme(panel.spacing = unit(0.2, "lines")) +
  geom_vline(aes(xintercept=changeP), data=changePoints, linetype="longdash", colour=c(rep("orangered",length(out)),rep("cyan3",length(chg_mswm)),rep("limegreen",length(chg_train))))


.no <- function(){
#--------------------------------#
# d <- data.frame(y,temp2)
# colnames(d) <- c("value","state","index")
# ggplot(d, aes(x=index, y=value)) + geom_line() +
#     facet_grid(value~ ., scales = "free_y") + theme(legend.position = "none")


# Y_test <- state[401:500]
# temp3 <- data.frame(Y_test,X=seq(1:100))
# ggplot() + geom_point(data=temp3, aes(x=X, y=Y_test, colour=Y_test)) +
#   xlab("index") + ylab("") + theme_bw() +
#   theme(legend.title = element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   ggtitle("State of the simulated data") +
#   scale_colour_manual(values=cbPalette)
#
# pred[which(pred == 1)] <- "Bad"
# pred[which(pred == 2)] <- "Normal"
# pred[which(pred == 3)] <- "Good"
# pred <- as.factor(pred)
# temp4 <- data.frame(pred,X=seq(1:100))
# ggplot() + geom_point(data=temp4, aes(x=X, y=pred, colour=pred)) +
#   xlab("index") + ylab("") + theme_bw() +
#   theme(legend.title = element_blank(),
#         axis.text.y=element_blank(),
#         axis.ticks.y=element_blank()) +
#   ggtitle("State of the simulated data") +
#   scale_colour_manual(values=cbPalette)


##
# test with one obs. at a time
# pred1 <- MSwM2::predict(mswm,test[1,])
# pred2 <- MSwM2::predict(mswm,test[2,]); pred2 # PROBLEM..!
# pred12 <- MSwM2::predict(mswm,test[1:2,]); pred12 # It's okay
}

#--------------------------------------------------------------------#
# Simulated Dataset 2
set.seed(1)
state2 <- rep(0,n)
samp <- sample(3,500,replace=TRUE)
ind_normal2 <- which(samp == 1); length(ind_normal2)
ind_bad2 <- which(samp == 2); length(ind_bad2)
ind_good2 <- which(samp == 3); length(ind_good2)
state2[ind_normal2] <- "Normal"
state2[ind_bad2] <- "Bad"
state2[ind_good2] <- "Good"

st2 <- rep(0,n)
st2[ind_normal2] <- 1
st2[ind_bad2] <- 2
st2[ind_good2] <- 3

chg2 <- which(diff(st2) != 0) + 1

yy <- rep(0,n)
yy[ind_normal2] <- y1[ind_normal2]
yy[ind_bad2] <- y2[ind_bad2]
yy[ind_good2] <- y3[ind_good2]

plot(yy, type="l")

ggplot(data.frame(index=seq(1:n),yy), aes(x=index, y=yy)) + geom_line() +
  ggtitle("Simulated data")+ theme_bw()

simu_data2 <- data.frame(x1,x2,yy)

train2 <- simu_data2[1:ind,]
test2 <- simu_data2[-c(1:ind),]

chg_train2 <- chg2[-which(chg2 > 400)]

mod2 <- lm(yy~., data=train2)
summary(mod2)

set.seed(1)
mswm2 <- MSwM2::msmFit(mod2, k=3, p=1, sw=rep(TRUE,length(mod2$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
summary(mswm2)

plotProb(mswm2)
# regime1 = bad (2)
# regime2 = good (3)
# regime3 = normal (1)

# pred_train_state <- apply(mswm2@Fit@smoProb,1,which.max)
# st <- samp[1:ind]
# st[which(st == 1)] <- 4
# st[which(st == 3)] <- 1
# st[which(st == 4)] <- 2

pred2 <- MSwM2::statePredict(mswm2,test2)
test_state2 <- state2[-c(1:ind)]
test_state2[which(test_state2 == "Bad")] <- 1
test_state2[which(test_state2 == "Normal")] <- 3
test_state2[which(test_state2 == "Good")] <- 2

tab2 <- table(actual=test_state2, predict=pred2)

sum(diag(tab2))/sum(tab2) # overall accuracy
1-sum(diag(tab2))/sum(tab2) # incorrect classification

# 
# YY <- as.factor(state2)
# YY <- factor(YY,levels(YY)[c(2,3,1)])
# 
# temp2 <- data.frame(YY,X=seq(1:500))
# 
# # plot(x=seq(1,500),y=samp, type="h")
# pp1 <- ggplot(data.frame(index=seq(1:n),yy), aes(x=index, y=yy)) + geom_line() +
#   ggtitle("Simulated Dataset 2")+ theme_bw()
# pp2 <- ggplot() + geom_bar(data=temp2, aes(x=X, y=YY, fill=YY, color=YY),stat="identity") +
#   xlab("index") + ylab("") + theme_bw() +
#   theme(legend.title = element_blank(),
#                      axis.ticks.y=element_blank(),
#                      axis.text.y=element_text(colour="white",size=5),
#                      legend.position="bottom") +
#   ggtitle("State of the Dataset 2")
# 
# 
# grid.arrange(pp1,pp2,nrow=2)
# 
# 
# #--------------------------------#
# # MSwM2
# # smoothed prob plot
# sim2 <- as.data.frame(mswm2@Fit@smoProb)
# sim2 <- cbind(index=seq(1,nrow(sim2)),sim2)
# colnames(sim2) <- c("index","State 1","State 2","State 3")
# 
# sim2 <- melt(sim2, id="index")
# ggplot(data=sim2, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("Simulated Dataset 2") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# 
# # plot with state area
# gen_sim2 <- function(object,data){
#   state <- sapply(1:nrow(data), function(x) which.max(object@Fit@smoProb[x,]))
#   state <- factor(state)
#   index=seq(1,nrow(data))
#   xmin=index-0.5
#   xmax=index+0.5
#   y=data$yy
#   ans <- data.frame(index,xmin,xmax,state,y=y,ymin=min(y),ymax=max(y))
#   return(ans)
# }

pred_state2 <- sapply(1:nrow(train2), function(x) which.max(mswm2@Fit@smoProb[x,]))
chg_mswm2 <- which(diff(pred_state2) != 0) + 1

# state_sim2 <- gen_sim2(mswm2, train2)
# ggplot(data=state_sim2, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_sim2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("y") + ggtitle("Simulated Dataset 2") + theme_bw()
# 
# 
# #--------------------------------#
# # ecp
# set.seed(1)
# Ediv_sim2 <- e.divisive(matrix(train2$yy), R=499, min.size=5)
# Ediv_sim2$k.hat
# Ediv_sim2$estimates
# out2 <- Ediv_sim2$estimates[c(-1,-length(Ediv_sim2$estimates))] # 270 300
# 
# dat <- data.frame(index=seq(1,nrow(train2)), y=train2$yy)
# ggplot(data=dat, aes(x=index, y=y)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   geom_vline(xintercept=out2, colour="red", linetype="longdash") +
#   ggtitle("E-divisive simulated Dataset 2") + theme_bw()
# 
# 
# #--------------------------------#
# # smo prob
# # quite difficult to see... OMG!!!!!!!!!!
# g <- ggplot(data=sim2, aes(x=index, y=value, colour=variable)) + geom_line() +
#   ylab("Smoothed Probabilities") + ggtitle("Simulated Dataset 2") + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
#   theme_bw() + theme(legend.title = element_blank())
# 
# g + geom_vline(xintercept=out2, color="red", size=0.6, linetype="longdash") +
#   geom_vline(xintercept=chg_train2, colour="blue", size=0.6, linetype="longdash")
# 
# 
# 
# # state
# # OMG!!!!!!!!! no different than the first one
# g <- ggplot(data=state_sim2, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_sim2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("t") + ggtitle("Simulated Dataset 2") + theme_bw()
# 
# g + geom_vline(xintercept=out2, color="red", linetype="longdash") +
#   geom_vline(xintercept=chg_train2, colour="blue", linetype="longdash")
# 
# 
# # TRUE state
# # OMG!!!!!!!!! no different than the first and second one
# temp <- data.frame(index=seq(1, nrow(train2)), y=train2$yy, state=state2[1:ind])
# index=seq(1,nrow(train2))
# state_sim_train2 <- data.frame(index,xmin=index-0.5,xmax=index+0.5,state=state2[1:ind],y=train2$yy,ymin=min(train2$yy),ymax=max(train2$yy))
# 
# ggplot(data=state_sim_train2, aes(x=index, y=y)) + geom_line() +
#   geom_rect(data=state_sim_train2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
#   scale_fill_manual(values=c("red","green","blue")) +
#   ylab("y") + ggtitle("Simulated Dataset 2") + theme_bw() +
#   geom_vline(xintercept=out2, color="magenta", linetype="longdash") +
#   geom_vline(xintercept=chg_mswm2, colour="blue", linetype="longdash")



# three plots at once
# uhh... still so scary
method <- c(rep("True",ind),rep("Markov switching model",ind),rep("E-divisive",ind))
changePoints <- data.frame(changeP=c(chg_train2, chg_mswm2, out2), method=c(rep("True",length(chg_train2)), rep("Markov switching model",length(chg_mswm2)), rep("E-divisive",length(out2))))
temp2 <- data.frame(index=rep(1:ind,3),y=rep(train2$yy,3), method)
ggplot(data=temp2, aes(x=index,y=y)) + geom_line() +
  facet_grid(method ~ ., scales = 'free_y') + theme_bw() +
  ggtitle("Simulated Dataset 2") +
  theme(panel.spacing = unit(0.2, "lines")) +
  geom_vline(aes(xintercept=changeP), data=changePoints, linetype="longdash", colour=c(rep("orangered",length(out2)),rep("cyan3",length(chg_mswm2)),rep("limegreen",length(chg_train2))))



