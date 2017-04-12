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

state <- rep(0,n)
ind_normal <- c(1:50,71:110,161:220,351:370,421:450)
ind_bad <- c(51:70,241:290,371:420,451:491)
ind_good <- c(111:160, 221:240,291:350,491:500)
state[ind_normal] <- "Normal"
state[ind_bad] <- "Bad"
state[ind_good] <- "Good"

y <- rep(0,n)
y[ind_normal] <- y1[ind_normal]
y[ind_bad] <- y2[ind_bad]
y[ind_good] <- y3[ind_good]

points(y, type="l", col="green")
plot(y, type="l")

ggplot(data.frame(index=seq(1:n),y), aes(x=index, y=y)) + geom_line() +
  ggtitle("Simulated data")+ theme_bw()

simu_data <- data.frame(x1,x2,y)

ind <- 500*0.8
train <- simu_data[1:ind,]
test <- simu_data[-c(1:ind),]
mod <- lm(y~., data=train)
summary(mod)

set.seed(1)
mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
summary(mswm)

plotProb(mswm)
# regime1 = bad
# regime2 = normal
# regime3 = good

pred <- MSwM2::predict(mswm,test)
test_state <- state[-c(1:ind)]
test_state[which(test_state == "Bad")] <- 1
test_state[which(test_state == "Normal")] <- 2
test_state[which(test_state == "Good")] <- 3

tab <- table(actual=test_state, predict=pred)

sum(diag(tab))/sum(tab) # overall accuracy
1-sum(diag(tab))/sum(tab) # incorrect classification

library(caret)
result <- confusionMatrix(test_state, pred)


# plot state
# X_normal <- rep(NA,n)
# X_good <- rep(NA,n)
# X_bad <- rep(NA,n)
# X_normal[ind_normal] <- ind_normal
# X_good[ind_good] <- ind_good
# X_bad[ind_bad] <- ind_bad

Y <- as.factor(state)
Y <- factor(Y,levels(Y)[c(1,3,2)])

# temp <- data.frame(Y,X_normal,X_good,X_bad)
# ggplot() + geom_point(data=temp, aes(x=X_normal, y=Y)) +
#   geom_point(data=temp, aes(x=X_good, y=Y)) +
#   geom_point(data=temp, aes(x=X_bad, y=Y)) +
#   xlab("index") + ylab("") + labs(y="") + theme_bw()

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

temp2 <- data.frame(Y,X=seq(1:500))
ggplot() + geom_point(data=temp2, aes(x=X, y=Y, colour=Y)) +
  xlab("index") + ylab("") + theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("State of the simulated data") +
  scale_colour_manual(values=cbPalette)


Y_test <- state[401:500]
temp3 <- data.frame(Y_test,X=seq(1:100))
ggplot() + geom_point(data=temp3, aes(x=X, y=Y_test, colour=Y_test)) +
  xlab("index") + ylab("") + theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("State of the simulated data") +
  scale_colour_manual(values=cbPalette)

pred[which(pred == 1)] <- "Bad"
pred[which(pred == 2)] <- "Normal"
pred[which(pred == 3)] <- "Good"
pred <- as.factor(pred)
temp4 <- data.frame(pred,X=seq(1:100))
ggplot() + geom_point(data=temp4, aes(x=X, y=pred, colour=pred)) +
  xlab("index") + ylab("") + theme_bw() +
  theme(legend.title = element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("State of the simulated data") +
  scale_colour_manual(values=cbPalette)


# library(pROC)
# plot(multiclass.roc(pred, test_state))

##
# test with one obs. at a time
pred1 <- MSwM2::predict(mswm,test[1,])
pred2 <- MSwM2::predict(mswm,test[2,]); pred2 # PROBLEM..!
pred12 <- MSwM2::predict(mswm,test[1:2,]); pred12 # It's okay


#####
## accuracy = 1
# y1 <- -12 + 0.7*x1 + 0.2*x2 + e3
# y0 <- c(0,y1[-n])
# y1 <- y1 - 0.2*y0
#
# y2 <- 10 + 0.6*x1 - 0.9*x2 + e1
# y0 <- c(0,y2[-n])
# y2 <- y2 + 0.5*y0
#
# y3 <- 2 + 0.8*x1 + e2
# y0 <- c(0,y2[-n])
# y3 <- y3 + 0.2*y0
#
#
# plot(y2, type="l", ylim=c(0,300)) # normal
# points(y3, type="l", col="red") # bad
# points(y1, type="l", col="orange") # good
#
# state <- rep(0,n)
# ind_normal <- c(1:50,71:110,161:220,351:370,421:450)
# ind_bad <- c(51:70,241:290,371:420,451:491)
# ind_good <- c(111:160, 221:240,291:350,491:500)
# state[ind_normal] <- "normal"
# state[ind_bad] <- "bad"
# state[ind_good] <- "good"
#
# y <- rep(0,n)
# y[ind_normal] <- y2[ind_normal]
# y[ind_bad] <- y3[ind_bad]
# y[ind_good] <- y1[ind_good]
#
# points(y, type="l", col="green")
# plot(y, type="l")
#
# simu_data <- data.frame(x1,x2,y)
#
# ind <- 500*0.8
# train <- simu_data[1:ind,]
# test <- simu_data[-c(1:ind),]
# mod <- lm(y~., data=train)
# summary(mod)
#
# set.seed(1)
# mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
# summary(mswm)
#
# plotProb(mswm)
# # regime1 = bad
# # regime2 = normal
# # regime3 = good
#
# pred <- MSwM2::predict(mswm,test)
# test_state <- state[-c(1:ind)]
# test_state[which(test_state == "good")] <- 1
# test_state[which(test_state == "normal")] <- 2
# test_state[which(test_state == "bad")] <- 3
#
# tab <- table(actual=test_state, predict=pred)
#
# sum(diag(tab))/sum(tab) # overall accuracy
# 1-sum(diag(tab))/sum(tab) # incorrect classification
####
