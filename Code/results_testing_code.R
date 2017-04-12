###############
# OLD simulation
###############
# set.seed(1)
# n <- 400
# dat <- data.frame(y=runif(n,0,300),
#                   x1=runif(n,0,300),
#                   x2=runif(n,0,1400),
#                   x3=runif(n,0,100),
#                   x4=factor(sample(x=c("A","B"), size=n, replace=TRUE, prob=c(0.1,0.9))),
#                   x5=factor(sample(x=c("F","T"), size=n, replace=TRUE, prob=c(0.8,0.2))),
#                   x6=factor(sample(x=c("1","2","3"), size=n, replace=TRUE, prob=c(0.7,0.2,0.1))),
#                   state=sample(x=c("normal","good","bad"), size=n, replace=TRUE, prob=c(0.4,0.3,0.3)))
#
# dat <- data.frame(x1=runif(n,50,300),
#                   x2=sample(x=c(rep(0,0.5*n),runif(0.5*n,1200,1500))),
#                   x3=sample(x=c(rep(0,0.8*n),runif(0.2*n,40,60))),
#                   state=sample(x=c("normal","good","bad"), size=n, replace=TRUE, prob=c(0.4,0.3,0.3)))
# y <- 100 + 0.2*dat$x1
# dat <- data.frame(dat,y)
#
# # y_1 <- c(NA,dat$y[-n])
# # dat <- cbind(dat,y_1) # first obs will be gone
#
# ind <- which(colnames(dat) == "state")
# train_state <- dat[1:360,]
# train <- train_state[,-ind]
# test_state <- dat[-c(1:360),]
# test <- test_state[,-ind]
#
# mod <- lm(y~., data=train)
# summary(mod)
#
# ##
# k=3
# p=1
# sw=rep(T,length(mod$coefficients)+p+1)
# control=list(trace = T,  maxiter = 500, tol = 1e-8, maxiterInner=10, maxiterOuter=5, parallelization=F)
# data <- train
# object <- mod
# ##
#
# set.seed(1)
# mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
# summary(mswm)
#
#
# pred <- predict(mswm, test)
#
#
# #---------------------------------------------------#
# n <- 400
# b0 <- 22
# b1 <- 0.8
# b2 <- -0.01
# b3 <- 0.6
# sigma <- 10
#
# set.seed(1)
# x1 <- runif(n,50,300)
# x2 <- sample(x=c(rep(0,0.5*n),runif(0.5*n,1300,1500)))
# x3 <- sample(x=c(rep(0,0.8*n),runif(0.2*n,40,60)))
# # state <- sample(x=c("normal","good","bad"), size=n, replace=TRUE, prob=c(0.4,0.3,0.3))
# # eps <- rnorm(n, sd=sigma)
# # y <- 10 + b1*x1 + b2*x2 + b3*x3 + eps
#
# dat <- data.frame(x1,x2,x3)
#
# ind <- which(colnames(dat) == "state")
# train_state <- dat[1:360,]
# train <- train_state[,-ind]
# test_state <- dat[-c(1:360),]
# test <- test_state[,-ind]
#
# mod <- lm(y~., data=train)
# summary(mod)
#
# set.seed(1)
# mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
# summary(mswm)


#---------------------------------------------------#

###############
# TRY out something
###############
# col <- c("light pink","light blue","light green")
# apply(as.matrix(1:k),1,function(i){
#   y=x["Fit"]["smoProb"][-1,i]
#   val=cbind(which(diff(c(0,findInterval(y,0.5)))==1),which(diff(c(findInterval(y,0.5),0))==-1))
#   apply(val,1,function(el) rect(el[1],min(z),el[2],max(z),col=col[i],border=NA))
# }
# )
# par(new=T,las=1,bty="o",yaxt="n")
# plot(ts(z),col=1,ylim=c(min(z),max(z)),xlab="",ylab="")
# par(las=1,yaxt="n")
# par(las=3,yaxt="s")
# mtext(names(z),side=2,line=2.5,col=1)
# axis(side=4)
# 
# 
# # normal plot
# x <- mswm_L16B_3
# z=x["model"]$model[1]
# state <- sapply(1:nrow(train_g2_L16B), function(x) which.max(mswm_L16B_3@Fit@smoProb[x,]))
# state <- factor(state)
# index=seq(1,nrow(train_g2_L16B))
# bf=index-0.5
# af=index+0.5
# state_L16B_3 <- data.frame(index,bf,af,state,y=train_g2_L16B$TotCpu)
# col <- c("light pink","light blue","light green")
# plot(0,type="l",xlim=c(1,length(t(z))),ylim=c(min(z),max(z)),xlab=paste(names(z),"vs. Smooth Probabilities"),ylab="")
# apply(state_L16B_3,1,function(el) rect(el[2],min(z),el[3],max(z),col=col[el[4]],border=NA))
# par(new=T,las=1,bty="o",yaxt="n")
# plot(ts(z))
# 
# 
# state <- sapply(1:nrow(train_g2_L16B), function(x) which.max(mswm_L16B_3@Fit@smoProb[x,]))
# state <- factor(state)
# index=seq(1,nrow(train_g2_L16B))
# xmin=index-0.5
# xmax=index+0.5
# y=train_g2_L16B$TotCpu
# state_L16B_3 <- data.frame(index,xmin,xmax,state,y=y,ymin=min(y),ymax=max(y))
# 
# 
# # ggplot2 with annotation
# col <- c("red","green","blue")
# add_state <- function(x){
#   annotate("rect", xmin=state_L16B_3[x,2], xmax=state_L16B_3[x,3], ymin=min(z), ymax=max(z), alpha=0.2, fill=col[state_L16B_3[x,4]])
# }
# 
# ln <- lapply(1:nrow(state_L16B_3),add_state)
# ggplot(state_L16B_3, aes(x=index, y=y)) + geom_line() +
#   ylab("TotCpu") + theme_bw() + ln + scale_colour_manual(name="",values=c("red"="red","green"="green","blue"="blue"),labels=c("State 1", "State 2", "State 3"))
# 

