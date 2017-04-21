library(MSwM2)
library(ggplot2)

pred_L16A <- MSwM2::statePredict(mswm_L16A_NN, test_g2_L16A) ##FIX
# Note:
# last observation NaN because Likl is [0,0,0] -> fProb is 0/0
# try model mswm_16A_3 -> works fine... probably
# try using whole dataset for NN -> still works fine 
# smoProb
# [59,] 0.01010105  5.188710e-04  9.893801e-01
# [60,] 0.22192155  7.221746e-81  7.780784e-01
# [61,] 1.00000000 9.175131e-143 2.992164e-225
# [62,] 1.00000000  0.000000e+00 1.113157e-160
# [63,] 1.00000000  0.000000e+00  0.000000e+00
# [64,] 1.00000000  0.000000e+00  0.000000e+00




#----------------------------------------------------------------#

pred_L16B <- MSwM2::statePredict(mswm_L16B_NYY, test_g2_L16B)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 
# 1  1  1  1  1  3  1  3  3  3  1  3  3  3  2  3  3  3  3  3  3  3  1  3  1

# plot with state area
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

gen2 <- function(x,dat,data){
    state <- factor(x)
    index=seq(1,nrow(data))
    xmin=index-0.5
    xmax=index+0.5
    y=data$TotCpu
    ymin=ifelse(unique(dat$ymin) > min(y), min(y), unique(dat$ymin))
    ymax=ifelse(unique(dat$ymax) > max(y), unique(dat$ymax), max(y))
    n=nrow(data)
    ans <- data.frame(index,xmin,xmax,state,y=y,ymin=rep(ymin,n),ymax=rep(ymax,n))
    return(ans)
}

state_L16B_3 <- gen(mswm_L16B_NYY, train_g2_L16B)
state_test_L16B <- gen2(pred_L16B, state_L16B_3, test_g2_L16B)
ggplot(data=state_test_L16B, aes(x=index, y=y)) + geom_line() +
    geom_rect(data=state_test_L16B, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
    scale_fill_manual(values=c("red","green","blue")) + 
    ylab("TotCpu") + ggtitle("L16B") + theme_bw()


#----------------------------------------------------------------#

pred_L17A <- MSwM2::statePredict(mswm_L17A_NNN, test_g2_L17A)
# 1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 
# 1  1  1  2  1  3  2  3  3  2  2  2  2  2  2 

state_L17A_3 <- gen(mswm_L17A_NNN, train_g2_L17A)
state_test_L17A <- gen2(pred_L17A, state_L17A_3, test_g2_L17A)
ggplot(data=state_test_L17A, aes(x=index, y=y)) + geom_line() +
  geom_rect(data=state_test_L17A, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=state), alpha=0.2, inherit.aes=FALSE) +
  scale_fill_manual(values=c("red","green","blue")) + 
  ylab("TotCpu") + ggtitle("L17A") + theme_bw()



