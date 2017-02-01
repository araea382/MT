# RUN EVERY TIME
setwd("~/Dropbox/Thesis/Data")
setwd("C:/Users/EARAEAM/Documents/Thesis/Data")

library(data.table)
library(gtools) # for sorting RW column
library(ecp)
library(cpm)
library(ggplot2)
library(dplyr)
library(depmixS4)
library(HiddenMarkov)
library(HMM)

#----------------------------------------------------------------------#
g2 <- fread("master-g2-17col.csv.txt")
g2_filter <- fread("master-g2.filtered.csv.txt")
colnames(g2_filter) <- colnames(g2)

g1_filter <- fread("master.filtered.ALL.csv.txt")

#----------------------------------------------------------------------#
# sort on multiple columns
# converting all character vectors to factors with mixedsorted sorted levels, and pass all vectors on to the standard order function
# http://stackoverflow.com/questions/20396582/order-a-mixed-vector-numbers-with-letters
multi.mixedorder <- function(..., na.last = TRUE, decreasing = FALSE){
  do.call(order, c(
    lapply(list(...), function(l){
      if(is.character(l)){
        factor(l, levels=mixedsort(unique(l)))
      } else {
        l
      }
    }),
    list(na.last = na.last, decreasing = decreasing)
  ))
}

#----------------------------------------------------------------------#
# sort by Release and SW column
g2_sort <- g2[multi.mixedorder(Release, SW),] 
g2_sort_filter <- g2_filter[multi.mixedorder(Release, SW),] 

g2_L16B <- g2_sort[which(Release == "L16B")]
g2_L16B_filter <- g2_sort_filter[which(Release == "L16B")]
g2_L16A <- g2_sort[which(Release == "L16A")]
g2_L17A <- g2_sort[which(Release == "L17A")]

#----------------------------------------------------------------------#
.explore_R15G <- function(){
g2_R15G <- g2_L16B[which(SW == "R15G")] # 3 test runs in raw data but only 2 test runs show in platypus
# write.table(g2_R15G$EventsPerSec, file="dat.txt")

# R15G = 208.5 # orange # 10.75.74.55
# R15G = 210.07 # green # 10.75.74.48
# R15G = 206.57 # not show # EventsPerSec most component is not around 165
}
#----------------------------------------------------------------------#
.explore_R12AK <- function(){
g2_green <- g2_L16B[which(NodeName == "10.75.74.48")]
g2_green_R12AK <- g2_green[which(SW == "R12AK")]
# write.table(g2_green_R12AK$EventsPerSec, file="dat-green.txt")

# R12AK = 206.82/ 205.47/ 205.43
# it seems like there is only the last test run which make it to the graph (at least the number is the same)
# first two test runs also have EventsPerSec component around 165. Do they make it or not??
## ANSWER: select the minimum value
}
#----------------------------------------------------------------------#
.explore_g1_filter <- function(){
  t <- filter(g1_filter, Release == "L17A")
  tt <- filter(t, eNB == "kienb2064")
  # cpu3 (?)
  tt2 <- filter(t, eNB == "kienb1058")
  
  # Average CPU Utilization %, LmMonitor, DUS41, L17A
  # Average CPU Utilization, LmCell/LmCentral, DUS41, L17A
}
#----------------------------------------------------------------------#
# sort ***LOOK and DECIDE again***
.sort <- function(){
# sort by Timestamp
g2_sort_time <- g2[order(Timestamp),]

# sort all three
g2_sort_all <- g2[multi.mixedorder(Timestamp, Release, SW),] 
}
#----------------------------------------------------------------------#
# change SW from character to factor and set the factor levels to be the same as in factor labels
level <- unique(g2_L16B$SW)
g2_L16B$SW <- factor(g2_L16B$SW, levels=level)

level_filter <- unique(g2_L16B_filter$SW)
g2_L16B_filter$SW <- factor(g2_L16B_filter$SW, levels=level_filter)

# plot TotCpu% vs SW
plot(g2_L16B$SW, g2_L16B$`TotCpu%`, xlab="", main="Average CPU Utilisation g2_L16B") #
ggplot(data=g2_L16B, aes(SW, `TotCpu%`)) + geom_point()
ggplot(data=g2_L16B, aes(SW, `TotCpu%`)) + geom_boxplot()
plot(density(g2_L16B_filter$`TotCpu%`))

plot(g2_L16B_filter$SW, g2_L16B_filter$`TotCpu%`, xlab="", main="Average CPU Utilisation g2_L16B_filter") # compare with platypus
ggplot(data=g2_L16B_filter, aes(SW, `TotCpu%`)) + geom_point()
ggplot(data=g2_L16B_filter, aes(SW, `TotCpu%`)) + geom_boxplot() # getting the same result as in plot

#----------------------------------------------------------------------#
# t-test for g2 data L16B
.ttest <- function(){
# subset for each software package (SW)
tr <- list()
for(i in 1:length(level)){
  tr[[i]] <- subset(g2_L16B, SW==level[i])
}

t.test(tr[[2]]$`TotCpu%`,tr[[3]]$`TotCpu%`) # try t-test for one pair

# apply t-test by considering on one SW
t <- lapply(2:length(level), function(i){
  if(nrow(tr[[i-1]]) > 1 & nrow(tr[[i]]) > 1){
    t.test(tr[[i-1]]$`TotCpu%`, tr[[i]]$`TotCpu%`)
  } else "Only one observation in subset"
})

tr_no <- apply(1:length(level), function(i){
  nrow(tr[[i]])
})

tr_no <- data.frame(unlist(tr_no))

t[1:10] # investigate for first ten SW (no.2 is significant)
ggplot(data=g2_L16B[1:sum(tr_no[1:10,]),], aes(SW, `TotCpu%`)) + geom_point()
ggplot(data=g2_L16B[1:sum(tr_no[1:10,]),], aes(SW, `TotCpu%`)) + geom_boxplot()

sig <- c()
for(i in 1:(length(level)-1)){
  if(is.character(t[i][[1]])){}
  else{
      if(t[i][[1]]$p.value < 0.05){
      sig <- c(sig,i)
    }
  }
}

sig
length(sig) # 26 significant points
ggplot(data=g2_L16B, aes(SW, `TotCpu%`)) + geom_point() + geom_vline(xintercept=sig, linetype="dashed", color="red")


# 5 SW
t <- lapply(2:length(level), function(i){
  t.test(tr[[i-1]]$`TotCpu%`, tr[[i]]$`TotCpu%`)
})

tr2 <- list()
for(i in 5:length(level)){
  tr2[[i]] <- rbind(tr[[i-4]],tr[[i-3]],tr[[i-2]],tr[[i-1]],tr[[i]])
}

}

#----------------------------------------------------------------------#
# ecp package

# t1 <- Sys.time()
# t2 <- Sys.time()
# print(t2 - t1)

# g2 data L16B
# univariate: TotCpu%
# E-divisive
Ediv1 <- e.divisive(matrix(g2_L16B$`TotCpu%`), R=499, alpha=1) 
Ediv2 <- e.divisive(matrix(g2_L16B$`TotCpu%`), R=499, alpha=2) 

Ediv1$k.hat
# Ediv1$order.found
Ediv1$estimates

Ediv2$k.hat
# Ediv2$order.found
Ediv2$estimates
# Ediv2$considered.last
# Ediv2$p.values
# Ediv2$permutations

ts.plot(matrix(g2_L16B$`TotCpu%`), main="E-divisive g2_L16B, alpha=1")
abline(v=Ediv1$estimates[c(-1,-length(Ediv1$estimates))], col="red", lty=2)

ts.plot(matrix(g2_L16B$`TotCpu%`), main="E-divisive g2_L16B, alpha=2")
abline(v=Ediv2$estimates[c(-1,-length(Ediv2$estimates))], col="blue", lty=2)

#----------------------#
# E-agglo
mem <- c(rep(1:40, each=6), rep(41:212, each=5), rep(213:243, each=6)) # just make it up without any speical reason
Eagglo1 <- e.agglo(matrix(g2_L16B$`TotCpu%`), member=mem, alpha=1)
Eagglo3 <- e.agglo(matrix(g2_L16B$`TotCpu%`), member=mem, alpha=2) 

pen <- function(x) -length(x)
Eagglo2 <- e.agglo(matrix(g2_L16B$`TotCpu%`), member=mem, alpha=1, penalty=pen)

Eagglo1$estimates
tail(Eagglo1$fit, 5)
Eagglo1$progression
Eagglo1$merged

Eagglo2$estimates

ts.plot(matrix(g2_L16B$`TotCpu%`), main="E-agglo g2_L16B, alpha=1")
abline(v=Eagglo1$estimates, col="red", lty=2)

ts.plot(matrix(g2_L16B$`TotCpu%`), main="E-agglo g2_L16B, alpha=1, penalty")
abline(v=Eagglo2$estimates, col="blue", lty=2)

abline(v=Eagglo3$estimates, col="green", lty=2)

#----------------------#
# g2 FILTER data L16B
# univariate: TotCpu%
# E-divisive
Ediv1_filter <- e.divisive(matrix(g2_L16B_filter$`TotCpu%`), R=499, alpha=1) 
Ediv2_filter <- e.divisive(matrix(g2_L16B_filter$`TotCpu%`), R=499, alpha=2) 

Ediv1_filter$k.hat
# Ediv1_filter$order.found
Ediv1_filter$estimates

Ediv2_filter$k.hat
# Ediv2_filter$order.found
Ediv2_filter$estimates
# value is the same

ts.plot(matrix(g2_L16B_filter$`TotCpu%`), main="E-divisive g2_L16B_filter, alpha=1", ylim=c(50,300))
abline(v=Ediv1_filter$estimates[c(-1,-length(Ediv1_filter$estimates))], col="red", lty=2)

ts.plot(matrix(g2_L16B_filter$`TotCpu%`), main="E-divisive g2_L16B_filter, alpha=2")
abline(v=Ediv2_filter$estimates[c(-1,-length(Ediv2_filter$estimates))], col="blue", lty=2)

#----------------------------------------------------------------------#
# BreakoutDetection
# EDM: E-divisive median
.breakoutdetection <- function(){
library(BreakoutDetection)

data(Scribe)
res = breakout(Scribe, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot
plot(Scribe, type="l")

Ediv_scribe <- e.divisive(matrix(Scribe), R=499, alpha=1)
ts.plot(matrix(Scribe))
abline(v=Ediv_scribe$estimates, col="red", lty=2)


res2 <- breakout(g2_L16B$`TotCpu%`, method="multi", plot=TRUE)
res2$plot
}
#----------------------------------------------------------------------#
# cpm package
# g2 FILTER data L16B
# batch detection
.cpm <- function(){
resultsStudent <- detectChangePointBatch(g2_L16B_filter$`TotCpu%`, cpmType = "Student", alpha = 0.05)
resultsMW <- detectChangePointBatch(g2_L16B_filter$`TotCpu%`, cpmType = "Mann-Whitney", alpha = 0.05)
plot(g2_L16B_filter$`TotCpu%`, type = "l", xlab = "Observation", ylab = "x", bty = "l")
if (resultsStudent$changeDetected)
  abline(v = resultsStudent$changePoint, col="red", lty = 2) # student
if (resultsMW$changeDetected)
  abline(v = resultsMW$changePoint, col="blue", lty = 2) # mann-whitney

plot(resultsStudent$Ds, type = "l", xlab = "Observation", ylab = expression(D[t]), bty = "l")
abline(h = resultsStudent$threshold, lty = 2)

plot(resultsMW$Ds, type = "l", xlab = "Observation", ylab = expression(D[t]), bty = "l")
abline(h = resultsMW$threshold, lty = 2)

# sequential change detection
resultsStudent <- detectChangePoint(g2_L16B_filter$`TotCpu%`, cpmType = "Student", ARL0 = 500)
resultsMW <- detectChangePoint(g2_L16B_filter$`TotCpu%`, cpmType = "Mann-Whitney", ARL0 = 500)
plot(g2_L16B_filter$`TotCpu%`, type = "l", bty = "l")
if (resultsStudent$changeDetected)
  abline(v = resultsStudent$detectionTime, col = "red") # student
if (resultsMW$changeDetected)
  abline(v = resultsMW$detectionTime, col = "blue") # mann-whitney

# sequences containing multiple change points
res <- processStream(g2_L16B_filter$`TotCpu%`, cpmType = "Mann-Whitney", ARL0 = 500, startup = 20)
plot(g2_L16B_filter$`TotCpu%`, type = "l", xlab = "Observation", ylab = "", bty = "l")
abline(v = res$detectionTimes) # change was detected
abline(v = res$changePoints, lty = 2) # estimated change point locations
}
#----------------------------------------------------------------------#
# divide train/test set (70/30)
# g2 data L16B
.split <- function(){
length(unique(g2_L16B$SW)) # 243
filter(g2_L16B, SW == "R2AE") # try subset

sw_name <- unique(g2_L16B$SW)
g2_L16B_subset <- lapply(sw_name, function(x) filter(g2_L16B, SW == x))

train <- data.table()
test <- data.table()
subset <- lapply(1:length(g2_L16B_subset), function(x){
  if(nrow(g2_L16B_subset[[x]]) == 1){ # if there is only one obs then assign to train
    train <- g2_L16B_subset[[x]]
  }
  else{
    n <- nrow(g2_L16B_subset[[x]])
    index <- sample(1:n, size=round(0.3*n))
    test <- g2_L16B_subset[[x]][index,]
    train <- g2_L16B_subset[[x]][-index,]
  }
  temp <- list("train"=train, "test"=test)
})

unlist(lapply(1:243,function(x) nrow(subset[[x]]$train)))
unlist(lapply(1:243,function(x) nrow(subset[[x]]$test)))
# another way which might be better when changing the dataset
divide <- function(subset,x){
  if(nrow(subset[[x]]) == 1){ # if there is only one obs then assign to train
    train <- subset[[x]]
  }
  else{
    n <- nrow(g2_L16B_subset[[x]])
    index <- sample(1:n, size=round(0.3*n))
    test <- subset[[x]][index,]
    train <- subset[[x]][-index,]
  }
  temp <- list("train"=train, "test"=test)
  return(temp)
}
subset2 <- lapply(1:length(g2_L16B_subset), function(x) divide(g2_L16B_subset,x))

# combine all train and test from different subset together
for(i in 1:length(g2_L16B_subset)){
  train <- rbind(train, subset[[i]]$train)
  test <- rbind(test, subset[[i]]$test)
}

length(unique(train$SW)) # 154
length(unique(test$SW)) # 98
# there are 56 software package which only has one test run

# an attempt to divide train/test by using dplyr 
# train <- sample_n(g2_L16B_subset[[2]], round(nrow(g2_L16B_subset[[2]])*0.7))
# test <- g2_L16B_subset[[2]][which()]
}

# combine the code above into one big function
# function for split data (in each product) for train/test set
divide <- function(subset,x){
  if(nrow(subset[[x]]) == 1){ # if there is only one obs then assign to train
    train <- subset[[x]]
  }
  else{
    n <- nrow(subset[[x]])
    index <- sample(1:n, size=round(0.3*n))
    test <- subset[[x]][index,]
    train <- subset[[x]][-index,]
  }
  temp <- list("train"=train, "test"=test)
  return(temp)
}
get_train_test <- function(data){
  sw_name <- unique(data$SW)
  subset <- lapply(sw_name, function(x) filter(data, SW == x))
  
  train <- data.table()
  test <- data.table()
  subset2 <- lapply(1:length(subset), function(x) divide(subset,x))
  
  # combine all train and test from different subset together
  for(i in 1:length(subset2)){
    train <- rbind(train, subset2[[i]]$train)
    test <- rbind(test, subset2[[i]]$test)
  }
  
  result <- list("train"=train, "test"=test)
  return(result)
}

train <- data.table()
test <- data.table()
train_L16B <- get_train_test(g2_L16B)$train
test_L16B <- get_train_test(g2_L16B)$test

ggplot(data=train_L16B , aes(SW, `TotCpu%`)) + geom_boxplot()
ggplot(data=test_L16B, aes(SW, `TotCpu%`)) + geom_boxplot()

#----------------------#
# implement e-divisive
# g2 data L16B train
Ediv1_train <- e.divisive(matrix(train_L16B$`TotCpu%`), R=499, alpha=1) 
Ediv2_train <- e.divisive(matrix(train_L16B$`TotCpu%`), R=499, alpha=2) 

Ediv1_train$k.hat # 16 clusters
# Ediv1_train$order.found
Ediv1_train$estimates 

Ediv2_train$k.hat # 14 clusters
# Ediv2_train$order.found
Ediv2_train$estimates # discard 202, 451 to 455

ts.plot(matrix(train_L16B$`TotCpu%`), main="E-divisive train_L16B, alpha=1")
abline(v=Ediv1_train$estimates[c(-1,-length(Ediv1_train$estimates))], col="red", lty=2)

ts.plot(matrix(train_L16B$`TotCpu%`), main="E-divisive train_L16B, alpha=2")
abline(v=Ediv2_train$estimates[c(-1,-length(Ediv2_train$estimates))], col="blue", lty=2)

#----------------------#
# g2 FILTER data L16B train 
train_L16B_filter <- get_train_test(g2_L16B_filter)$train
test_L16B_filter <- get_train_test(g2_L16B_filter)$test

ggplot(data=train_L16B_filter , aes(SW, `TotCpu%`)) + geom_point()
ggplot(data=train_L16B_filter , aes(SW, `TotCpu%`)) + geom_boxplot()

Ediv1_train_filter <- e.divisive(matrix(train_L16B_filter$`TotCpu%`), R=499, alpha=1) 
Ediv2_train_filter <- e.divisive(matrix(train_L16B_filter$`TotCpu%`), R=499, alpha=2) 

Ediv1_train_filter$k.hat # 4 clusters
Ediv1_train_filter$estimates 

Ediv2_train_filter$k.hat # 3 clusters
Ediv2_train_filter$estimates # discard 121, 64 to 65

ts.plot(matrix(train_L16B_filter$`TotCpu%`), main="E-divisive train_L16B_filter, alpha=1")
abline(v=Ediv1_train_filter$estimates[c(-1,-length(Ediv1_train_filter$estimates))], col="red", lty=2)

ts.plot(matrix(train_L16B_filter$`TotCpu%`), main="E-divisive train_L16B_filter, alpha=2")
abline(v=Ediv2_train_filter$estimates[c(-1,-length(Ediv2_train_filter$estimates))], col="blue", lty=2)

#----------------------#
# average TotCpu% for the same software package (sw)
# not include in the get_train_test just in case not using it
get_average <- function(data){
  sw_name <- unique(data$SW)
  subset <- lapply(sw_name, function(x) filter(data, SW == x))
  sw_mean <- unlist(lapply(1:length(subset), function(x) mean(subset[x][[1]]$`TotCpu%`)))
  sw <- data.frame(SW=sw_name, value=sw_mean)
  return(sw)
}

train_L16B_avg <- get_average(train_L16B)
test_L16B_avg <- get_average(test_L16B)

# plot average TotCpu% for the same sw
ggplot(data=train_L16B_avg , aes(SW, value)) + geom_point()
plot(density(train_L16B_avg$value))
ggplot(data=test_L16B_avg, aes(SW, value)) + geom_point()

# g2 data L16B train_avg
Ediv1_train_avg <- e.divisive(matrix(train_L16B_avg$value), R=499, alpha=1) 
Ediv2_train_avg <- e.divisive(matrix(train_L16B_avg$value), R=499, alpha=2) 

Ediv1_train_avg$k.hat # 3 clusters
Ediv1_train_avg$estimates 

Ediv2_train_avg$k.hat # 3 clusters
Ediv2_train_avg$estimates # same

ggplot(data=train_L16B_avg , aes(SW, value, group=1)) + geom_line() + ggtitle("E-divisive train_L16B_avg, alpha=1") + geom_vline(xintercept=Ediv1_train_avg$estimates[c(-1,-length(Ediv1_train_avg$estimates))], color="red")
ts.plot(train_L16B_avg$value, main="E-divisive train_L16B_avg, alpha=1")
abline(v=Ediv1_train_avg$estimates[c(-1,-length(Ediv1_train_avg$estimates))], col="red", lty=2)

ts.plot(train_L16B_avg$value, main="E-divisive train_L16B_avg, alpha=2")
abline(v=Ediv2_train_avg$estimates[c(-1,-length(Ediv2_train_avg$estimates))], col="blue", lty=2)

#----------------------#
# g2 FILTER data L16B train_avg
train_L16B_filter_avg <- get_average(train_L16B_filter)
test_L16B_filter_avg <- get_average(test_L16B_filter)

# plot average TotCpu% for the same sw
ggplot(data=train_L16B_filter_avg , aes(SW, value)) + geom_point()
plot(density(train_L16B_filter_avg$value))
ggplot(data=test_L16B_filter_avg, aes(SW, value)) + geom_point()

# g2 FILTER data L16B train_avg
Ediv1_train_filter_avg <- e.divisive(matrix(train_L16B_filter_avg$value), R=499, alpha=1) 
Ediv2_train_filter_avg <- e.divisive(matrix(train_L16B_filter_avg$value), R=499, alpha=2) 

Ediv1_train_filter_avg$k.hat # 4 clusters
Ediv1_train_filter_avg$estimates 

Ediv2_train_filter_avg$k.hat # 3 clusters
Ediv2_train_filter_avg$estimates # discard 112, 62 to 63

ggplot(data=train_L16B_filter_avg , aes(SW, value, group=1)) + geom_line() + ggtitle("E-divisive train_L16B_filter_avg, alpha=1") + geom_vline(xintercept=Ediv1_train_filter_avg$estimates[c(-1,-length(Ediv1_train_filter_avg$estimates))], color="red")
ts.plot(train_L16B_filter_avg$value, main="E-divisive train_L16B_filter_avg, alpha=1")
abline(v=Ediv1_train_filter_avg$estimates[c(-1,-length(Ediv1_train_filter_avg$estimates))], col="red", lty=2)

ts.plot(train_L16B_filter_avg$value, main="E-divisive train_L16B_filter_avg, alpha=2")
abline(v=Ediv2_train_filter_avg$estimates[c(-1,-length(Ediv2_train_filter_avg$estimates))], col="blue", lty=2)

#----------------------#
# select min TotCpu% for the same software package (sw)
# not include in the get_train_test just in case not using it
# get_min <- function(data){
#   sw_name <- unique(data$SW)
#   subset <- lapply(sw_name, function(x) filter(data, SW == x))
#   sw_min <- unlist(lapply(1:length(subset), function(x) min(subset[x][[1]]$`TotCpu%`)))
#   sw <- data.frame(SW=sw_name, value=sw_min)
#   return(sw)
# }
# 
# train_L16B_min <- get_min(train_L16B)
# test_L16B_min <- get_min(test_L16B)
# 
# 
#----------------------------------------------------------------------#


#----------------------------------------------------------------------#
# try depmixS4
temp <- get_average(g2_L16B_filter)
model <- depmix(`TotCpu%` ~ 1, data = g2_L16B_filter, nstates = 3)
model <- depmix(value ~ 1, data = temp, nstates = 3)
set.seed(1)
fitted <- fit(model)
summary(fitted)
print(fitted)
# state 1 is the starting state for the process

prob <- posterior(fitted) # Compute probability of being in each state
head(prob)
rowSums(head(prob)[,2:4]) # Check that probabilities sum to 1

par(mfrow=c(2,1))
ts.plot(matrix(temp$value))
plot(1:nrow(prob), prob[,1], type='l')

dat <- temp
p <- prob[,2] # Pick out the first state
dat$p <- p # Put it in the data frame for plotting

# Pick out an interesting subset of the data or plotting and
# reshape the data in a form convenient for ggplot
dat2 <- melt(dat[,c(7,14,18)],id="SW",measure=c("TotCpu%","p"))
dat2 <- melt(dat[,1:3],id="SW",measure=c("value","p"))
#head(df)

# Plot the log return time series along withe the time series of probabilities
qplot(SW,value,data=dat2,geom="line", ylab = "") + facet_grid(variable ~ ., scales="free_y")

#----------------------------------------------------------------------#

regexpr("RrcConnectionSetupComplete", q)




