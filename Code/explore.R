setwd("C:/Users/EARAEAM/Documents/Thesis/Data")
setwd("~/Dropbox/Thesis/Data")

library(data.table)
library(gtools) # for sorting RW column
library(ecp)
library(cpm)
library(ggplot2)

# g2_org <- fread("master-g2.csv.txt") # some lines don't have 17 columns
# g2 <- read.table("master-g2.csv.txt", sep=",") # line 1147 did not have 17 elements

g2 <- fread("master-g2-17col.csv.txt")
g2_filter <- fread("master-g2.filtered.csv.txt")
colnames(g2_filter) <- colnames(g2)

#----------------------------------------------------------------------#
# testing sort: For SW
# x <- c("R2AKX","R18U","R9GT","R10AG")
# sort(x) # "R10AG" "R18U"  "R2AKX" "R9GT" 
# mixedsort(x) # "R2AKX" "R9GT"  "R10AG" "R18U" 
# 
# y <- c("18","2","1")
# z <- c("R18","R2","R1")
# sort(y) # "1" 18" "2" 
# sort(z) # "R1"  "R18" "R2" 
# mixedsort(y)
# mixedsort(z)

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
# g2_sort <- g2[order(Release, SW)] # problem with some sorting
# g2_sort <- g2[mixedorder(Release, SW)] # error # mixedorder does not support multiple columns
g2_sort <- g2[multi.mixedorder(Release, SW),] 
g2_sort_filter <- g2_filter[multi.mixedorder(Release, SW),] 

# another way if can't fix the 'mixedorder' is to sepearte Release first then sort by SW
# g2_L16A <- g2[which(Release == "L16A")]
# g2_L16A_sort <- g2_L16A[mixedorder(SW)]
# g2_L16B <- g2[which(Release == "L16B")]
# g2_L16B_sort <- g2_L16B[mixedorder(SW)]

# g2_L16A <- g2_sort[which(Release == "L16A")]
g2_L16B <- g2_sort[which(Release == "L16B")]
g2_L16B_filter <- g2_sort_filter[which(Release == "L16B")]
# g2_L17A <- g2_sort[which(Release == "L17A")]

g2_R15G <- g2_L16B[which(SW == "R15G")] # 3 test runs in raw data but only 2 test runs show in platypus
# write.table(g2_R15G$EventsPerSec, file="dat.txt")

# R15G = 208.5 # orange # 10.75.74.55
# R15G = 210.07 # green # 10.75.74.48
# R15G = 206.57 # not show # EventsPerSec most component is not around 165

#----------------------------------------------------------------------#
g2_green <- g2_L16B[which(NodeName == "10.75.74.48")]
g2_green_R12AK <- g2_green[which(SW == "R12AK")]
# write.table(g2_green_R12AK$EventsPerSec, file="dat-green.txt")

# R12AK = 206.82/ 205.47/ 205.43
# it seems like there is only the last test run which make it to the graph (at least the number is the same)
# first two test runs also have EventsPerSec component around 165. Do they make it or not??
## ANSWER: select the minimum value

#----------------------------------------------------------------------#
# LOOK and DECIDE again
# sort by Timestamp
g2_sort_time <- g2[order(Timestamp),]

# sort all three
g2_sort_all <- g2[multi.mixedorder(Timestamp, Release, SW),] 

#----------------------------------------------------------------------#
# change SW from character to factor and set the factor levels to be the same as in factor labels
level <- unique(g2_L16B$SW)
g2_L16B$SW <- factor(g2_L16B$SW, levels=level)

level_filter <- unique(g2_L16B_filter$SW)
g2_L16B_filter$SW <- factor(g2_L16B_filter$SW, levels=level_filter)

# plot TotCpu% vs SW
plot(g2_L16B$SW, g2_L16B$`TotCpu%`, xlab="") #
ggplot(data=g2_L16B, aes(SW, `TotCpu%`)) + geom_point()
ggplot(data=g2_L16B, aes(SW, `TotCpu%`)) + geom_boxplot()
plot(density(g2_L16B_filter$`TotCpu%`))

plot(g2_L16B_filter$SW, g2_L16B_filter$`TotCpu%`, xlab="", main="Filter: Average CPU Utilisation L16B") # compare with platypus
ggplot(data=g2_L16B_filter, aes(SW, `TotCpu%`)) + geom_point()
ggplot(data=g2_L16B_filter, aes(SW, `TotCpu%`)) + geom_boxplot() # getting the same result as in plot


#----------------------------------------------------------------------#
# t-test for L16B

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

#----------------------------------------------------------------------#
# try ecp package

# g2 data
# univariate: TotCpu%
result1 <- e.divisive(matrix(g2_L16B$`TotCpu%`), R=499, alpha=2) # approx time 5 mins
result2 <- e.divisive(matrix(g2_L16B$`TotCpu%`), R=499, alpha=1) # approx time 7 mins

result2$k.hat
result2$order.found
result2$estimates
result2$considered.last
result2$p.values
result2$permutations

ts.plot(matrix(g2_L16B$`TotCpu%`), main="E-divisive")
abline(v=result2$estimates[c(-1,-19)], col="red", lty=2)


result <- e.agglo(matrix(g2_L16B$`TotCpu%`), alpha=1)
result$estimates
tail(result$fit, 5)
z <- result$progression
result$merged

ts.plot(matrix(g2_L16B_filter$`TotCpu%`), main="E-agglo")
abline(v=result$estimates[c(-1,-4)], col="red", lty=2)


#----------------------------------------------------------------------#
# try BreakoutDetection
library(BreakoutDetection)

data(Scribe)
res = breakout(Scribe, min.size=24, method='multi', beta=.001, degree=1, plot=TRUE)
res$plot

plot(Scribe, type="l")

ts.plot(matrix(Scribe))
abline(v=test$estimates[c(-1,-5)], col="red", lty=2)


res2 <- breakout(g2_L16B_filter$`TotCpu%`, method="multi", plot=TRUE)
res2$plot
