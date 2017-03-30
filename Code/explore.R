# RUN EVERY TIME
setwd("C:/Users/EARAEAM/Dropbox/Thesis/Data")
setwd("C:/Users/EARAEAM/Documents/Thesis/Data")

library(data.table)
library(gtools) # for sorting RW column
library(dplyr)
library(stringr)
library(ggplot2)

library(ecp)
library(glmnet)
# library(MASS)

#----------------------------------------------------------------------#
g2 <- fread("master-g2-17col.csv.txt")
g2_filter <- fread("master-g2.filtered.csv.txt")
colnames(g2_filter) <- colnames(g2)

g1_filter <- fread("master.filtered.ALL.csv.txt")

#----------------------------------------------------------------------#
# discard test run which has no value in EventsPerSec
g2 <- filter(g2, EventsPerSec != "")
g2 <- as.data.table(g2)

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
# average TotCpu for the same software package (sw)
# not include in the get_train_test just in case not using it
get_average <- function(data, y){
  sw_name <- unique(data$SW)
  subset <- lapply(sw_name, function(x) filter(data, SW == x))
  sw_mean <- unlist(lapply(1:length(subset), function(x) mean(subset[x][[1]][,y])))
  sw <- data.frame(SW=sw_name, value=sw_mean)
  return(sw)
}

#----------------------------------------------------------------------#
# select min TotCpu for the same software package (sw)
# still contain all columns
.old_get_min <- function(){
# not include in the get_train_test just in case not using it
# get_min <- function(data){
#   sw_name <- unique(data$SW)
#   subset <- lapply(sw_name, function(x) filter(data, SW == x))
#   sw_min <- unlist(lapply(1:length(subset), function(x) min(subset[x][[1]]$`TotCpu`)))
#   sw <- data.frame(SW=sw_name, value=sw_min)
#   return(sw)
# }
#
# train_L16B_min <- get_min(train_L16B)
# test_L16B_min <- get_min(test_L16B)
#
# # select min TotCpu for the same software package (sw)
# # still contain all columns
# get_min <- function(data, y){
#   sw_name <- unique(data$SW)
#   subset <- lapply(sw_name, function(x) filter(data, SW == x))
#   sw_min <- unlist(lapply(1:length(subset), function(x) min(subset[x][[1]][,y])))
#   subset_min <- data.frame()
#   for(i in 1:length(sw_name)){
#     s <- filter(data, SW == sw_name[i] & y == sw_min[i])
#     s <- s %>% distinct(SW, y, .keep_all = TRUE) # in case there are duplicate rows
#     subset_min <- bind_rows(subset_min, s)
#   }
#   return(subset_min)
# }
}
get_min <- function(data, y){
  require("lazyeval")
  sw_name <- unique(data$SW)
  if(isTRUE(length(sw_name) <1)){stop("No data for this value")}
  subset <- lapply(sw_name, function(x) dplyr::filter(data, SW == x))
  sw_min <- unlist(lapply(1:length(subset), function(x) min(subset[x][[1]][,y])))
  subset_min <- data.frame()
  for(i in 1:length(sw_name)){
    filter_criteria <- interp(~y == x, .values=list(y=as.name(y), x=sw_min[i]))
    s <- data %>% filter(SW == sw_name[i]) %>% filter_(filter_criteria)
    if(nrow(s) > 1){ # in case there are duplicate rows
      s <- s %>% distinct(SW, paste(y), .keep_all = TRUE)
      s <- s[,-length(s)] # discard the generated new column
    }
    subset_min <- bind_rows(subset_min, s)
  }
  return(subset_min)
}

#----------------------------------------------------------------------#
# select max TotCpu for the same software package (sw)
# still contain all columns
get_max <- function(data, y){
  require("lazyeval")
  sw_name <- unique(data$SW)
  if(isTRUE(length(sw_name) <1)){stop("No data for this value")}
  subset <- lapply(sw_name, function(x) filter(data, SW == x))
  sw_max <- unlist(lapply(1:length(subset), function(x) max(subset[x][[1]][,y])))
  subset_max <- data.frame()
  for(i in 1:length(sw_name)){
    filter_criteria <- interp(~y == x, .values=list(y=as.name(y), x=sw_max[i]))
    s <- data %>% filter(SW == sw_name[i]) %>% filter_(filter_criteria)
    if(nrow(s) > 1){ # in case there are duplicate rows
      s <- s %>% distinct(SW, paste(y), .keep_all = TRUE)
      s <- s[,-length(s)] # discard the generated new column
    }
    subset_max <- bind_rows(subset_max, s)
  }
  return(subset_max)
}

#----------------------------------------------------------------------#
# sort by Release and SW column
g2_sort <- g2[multi.mixedorder(Release, SW),]
g2_sort_filter <- g2_filter[multi.mixedorder(Release, SW),]

#----------------------------------------------------------------------#
# extract components in EventsPerSec
.old_extract <- function(){
  # rrc <- apply(g2_L16B, 1, function(x){
  #   events <- as.character(x[17]) # get content from EventsPerSec column
  #   st <- unlist(strsplit(events, " "))
  #   value <- str_subset(st, "RrcConnectionSetupComplete")
  #   if(length(value) == 0L){
  #     0
  #   }else{
  #     as.numeric(str_replace(value, "RrcConnectionSetupComplete=", "")) # replace name with blank in order to get only value
  #   }
  # })

  # for(j in 1:nrow(data)){
  #   events <- as.character(data[j,17])
  #   st <- unlist(strsplit(events, " "))
  #   for(i in 1:length(st)){
  #     s <- strsplit(st[i],"=")
  #     name <- s[[1]][1]
  #     value <- as.numeric(s[[1]][2])
  #     detect <- which(str_detect(colnames(data),name) == TRUE) # something wrong with this function. It can not detect properly
  #     if(length(detect) == 0L){
  #       data[j,"name"] <- value
  #       colnames(data) <- str_replace(colnames(data), "name", name)
  #     }else{
  #       data[j,detect] <- value
  #     }
  #   }
  # }

  # tab <- apply(g2_L16B[1:2,1:17], 1, function(x){
  # events <- as.character(x[17])
  # st <- unlist(strsplit(events, " "))
  # for(i in 1:length(st)){
  #   s <- strsplit(st[i],"=")
  #   name <- s[[1]][1]
  #   value <- as.numeric(s[[1]][2])
  #   detect <- which(str_detect(names(x),name) == TRUE)
  #     if(length(detect) == 0L){
  #       x["name"] <- value
  #       names(x) <- str_replace(names(x), "name", name)
  #     }else{
  #       x[detect] <- value
  #     }
  #   }
  # })
  # still cannot do it with apply or whatever their family are

}
extract_component <- function(data){
  for(j in 1:nrow(data)){
    events <- as.character(data[j,17])
    st <- unlist(strsplit(events, " "))
    for(i in 1:length(st)){
      s <- strsplit(st[i],"=")
      name <- s[[1]][1]
      value <- as.numeric(s[[1]][2])
      detect <- match(name, colnames(data), nomatch=0)
      if(detect == 0){
        data[j,"name"] <- value
        colnames(data) <- str_replace(colnames(data), "name", name)
      }else{
        data[j,detect] <- value
      }
    }
  }
  data[is.na(data)] <- 0 # replace NA with 0
  return(data)
}

#----------------------------------------------------------------------#
# ********MAMUALLY manipulate data**********
# discard test run which has strange components in EventsPerSec
g2_extract <- extract_component(g2_sort)
which(g2_extract$`Active(anon):` != 0) # 557 561 1791 1798 1890 1895 1967
g2_sort <- g2_sort[-which(g2_extract$`Active(anon):` != 0),] # remove test run
g2_extract <- extract_component(g2_sort) # extract again

g2_extract_filter <- extract_component(g2_sort_filter)

#----------------------------------------------------------------------#
# rename variable
colnames(g2_extract)[which(colnames(g2_extract)=="TotCpu%")] <- "TotCpu"
colnames(g2_extract)[which(colnames(g2_extract)=="Fdd/Tdd")] <- "Fdd.Tdd"

#----------------------------------------------------------------------#
# subset for each Release
# g2_L16B <- g2_sort[which(Release == "L16B")]
# g2_L16B_filter <- g2_sort_filter[which(Release == "L16B")]
# g2_L16A <- g2_sort[which(Release == "L16A")]
# g2_L17A <- g2_sort[which(Release == "L17A")]
# g2_L17A_filter <- g2_sort_filter[which(Release == "L17A")]

# g2_L16B <- g2_extract[which(Release == "L16B")]
# g2_L16B_filter <- g2_extract_filter[which(Release == "L16B")]
# g2_L16A <- g2_extract[which(Release == "L16A")]
# g2_L17A <- g2_extract[which(Release == "L17A")]
# g2_L17A_filter <- g2_extract_filter[which(Release == "L17A")]

get_subset <- function(data, release){
  data <- data[which(Release == release)]
  data1 <- get_min(data, "TotCpu")

  # DuProdName, Fdd/Tdd, NumCells to factor
  data1$DuProdName <- as.factor(data1$DuProdName)
  data1$Fdd.Tdd <- as.factor(data1$Fdd.Tdd)
  data1$NumCells <- as.factor(data1$NumCells)

  # change SW from character to factor and set the factor levels to be the same as in factor labels
  level <- unique(data1$SW)
  data1$SW <- factor(data1$SW, levels=level)

  return(data1)
}

g2_L16A <- get_subset(g2_extract, "L16A")
g2_L16B <- get_subset(g2_extract, "L16B")
g2_L17A <- get_subset(g2_extract, "L17A")
g2_L17B <- get_subset(g2_extract, "L17B")


# extract components
# g2_L16B_extract <- extract_component(g2_L16B) # 143 variables are added

# components which are not appear in g2_filter
# setdiff(colnames(g2_extract), colnames(g2_extract_filter))

#----------------------------------------------------------------------#
# # use get_min() for each subset
# g2_L16A_min <- get_min(g2_L16A,"TotCpu")
# g2_L16B_min <- get_min(g2_L16B,"TotCpu")
# g2_L17A_min <- get_min(g2_L17A,"TotCpu")

#----------------------------------------------------------------------#
# Divide train (90) test (10) for each dataset
train_test <- function(data, num){
  train_num <- floor(nrow(data) * num)
  train <- data[1:train_num,]
  test <- data[-c(1:train_num),]
  return(list(train=train,test=test))
}

num <- 0.9
train_g2_L16A <- train_test(g2_L16A, num)$train
test_g2_L16A <- train_test(g2_L16A, num)$test

train_g2_L16B <- train_test(g2_L16B, num)$train
test_g2_L16B <- train_test(g2_L16B, num)$test

train_g2_L17A <- train_test(g2_L17A, num)$train
test_g2_L17A <- train_test(g2_L17A, num)$test

# density function for TotCpu
plot(density(train_g2_L16B$TotCpu))
plot(density(train_g2_L16A$TotCpu))
plot(density(train_g2_L17A$TotCpu))

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
# # change SW from character to factor and set the factor levels to be the same as in factor labels
# level <- unique(g2_L16B$SW)
# g2_L16B$SW <- factor(g2_L16B$SW, levels=level)
#
# level_filter <- unique(g2_L16B_filter$SW)
# g2_L16B_filter$SW <- factor(g2_L16B_filter$SW, levels=level_filter)

# plot TotCpu vs SW
plot(g2_L16B$SW, g2_L16B$`TotCpu`, xlab="", main="Average CPU Utilisation g2_L16B")
ggplot(data=g2_L16B, aes(SW, `TotCpu`)) + geom_point()
ggplot(data=g2_L16B, aes(SW, `TotCpu`)) + geom_boxplot()
plot(density(g2_L16B_filter$`TotCpu`))

plot(g2_L16B_filter$SW, g2_L16B_filter$`TotCpu`, xlab="", main="Average CPU Utilisation g2_L16B_filter") # compare with platypus
ggplot(data=g2_L16B_filter, aes(SW, `TotCpu`, group=1)) + geom_point(stat="summary") + stat_summary(geom="line")
ggplot(data=g2_L16B_filter, aes(SW, `TotCpu`)) + geom_boxplot() # getting the same result as in plot

#----------------------------------------------------------------------#
# t-test for g2 data L16B
.ttest <- function(){
# subset for each software package (SW)
tr <- list()
for(i in 1:length(level)){
  tr[[i]] <- subset(g2_L16B, SW==level[i])
}

t.test(tr[[2]]$`TotCpu`,tr[[3]]$`TotCpu`) # try t-test for one pair

# apply t-test by considering on one SW
t <- lapply(2:length(level), function(i){
  if(nrow(tr[[i-1]]) > 1 & nrow(tr[[i]]) > 1){
    t.test(tr[[i-1]]$`TotCpu`, tr[[i]]$`TotCpu`)
  } else "Only one observation in subset"
})

tr_no <- apply(1:length(level), function(i){
  nrow(tr[[i]])
})

tr_no <- data.frame(unlist(tr_no))

t[1:10] # investigate for first ten SW (no.2 is significant)
ggplot(data=g2_L16B[1:sum(tr_no[1:10,]),], aes(SW, `TotCpu`)) + geom_point()
ggplot(data=g2_L16B[1:sum(tr_no[1:10,]),], aes(SW, `TotCpu`)) + geom_boxplot()

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
ggplot(data=g2_L16B, aes(SW, `TotCpu`)) + geom_point() + geom_vline(xintercept=sig, linetype="dashed", color="red")


# 5 SW
t <- lapply(2:length(level), function(i){
  t.test(tr[[i-1]]$`TotCpu`, tr[[i]]$`TotCpu`)
})

tr2 <- list()
for(i in 5:length(level)){
  tr2[[i]] <- rbind(tr[[i-4]],tr[[i-3]],tr[[i-2]],tr[[i-1]],tr[[i]])
}

}

#----------------------------------------------------------------------#
# # apply function get to data g2 L16B
# # use get_average()
# g2_L16B_avg <- get_average(g2_L16B,"TotCpu")
# g2_L16B_filter_avg <- get_average(g2_L16B_filter,"TotCpu")
#
# # use get_min()
# g2_L16B_min <- get_min(g2_L16B,"TotCpu")
# g2_L16B_filter_min <- get_min(g2_L16B_filter,"TotCpu")
#
# # use get_max()
# g2_L16B_max <- get_max(g2_L16B,"TotCpu")

#----------------------------------------------------------------------#
# *******OLD one********
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

ggplot(data=train_L16B , aes(SW, `TotCpu`)) + geom_boxplot()
ggplot(data=test_L16B, aes(SW, `TotCpu`)) + geom_boxplot()

#----------------------------------------------------------------------#
# plot different NodeName and SW (like in platypus but this one use mean instead of min)
# g2 L16B
nn_name <- unique(g2_L16B$NodeName)
sw_name <- unique(g2_L16B$SW)
g2_L16B_nn <- lapply(unique(g2_L16B$NodeName), function(x) filter(g2_L16B, NodeName == x))

range <- seq(min(g2_L16B$`TotCpu`),max(g2_L16B$`TotCpu`),length.out=length(sw_name)) # in order to make the plot

plot(as.numeric(sw_name), range, type="n", xaxt="n", xlab="SW", ylab="CPU") # make blank plot
axis(1, at=1:length(sw_name), labels=sw_name)

for(i in 1:length(nn_name)){
  nn_subset <- get_average(g2_L16B_nn[[i]], "TotCpu")
  points(as.numeric(nn_subset$SW), nn_subset$value, col=i, type="o")
}

#----------------------#
# g2 FILTER L16B
.plot_filter <- function(){
nn_name_filter <- unique(g2_L16B_filter$NodeName)
sw_name_filter <- unique(g2_L16B_filter$SW)
g2_L16B_nn_filter <- lapply(unique(g2_L16B_filter$NodeName), function(x) filter(g2_L16B_filter, NodeName == x))

a <- get_average(g2_L16B_nn_filter[[1]],"TotCpu")

range <- seq(min(g2_L16B_filter$`TotCpu`),max(g2_L16B_filter$`TotCpu`),length.out=length(sw_name_filter))

plot(as.numeric(sw_name_filter), range, type="n", ylim=c(50,300), xaxt="n", xlab="SW", ylab="CPU")
axis(1, at=1:length(sw_name_filter), labels=sw_name_filter)
# axis(1, at=1:length(sw_name_filter), labels=FALSE)
# text(1:length(sw_name_filter), par("usr")[3]-0.2, labels=sw_name_filter, cex=0.5, srt=45, pos=2, xpd=TRUE)

for(i in 1:length(nn_name_filter)){
  nn_subset_filter <- get_average(g2_L16B_nn_filter[[i]],"TotCpu")
  points(as.numeric(nn_subset_filter$SW), nn_subset_filter$value, col=i, type="o") # same as in platypus
}
}

#----------------------------------------------------------------------#
# extract components in EventsPerSec
.try_extract <- function(){
q <- g2_L16B[1,]$EventsPerSec
qt <- as.character(q)
st <- unlist(strsplit(qt, " "))

grep("RrcConnectionSetupComplete", st) # index of this word
rrc <- st[grep("RrcConnectionSetupComplete", st)]
sub("RrcConnectionSetupComplete=", "", rrc)

# another method to extract value of RrcConnectionSetupComplete
library(stringr)
rrc <- str_subset(st, "RrcConnectionSetupComplete")
str_replace(rrc, "RrcConnectionSetupComplete=", "")

# g <- g2_L16B[1:5,] # just to test before using the whole dataset
}

# extrct value for RrcConnectionSetupComplete
.extract_rrc <- function(){
rrc <- apply(g2_L16B, 1, function(x){
  events <- as.character(x[17]) # get content from EventsPerSec column
  st <- unlist(strsplit(events, " "))
  value <- str_subset(st, "RrcConnectionSetupComplete")
  if(length(value) == 0L){
    0
  }else{
    as.numeric(str_replace(value, "RrcConnectionSetupComplete=", "")) # replace name with blank in order to get only value
  }
})
rrc <- unlist(rrc) # vector

g2_L16B$RrcConnectionSetupComplete <- rrc
g2_L16B$Normalize <- g2_L16B$RrcConnectionSetupComplete / g2_L16B$`TotCpu`
}

#----------------------#
# g2 data L16B
# univariate: Normalize
# E-divisive
.not_run <- function(){
# change SW from character to factor and set the factor levels to be the same as in factor labels
level <- unique(g2_L16B$SW)
g2_L16B$SW <- factor(g2_L16B$SW, levels=level)

# plot Normalize vs SW
plot(g2_L16B$SW, g2_L16B$Normalize, xlab="", main="Average CPU Utilisation g2_L16B") #
ggplot(data=g2_L16B, aes(SW, Normalize)) + geom_point()
ggplot(data=g2_L16B, aes(SW, Normalize)) + geom_boxplot()

# which(g2_L16B$Normalize > 1)
# g2_L16B$Normalize[c(425, 981, 1099, 1232)]

# discard Normalize = 0
g2_L16B_new <- filter(g2_L16B, Normalize != 0) # 93 obs that has value eqaul to 0
ggplot(data=g2_L16B_new, aes(SW, Normalize)) + geom_point()

Ediv1_new <- e.divisive(matrix(g2_L16B_new$Normalize), R=499, alpha=1)
Ediv1_new$k.hat # 11 clusters
Ediv1_new$estimates

ts.plot(matrix(g2_L16B_new$Normalize), main="E-divisive g2_L16B_new, alpha=1")
abline(v=Ediv1_new$estimates[c(-1,-length(Ediv1_new$estimates))], col="red", lty=2)

# use get_average()
g2_L16B_new_avg <- get_average(g2_L16B_new, "Normalize")

ggplot(data=g2_L16B_new_avg , aes(SW, value)) + geom_point()
plot(density(g2_L16B_new_avg$value))

Ediv1_new_avg <- e.divisive(matrix(g2_L16B_new_avg$value), R=499, alpha=0.1) # it cann't detect the peak if use other value
Ediv1_new_avg$k.hat
Ediv1_new_avg$estimates

ts.plot(g2_L16B_new_avg$value, main="E-divisive g2_L16B_new_avg, alpha=1")
abline(v=Ediv1_new_avg$estimates[c(-1,-length(Ediv1_new_avg$estimates))], col="red", lty=2)

plot(g2_L16B_new_avg$value, main="E-divisive g2_L16B_new_avg, alpha=1", type="l", xaxt="n")
abline(v=Ediv1_new_avg$estimates[c(-1,-length(Ediv1_new_avg$estimates))], col="red", lty=2)
axis(1, at=1:nrow(g2_L16B_new_avg), labels=g2_L16B_new_avg$SW)

# use get_min()
g2_L16B_new_min <- get_min(g2_L16B_new, "Normalize")
Ediv1_new_min <- e.divisive(matrix(g2_L16B_new_min$Normalize), R=499, min.size=15, alpha=0.1)
Ediv1_new_min$k.hat
Ediv1_new_min$estimates

plot(g2_L16B_new_min$Normalize, main="E-divisive g2_L16B_new_min, alpha=1", type="l", xaxt="n")
abline(v=Ediv1_new_min$estimates[c(-1,-length(Ediv1_new_min$estimates))], col="red", lty=2)
axis(1, at=1:nrow(g2_L16B_new_min), labels=g2_L16B_new_min$SW)

# Note: do not have to do with g2_filter because it is already filtered RrcConnectionSetupComplete to be around 165
}
