library(data.table) # for importing data to R
library(gtools) # for sorting Release and SW column in aescending order
library(dplyr) # for data manipulation
library(stringr) # for using in extract_component()
library(ggplot2) # for graphics
library(MSwM2) # for performing Markov switching model
library(ecp) # for performaing E-divisive method

#----------------------------------------------------------------------#
# Data preprocessing
#----------------------------------------------------------------------#
# import data
g2 <- fread("master-g2-17col.csv.txt")

#-----------------------------------------------------#
# discard test run which has no value in EventsPerSec
g2 <- filter(g2, EventsPerSec != "")
g2 <- as.data.table(g2)

#-----------------------------------------------------#
# function to sort on multiple columns
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

#-----------------------------------------------------#
# function to get minimum value of TotCpu in each SW
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

#-----------------------------------------------------#
# sort by Release and SW column
g2_sort <- g2[multi.mixedorder(Release, SW),]

#-----------------------------------------------------#
# function to extract local events in EventPerSec column
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

#-----------------------------------------------------#
# discard test run which has strange local events in EventsPerSec
g2_extract <- extract_component(g2_sort)
which(g2_extract$`Active(anon):` != 0) 
g2_sort <- g2_sort[-which(g2_extract$`Active(anon):` != 0),] # remove test cases
g2_extract <- extract_component(g2_sort) # extract local events again

#-----------------------------------------------------#
# rename variable in order to properly use in fitting the Markov switching model
colnames(g2_extract)[which(colnames(g2_extract)=="TotCpu%")] <- "TotCpu"
colnames(g2_extract)[which(colnames(g2_extract)=="Fdd/Tdd")] <- "Fdd.Tdd"

#-----------------------------------------------------#
# function to get dataset for each SW
get_subset <- function(data, release){
  # filter Release
  data <- data[which(Release == release)]
  
  # select test case which has minimum value
  data1 <- get_min(data, "TotCpu")
  
  # change DuProdName, Fdd/Tdd, NumCells to factor
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

#-----------------------------------------------------#
# function to split dataset into training and test set 
train_test <- function(data, num){
  train_num <- floor(nrow(data) * num)
  train <- data[1:train_num,]
  test <- data[-c(1:train_num),]
  return(list(train=train,test=test))
}

num <- 0.9 # training set with 90%
train_g2_L16A <- train_test(g2_L16A, num)$train
test_g2_L16A <- train_test(g2_L16A, num)$test

train_g2_L16B <- train_test(g2_L16B, num)$train
test_g2_L16B <- train_test(g2_L16B, num)$test

train_g2_L17A <- train_test(g2_L17A, num)$train
test_g2_L17A <- train_test(g2_L17A, num)$test

#-----------------------------------------------------#
# define predictor variables and create formula for linear model
predictor <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","DuProdName","Fdd.Tdd","NumCells")
fmla <- as.formula(paste("TotCpu ~ ", paste(predictor, collapse= "+")))


#----------------------------------------------------------------------#
# Markov switching model
#----------------------------------------------------------------------#
#-----------------------------------------------------#
# software release L16A
#-----------------------------------------------------#
# define predictor variables
# drop DuProdName because of singularity
predictor2 <- c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Fdd.Tdd","NumCells")
fmla2 <- as.formula(paste("TotCpu ~ ", paste(predictor2, collapse= "+")))

# fit linear model
mod_L16A <- lm(fmla2, data=train_g2_L16A)
summary(mod_L16A)

# perform Markov switching autoregressive model
# (1) NN
switch <- rep(TRUE,length(mod_L16A$coefficients)+1+1) # make all variables to have switching effect
names(switch) <- c(names(mod_L16A$coefficients),"AR","var")
switch[c(5,6,7)] <- FALSE # defining non-switching effect to Fdd.Tdd and NumCells

set.seed(1)
mswm_L16A_NN <- MSwM2::msmFit(mod_L16A, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=500, parallel=FALSE))
summary(mswm_L16A_NN)

# plot
plotArea(mswm_L16A_NN)
plotSmo(mswm_L16A_NN)

# predict
pred_L16A <- MSwM2::statePredict(mswm_L16A_NN, test_g2_L16A)

#-----------------------------------------------------#
# software release L16B
#-----------------------------------------------------#
# fit linear model
mod_L16B <- lm(fmla, data=train_g2_L16B)
summary(mod_L16B)

# perform Markov switching autoregressive model
# (4) NYY
switch <- rep(TRUE,length(mod_L16B$coefficients)+1+1) # make all variables to have switching effect
names(switch) <- c(names(mod_L16B$coefficients),"AR","var")
switch[c(5)] <- FALSE  # defining non-switching effect to DuProdName

set.seed(1)
mswm_L16B_NYY <- MSwM2::msmFit(mod_L16B, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_NYY)

# plot
plotArea(mswm_L16B_NYY)
plotSmo(mswm_L16B_NYY)

# predict
pred_L16B <- MSwM2::statePredict(mswm_L16B_NYY, test_g2_L16B)

#-----------------------------------------------------#
# software release L17A
#-----------------------------------------------------#
# fit linear model
mod_L17A <- lm(fmla, data=train_g2_L17A)
summary(mod_L17A) 

# perform Markov switching autoregressive model
# (1) NNN
switch <- rep(TRUE,length(mod_L17A$coefficients)+1+1) # make all variables to have switching effect
names(switch) <- c(names(mod_L17A$coefficients),"AR","var")
switch[c(5,6,7,8,9)] <- FALSE  # defining non-switching effect to all test environments

set.seed(1)
mswm_L17A_NNN <- MSwM2::msmFit(mod_L17A, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=500, parallel=FALSE))
summary(mswm_L17A_NNN)

# plot
plotArea(mswm_L17A_NNN)
plotSmo(mswm_L17A_NNN)

# predict
pred_L17A <- MSwM2::statePredict(mswm_L17A_NNN, test_g2_L17A)


#----------------------------------------------------------------------#
# E-divisive method
#----------------------------------------------------------------------#
#-----------------------------------------------------#
# software release L16A
#-----------------------------------------------------#
set.seed(1)
Ediv_L16A <- e.divisive(matrix(train_g2_L16A$TotCpu), R=499, min.size=5)
Ediv_L16A$estimates
out_L16A <- Ediv_L16A$estimates[c(-1,-length(Ediv_L16A$estimates))] # estimated change point locations


#-----------------------------------------------------#
# software release L16B
#-----------------------------------------------------#
set.seed(1)
Ediv_L16B <- e.divisive(matrix(train_g2_L16B$TotCpu), R=499, min.size=5)
Ediv_L16B$estimates
out_L16B <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))] # estimated change point locations


#-----------------------------------------------------#
# software release L17A
#-----------------------------------------------------#
set.seed(1)
Ediv_L17A <- e.divisive(matrix(train_g2_L17A$TotCpu), R=499, min.size=5)
Ediv_L17A$estimates 
out_L17A <- Ediv_L17A$estimates[c(-1,-length(Ediv_L17A$estimates))] # estimated change point locations


#----------------------------------------------------------------------#
# Comparison graphs
#----------------------------------------------------------------------#
# function to plot the results from both methods
plotCompare <- function(data, markov, ediv){
  ind <- nrow(data)
  
  # the state change from Markov switching model
  pred_state <- sapply(1:ind, function(x) which.max(markov@Fit@smoProb[x,]))
  chg_mswm <- which(diff(pred_state) != 0) + 1
  
  # dataframe with matching method and the change points
  method <- c(rep("Markov switching model",ind),rep("E-divisive",ind))
  changePoints <- data.frame(changeP=c(chg_mswm, ediv), method=c(rep("Markov switching model",length(chg_mswm)), rep("E-divisive",length(ediv))))
  temp <- data.frame(index=rep(1:ind,2),y=rep(data$TotCpu,2), method)
  temp$method <- factor(temp$method, levels=c("Markov switching model","E-divisive"))
  
  g <- ggplot(data=temp, aes(x=index,y=y)) + geom_line() +
    facet_grid(method ~ ., scales = 'free_y') + theme_bw() +
    ggtitle("L16B") +
    theme(panel.spacing = unit(0.2, "lines")) +
    geom_vline(aes(xintercept=changeP), data=changePoints, linetype="longdash", colour=c(rep("cyan3",length(chg_mswm)),rep("orangered",length(ediv))))
  
  return(g)
}

plotCompare(train_g2_L16B, mswm_L16B_NYY, out_L16B)


#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
# Future work
# software release L16B 
# Test case type: TC8
#----------------------------------------------------------------------#
# select 164 < RrcConnectionSetupComplete < 166
g2_extract2 <- dplyr::filter(g2_extract, RrcConnectionSetupComplete > 164 & RrcConnectionSetupComplete < 166)
g2_extract2 <- data.table(g2_extract2)

# for software release L16B
g2_L16B_TC8 <- get_subset(g2_extract2, "L16B") # 241 -> 143

mod_L16B_TC8 <- lm(TotCpu~1, data=g2_L16B_TC8)
summary(mod_L16B_TC8)

# perform Markov switching autoregressive model
switch <- rep(TRUE,length(mod_L16B_TC8$coefficients)+1+1)
# names(switch) <- c(names(mod_L16B_TC8$coefficients),"AR","var")
# switch[c(5)] <- FALSE 

# two states
set.seed(1)
mswm_L16B_2_TC8 <- MSwM2::msmFit(mod_L16B_TC8, k=2, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_2_TC8)

# three states
set.seed(1)
mswm_L16B_3_TC8 <- MSwM2::msmFit(mod_L16B_TC8, k=3, p=1, sw=switch, control=list(trace=FALSE, maxiter=1000, parallel=FALSE))
summary(mswm_L16B_3_TC8)

# plot
plotSmo(mswm_L16B_2_TC8)
plotArea(mswm_L16B_2_TC8)

plotSmo(mswm_L16B_3_TC8)
plotArea(mswm_L16B_3_TC8)


# E-divisive method
set.seed(1)
Ediv_L16B <- e.divisive(matrix(g2_L16B_TC8$TotCpu), R=499, min.size=5)
Ediv_L16B$estimates
out_L16B <- Ediv_L16B$estimates[c(-1,-length(Ediv_L16B$estimates))]

dat <- data.frame(index=seq(1,nrow(g2_L16B_TC8)), TotCpu=g2_L16B_TC8$TotCpu)
ggplot(data=dat, aes(x=index, y=TotCpu)) + geom_line() + scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) +
  geom_vline(xintercept=out_L16B, colour="red", linetype="longdash") +
  ggtitle("E-divisive") + theme_bw() 



