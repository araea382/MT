# t-test to determine whether NodeName and SW are significantly different from each other
# ANOVA

unique(g2_L16B$NodeName) # 29

g2_L16B_subset <- g2_L16B[,c(2,7,14)]
g2_L16B_subset <- lapply(unique(g2_L16B_subset$SW), function(x) filter(g2_L16B_subset, SW == x))

subset <- lapply(unique(g2_L16B$SW), function(x) filter(g2_L16B, SW == x))

# check 
t <- unlist(lapply(1:length(g2_L16B_subset), function(x) length(unique(g2_L16B_subset[[x]]$NodeName))))
ind1 <- which(t != 1) # 164 # index of SW that run more than one NodeName  
ind2 <- which(t == 2) # 86 (t-test)
ind3 <- which(t != 1 & t !=2) # more than 2 # 78 (anova)

g <- g2_L16B_subset[[14]]
t.test(220.72,c(411.57,383.6))

# t-test for 2 NameNode
ttest2 <- list()
for(i in ind2){
  sw_set <- g2_L16B_subset[[i]]
  nn_count <- count(sw_set, NodeName) # count row with respect to NodeName
  if(1 %in% nn_count$n){
    ttest2[[i]] <- "There is one observation in some NodeName"
  }else{
    vec <- list()
    for(j in 1:length(nn_count$NodeName)){
      vec[[j]] <- filter(sw_set, NodeName == nn_count$NodeName[j])$`TotCpu%` # get vector of value in particular NodeName
    }
    ttest2[[i]] <- t.test(vec[[1]], vec[[2]])$p.value
  }
}
# significant: 156, 234 (2 out of 14/ out of 86)

# ttest2 <- list()
# for(i in length(ind2)){
#   sw_set <- g2_L16B_subset[[ind2[i]]]
#   nn_count <- count(sw_set, NodeName) # count row with respect to NodeName
#   if(1 %in% nn_count$n){
#     ttest2[[i]] <- "There is one observation in some NodeName"
#   }else{
#     vec <- list()
#     for(j in 1:length(nn_count$NodeName)){
#       vec[[j]] <- filter(sw_set, NodeName == nn_count$NodeName[j])$`TotCpu%` # get vector of value in particular NodeName
#     }
#     ttest2[[i]] <- t.test(vec[[1]], vec[[2]])$p.value
#   }
# }


# ANOVA for more than 2 NameNode
sw_set <- g2_L16B_subset[[i]][-2]
sw_set$NodeName <- as.factor(sw_set$NodeName)

# aov(`TotCpu%`~NodeName, data=sw_set) # the hell 
anova(lm(`TotCpu%`~NodeName, data=sw_set))

anova3 <- list()
for(i in ind3){
  sw_set <- g2_L16B_subset[[i]] # to get only NodeName and TotCpu%
  sw_set$NodeName <- as.factor(sw_set$NodeName) # change NodeName to factor
  nn_count <- count(sw_set, NodeName) # count row with respect to NodeName
  if(1 %in% nn_count$n){
    anova3[[i]] <- "There is one observation in some NodeName"
  }else{
    anova3[[i]] <- anova(lm(`TotCpu%`~NodeName, data=sw_set))
  }  
}
# significant: 150, 167, 169, 197, 207, 218, 221 (7 out of 12/ out of 78)

anova3 <- list()
for(i in 1:length(ind3)){
  sw_set <- g2_L16B_subset[[ind3[i]]] # to get only NodeName and TotCpu%
  sw_set$NodeName <- as.factor(sw_set$NodeName) # change NodeName to factor
  nn_count <- count(sw_set, NodeName) # count row with respect to NodeName
  if(1 %in% nn_count$n){
    anova3[[i]] <- "There is one observation in some NodeName"
  }else{
    anova3[[i]] <- anova(lm(`TotCpu%`~NodeName, data=sw_set))$`Pr(>F)`[1]
  }  
}

# oneway.test(`TotCpu%`~NodeName, data=sw_set, var.equal = TRUE)
# the result is the same with using anova

pairwise.t.test(sw_set$`TotCpu%`, sw_set$NodeName, p.adjust="none")
# some tests are significant
# is it because of the EventsPerSec esp. RrcConnectionSetupComplete ???? 

# explore in deep detail for EventsPerSec
R18AG <- filter(g2_L16B, SW == "R18AG")
write.table(R18AG[,c(2,14,17)], file="L16B_R18AG.txt")

R18AG_filter <- filter(g2_L16B_filter, SW == "R18AG")

sw_set <- g2_L16B_subset[[218]]
RrcConnectionSetupComplete <- c(165.18, 292.80, 165.07, 292.61, 165.40, 41.24, 278.56, 292.30, 268.93)
sw_set$norm <- RrcConnectionSetupComplete / sw_set$`TotCpu%`
anova(lm(norm~NodeName, data=sw_set))


R17HD <- filter(g2_L16B, SW == "R17HD")
write.table(R17HD[,c(2,14,17)], file="L16B_R17HD.txt")

R17HD_filter <- filter(g2_L16B_filter, SW == "R17HD")

sw_set <- g2_L16B_subset[[207]]
RrcConnectionSetupComplete <- c(235.28, 234.93, 286.27, 231.78, 165.01, 164.98, 175.36, 169.00, 164.83, 165.07)
sw_set$norm <- RrcConnectionSetupComplete / sw_set$`TotCpu%`
anova(lm(norm~NodeName, data=sw_set))
# after normalizing, it's not significant anymore **but only for this two**

#----------------------#
# with t-test
R16DE <- filter(g2_L16B, SW == "R16DE")
write.table(R16DE[,c(2,14,17)], file="L16B_R16DE.txt")

R16DE_filter <- filter(g2_L16B_filter, SW == "R16DE") # doesn't make it

sw_set <- g2_L16B_subset[[156]]
RrcConnectionSetupComplete <- c(286.11, 292.38, 286.16, 292.23)
sw_set$norm <- RrcConnectionSetupComplete / sw_set$`TotCpu%`
nn_count <- count(sw_set, NodeName)
vec <- list()
for(j in 1:length(nn_count$NodeName)){
  vec[[j]] <- filter(sw_set, NodeName == nn_count$NodeName[j])$norm # get vector of value in particular NodeName
}
t.test(vec[[1]], vec[[2]])


R18DV <- filter(g2_L16B, SW == "R18DV")
write.table(R18DV[,c(2,14,17)], file="L16B_R18DV.txt")

R18DV_filter <- filter(g2_L16B_filter, SW == "R18DV") # doesn't make it

sw_set <- g2_L16B_subset[[234]]
RrcConnectionSetupComplete <- c(279.87, 290.98, 290.50, 289.31, 290.91)
sw_set$norm <- RrcConnectionSetupComplete / sw_set$`TotCpu%`
nn_count <- count(sw_set, NodeName)
vec <- list()
for(j in 1:length(nn_count$NodeName)){
  vec[[j]] <- filter(sw_set, NodeName == nn_count$NodeName[j])$norm # get vector of value in particular NodeName
}
t.test(vec[[1]], vec[[2]])
# after normalizing, it doesn't change the result (still significant).

#----------------------------------------------------------------------#
# Filter data
g2_L16B_subset_filter <- g2_L16B_filter[,c(2,7,14)]
g2_L16B_subset_filter <- lapply(unique(g2_L16B_subset_filter$SW), function(x) filter(g2_L16B_subset_filter, SW == x))

# check 
t_filter <- unlist(lapply(1:141, function(x) length(unique(g2_L16B_subset_filter[[x]]$NodeName))))
ind1_filter <- which(t_filter != 1) # 7 # index of SW that run more than one NodeName  
ind2_filter <- which(t_filter == 2) # 6 (t-test)
ind3_filter <- 43 # 1

g <- g2_L16B_subset[[14]]
t.test(220.72,c(411.57,383.6))

# t-test for 2 NameNode
ttest2 <- list()
for(i in ind2_filter){
  sw_set <- g2_L16B_subset_filter[[i]]
  nn_count <- count(sw_set, NodeName) # count row with respect to NodeName
  if(1 %in% nn_count$n){
    ttest2[[i]] <- "There is one observation in some NodeName"
  }else{
    vec <- list()
    for(j in 1:length(nn_count$NodeName)){
      vec[[j]] <- filter(sw_set, NodeName == nn_count$NodeName[j])$`TotCpu%` # get vector of value in particular NodeName
    }
    ttest2[[i]] <- t.test(vec[[1]], vec[[2]])$p.value
  }
}

# NO USE
# data is already filtered and there is only one or two obs which cannot apply t-test


#----------------------------------------------------------------------#
# extract components in EventsPerSec
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
# extrct value for RrcConnectionSetupComplete
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

# g$RrcConnectionSetupComplete <- rrc
g2_L16B$RrcConnectionSetupComplete <- rrc
g2_L16B$Normalize <- g2_L16B$RrcConnectionSetupComplete / g2_L16B$`TotCpu%`

#----------------------------------------------------------------------#
# try with ecp again...
# change SW from character to factor and set the factor levels to be the same as in factor labels
level <- unique(g2_L16B$SW)
g2_L16B$SW <- factor(g2_L16B$SW, levels=level)

# plot Normalize vs SW
plot(g2_L16B$SW, g2_L16B$Normalize, xlab="", main="Average CPU Utilisation g2_L16B") #
ggplot(data=g2_L16B, aes(SW, Normalize)) + geom_point()
ggplot(data=g2_L16B, aes(SW, Normalize)) + geom_boxplot()


ggplot(data=g2_L16B_filter, aes(SW, Normalize)) + geom_point()

which(g2_L16B$Normalize > 1)
g2_L16B$Normalize[c(456, 1026, 1144, 1278)]

# discard Normalize = 0
g2_L16B_new <- filter(g2_L16B, Normalize != 0) # 139 obs that has value eqaul to 0 
ggplot(data=g2_L16B_new, aes(SW, Normalize)) + geom_point()

Ediv1_new <- e.divisive(matrix(g2_L16B_new$`TotCpu%`), R=499, alpha=1) 
Ediv2_new <- e.divisive(matrix(g2_L16B_new$`TotCpu%`), R=499, alpha=2) 

Ediv1_new$k.hat # 18 clusters
Ediv1_new$estimates 

Ediv2_new $k.hat # 16 clusters
Ediv2_new $estimates

ts.plot(matrix(g2_L16B_new$Normalize), main="E-divisive g2_L16B_new, alpha=1")
abline(v=Ediv1_new$estimates[c(-1,-length(Ediv1_new$estimates))], col="red", lty=2)

ts.plot(matrix(g2_L16B_new$Normalize), main="E-divisive g2_L16B_new, alpha=2")
abline(v=Ediv2_new$estimates[c(-1,-length(Ediv2_new$estimates))], col="blue", lty=2)

get_norm_average <- function(data){
  sw_name <- unique(data$SW)
  subset <- lapply(sw_name, function(x) filter(data, SW == x))
  sw_mean <- unlist(lapply(1:length(subset), function(x) mean(subset[x][[1]]$Normalize)))
  sw <- data.frame(SW=sw_name, value=sw_mean)
  return(sw)
}
g2_L16B_new_avg <- get_norm_average(g2_L16B_new)

ggplot(data=g2_L16B_new_avg , aes(SW, value)) + geom_point()
plot(density(g2_L16B_new_avg$value))

Ediv1_new_avg <- e.divisive(matrix(g2_L16B_new_avg$value), R=999, alpha=1) 
Ediv2_new_avg <- e.divisive(matrix(g2_L16B_new_avg$value), R=999, alpha=2) 

Ediv1_new_avg$k.hat # 3 clusters
Ediv1_new_avg$estimates 

Ediv2_new_avg$k.hat # 3 clusters
Ediv2_new_avg$estimates 

ts.plot(g2_L16B_new_avg$value, main="E-divisive g2_L16B_new_avg, alpha=1")
abline(v=Ediv1_new_avg$estimates[c(-1,-length(Ediv1_new_avg$estimates))], col="red", lty=2)

ts.plot(g2_L16B_new_avg$value, main="E-divisive g2_L16B_new_avg, alpha=2")
abline(v=Ediv2_new_avg$estimates[c(-1,-length(Ediv2_new_avg$estimates))], col="blue", lty=2)

# it cann't detect the peak 


