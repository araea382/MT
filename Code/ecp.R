library(ecp)

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


# # use get_average()
# Ediv1_avg <- e.divisive(matrix(g2_L16B_avg$value), R=499, alpha=1) 
# Ediv1_avg$k.hat
# Ediv1_avg$estimates
# 
# plot(g2_L16B_avg$value, main="E-divisive g2_L16B_avg, alpha=1", type="l", xaxt="n")
# abline(v=Ediv1_avg$estimates[c(-1,-length(Ediv1_avg$estimates))], col="red", lty=2)
# axis(1, at=1:nrow(g2_L16B_avg), labels=g2_L16B_avg$SW)

# use get_min()
Ediv1_min <- e.divisive(matrix(g2_L16B_min$`TotCpu%`), R=499, alpha=1) 
Ediv1_min$k.hat # 3 clusters
Ediv1_min$estimates

plot(g2_L16B_min$`TotCpu%`, main="E-divisive g2_L16B_min, alpha=1", type="l", xaxt="n")
abline(v=Ediv1_min$estimates[c(-1,-length(Ediv1_min$estimates))], col="red", lty=2)
axis(1, at=1:nrow(g2_L16B_min), labels=g2_L16B_min$SW)


#----------------------#
# E-agglo
mem <- c(rep(1:30, each=5), rep(31:220, each=5), rep(221:243, each=6)) # just make it up without any speical reason
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

Ediv1_filter$k.hat # 4 clusters
# Ediv1_filter$order.found
Ediv1_filter$estimates

Ediv2_filter$k.hat # 4 clusters
# Ediv2_filter$order.found
Ediv2_filter$estimates
# same

ts.plot(matrix(g2_L16B_filter$`TotCpu%`), main="E-divisive g2_L16B_filter, alpha=1", ylim=c(50,300))
abline(v=Ediv1_filter$estimates[c(-1,-length(Ediv1_filter$estimates))], col="red", lty=2)

ts.plot(matrix(g2_L16B_filter$`TotCpu%`), main="E-divisive g2_L16B_filter, alpha=2")
abline(v=Ediv2_filter$estimates[c(-1,-length(Ediv2_filter$estimates))], col="blue", lty=2)

# # use get_average()
# Ediv1_filter_avg <- e.divisive(matrix(g2_L16B_filter_avg$value), R=499, alpha=1) 
# Ediv1_filter_avg$k.hat
# Ediv1_filter_avg$estimates
# 
# plot(g2_L16B_filter_avg$value, main="E-divisive g2_L16B_filter_avg, alpha=1", type="l", xaxt="n", ylim=c(50,300))
# abline(v=Ediv1_filter_avg$estimates[c(-1,-length(Ediv1_filter_avg$estimates))], col="red", lty=2)
# axis(1, at=1:nrow(g2_L16B_filter_avg), labels=g2_L16B_filter_avg$SW)

# use get_min()
Ediv1_filter_min <- e.divisive(matrix(g2_L16B_filter_min$`TotCpu%`), R=499, alpha=1) 
Ediv1_filter_min$k.hat # 4 clusters
Ediv1_filter_min$estimates

plot(g2_L16B_filter_min$`TotCpu%`, main="E-divisive g2_L16B_filter_min, alpha=1", type="l", xaxt="n", ylim=c(50,300))
abline(v=Ediv1_filter_min$estimates[c(-1,-length(Ediv1_filter_min$estimates))], col="red", lty=2)
axis(1, at=1:nrow(g2_L16B_filter_min), labels=g2_L16B_filter_min$SW)

# explore why ecp detect these three changes
library(quantmod)
Delt(g2_L16B_filter_min$`TotCpu%`)

# index 31 # around 1% change # DuProdName, Fdd/Tdd, NumCells are the same
# index 62 # around 2% change # same test environment 

#----------------------#
# # g2 FILTER data L17A
# # univariate: TotCpu%
# # E-divisive
# g2_L17A_filter_min <- get_min(g2_L17A_filter, "TotCpu%")
# Ediv1_L17A_filter_min <- e.divisive(matrix(g2_L17A_filter_min$`TotCpu%`), R=499, alpha=1) 
# Ediv1_L17A_filter_min$k.hat # 3 clusters
# Ediv1_L17A_filter_min$estimates
# 
# plot(g2_L17A_filter_min$`TotCpu%`, main="E-divisive g2_L17A_filter_min, alpha=1", type="l", xaxt="n", ylim=c(50,300))
# abline(v=Ediv1_L17A_filter_min$estimates[c(-1,-length(Ediv1_L17A_filter_min$estimates))], col="red", lty=2)
# axis(1, at=1:nrow(g2_L17A_filter_min), labels=g2_L17A_filter_min$SW)
# 
# # explore why ecp detect these three changes
# Delt(g2_L17A_filter_min$`TotCpu%`)
# 
# # index 31 # around 1% change # DuProdName, Fdd/Tdd, NumCells are the same
# # index 62 # around 8% change # same test environment 

#----------------------#
# implement e-divisive
# g2 data L16B train
Ediv1_train <- e.divisive(matrix(train_L16B$`TotCpu%`), R=499, alpha=1) 
Ediv2_train <- e.divisive(matrix(train_L16B$`TotCpu%`), R=499, alpha=2) 

Ediv1_train$k.hat # 16 clusters
# Ediv1_train$order.found
Ediv1_train$estimates 

Ediv2_train$k.hat # 13 clusters
# Ediv2_train$order.found
Ediv2_train$estimates 

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
Ediv2_train_filter$estimates # discard 124

ts.plot(matrix(train_L16B_filter$`TotCpu%`), main="E-divisive train_L16B_filter, alpha=1")
abline(v=Ediv1_train_filter$estimates[c(-1,-length(Ediv1_train_filter$estimates))], col="red", lty=2)

ts.plot(matrix(train_L16B_filter$`TotCpu%`), main="E-divisive train_L16B_filter, alpha=2")
abline(v=Ediv2_train_filter$estimates[c(-1,-length(Ediv2_train_filter$estimates))], col="blue", lty=2)

#----------------------#

train_L16B_avg <- get_average(train_L16B,"TotCpu%")
test_L16B_avg <- get_average(test_L16B,"TotCpu%")

# plot average TotCpu% for the same sw
ggplot(data=train_L16B_avg , aes(SW, value)) + geom_point()
plot(density(train_L16B_avg$value))
ggplot(data=test_L16B_avg, aes(SW, value)) + geom_point()

# g2 data L16B train_avg
Ediv1_train_avg <- e.divisive(matrix(train_L16B_avg$value), R=499, alpha=1) 
Ediv2_train_avg <- e.divisive(matrix(train_L16B_avg$value), R=499, alpha=2) 

Ediv1_train_avg$k.hat # 4 clusters
Ediv1_train_avg$estimates 

Ediv2_train_avg$k.hat # 4 clusters
Ediv2_train_avg$estimates # same

ggplot(data=train_L16B_avg , aes(SW, value, group=1)) + geom_line() + ggtitle("E-divisive train_L16B_avg, alpha=1") + geom_vline(xintercept=Ediv1_train_avg$estimates[c(-1,-length(Ediv1_train_avg$estimates))], color="red")
ts.plot(train_L16B_avg$value, main="E-divisive train_L16B_avg, alpha=1")
abline(v=Ediv1_train_avg$estimates[c(-1,-length(Ediv1_train_avg$estimates))], col="red", lty=2)

ts.plot(train_L16B_avg$value, main="E-divisive train_L16B_avg, alpha=2")
abline(v=Ediv2_train_avg$estimates[c(-1,-length(Ediv2_train_avg$estimates))], col="blue", lty=2)

#----------------------#
# g2 FILTER data L16B train_avg
train_L16B_filter_avg <- get_average(train_L16B_filter,"TotCpu%")
test_L16B_filter_avg <- get_average(test_L16B_filter,"TotCpu%")

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
Ediv2_train_filter_avg$estimates # discard 112

ggplot(data=train_L16B_filter_avg , aes(SW, value, group=1)) + geom_line() + ggtitle("E-divisive train_L16B_filter_avg, alpha=1") + geom_vline(xintercept=Ediv1_train_filter_avg$estimates[c(-1,-length(Ediv1_train_filter_avg$estimates))], color="red")
ts.plot(train_L16B_filter_avg$value, main="E-divisive train_L16B_filter_avg, alpha=1")
abline(v=Ediv1_train_filter_avg$estimates[c(-1,-length(Ediv1_train_filter_avg$estimates))], col="red", lty=2)

ts.plot(train_L16B_filter_avg$value, main="E-divisive train_L16B_filter_avg, alpha=2")
abline(v=Ediv2_train_filter_avg$estimates[c(-1,-length(Ediv2_train_filter_avg$estimates))], col="blue", lty=2)

