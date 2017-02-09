library(glmnet)
library(stringr)
library(MASS)

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
  data[is.na(data)] <- 0 # replace NA with 0 # should it be 0 or NA...?
  return(data)
}

g_new <- extract_component(g2_L16B[,1:17])
# total 187 variables (170 variables are added to data.frame)
# run time is approx. 2 mins


# tab <- apply(g2_L16B[1:2,1:17], 1, function(x){
#   events <- as.character(x[17])
#   st <- unlist(strsplit(events, " "))
#   for(i in 1:length(st)){
#     s <- strsplit(st[i],"=")
#     name <- s[[1]][1]
#     value <- as.numeric(s[[1]][2])
#     detect <- which(str_detect(names(x),name) == TRUE)
#     if(length(detect) == 0L){
#       x["name"] <- value
#       names(x) <- str_replace(names(x), "name", name)
#     }else{
#       x[detect] <- value
#     }
#   }
# })
# still cannot do it with apply or whatever their family are

#----------------------------------------------------------------------#
# convert DuProdName, Fdd/Tdd, NumCells to factor
g_new$DuProdName <- factor(g_new$DuProdName)
g_new$NumCells <- factor(g_new$NumCells)
g_new$`Fdd/Tdd` <- factor(g_new$`Fdd/Tdd`)

var_x <- as.matrix(g_new[,3:5])
fit <- glmnet(var_x, y, alpha=1) 

# fit linear regression
fit_rrc <- lm(`TotCpu%` ~ RrcConnectionSetupComplete, data=g_new)
summary(fit_rrc)
# significant: there is relationship between these two variables (56% of TotCpu% is due to the Rrc)
cor(g_new$`TotCpu%`, g_new$RrcConnectionSetupComplete) # 0.7468125

fit_num <- lm(`TotCpu%` ~ factor(NumCells), data=g_new)
summary(fit_num)
cor(g_new$`TotCpu%`, as.numeric(g_new$NumCells)) # 0.07159982

fit_dd <- lm(`TotCpu%` ~ factor(`Fdd/Tdd`), data=g_new)
summary(fitdd)
cor(g_new$`TotCpu%`, as.numeric(g_new$`Fdd/Tdd`)) # 0.0591176

fit_du <- lm(`TotCpu%` ~ factor(DuProdName), data=g_new)
summary(fit_du)
cor(g_new$`TotCpu%`, as.numeric(g_new$DuProdName)) # 0.1744523

fit <- lm(`TotCpu%` ~ RrcConnectionSetupComplete + factor(NumCells) + factor(`Fdd/Tdd`) + factor(DuProdName), data=g_new)
summary(fit)

#----------------------#
corr <- apply(subset(g_new, select=c(18:ncol(g_new))), 2, function(x){
  cor(g_new$`TotCpu%`,x)
}) # correlation between TotCpu% and each component in EverntsPerSec
corr2 <- as.data.frame(corr)


#----------------------#
temp <- subset(g_new, select=c(3:5,14,18:ncol(g_new)))
mod <- lm(`TotCpu%`~., data=temp)
summary(mod)

temp2 <- subset(g_new, select=c(14,18:ncol(g_new)))
mod2 <- lm(`TotCpu%`~., data=temp2)
summary(mod2)

# stepwise regression (forward-backward)
# step <- stepAIC(mod, direction="both")
step$coefficients
length(step$coefficients) - 1 # 86 variables
summary(step)
plot(step$residuals, type="l")

step2 <- stepAIC(mod2, direction="both")
step2$coefficients
length(step$coefficients) - 1 # 86 variables but different coeff than the first model
summary(step2)
plot(step2$residuals, main="model step 2") # 49, 50, 52
res <- step2$residuals

# different coeff
r1 <- names(step$coefficients)
r2 <- names(step2$coefficients)
setdiff(r1,r2)
setdiff(r2,r1)

# cook's distance
cook <- cooks.distance(step2)
plot(cook)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(g_new)-length(fit$coefficients)-2)) 
plot(step2, which=4, cook.levels=cutoff) 
# seem like obs. 643 and 1099 are considered as influential
# cook's distance refers to how far, on average, predicted y-values will move if the observation is dropped from the data set.

lev <- hat(model.matrix(step2))
plot(lev)

plot(g_new$`TotCpu%`, step2$residuals)

#----------------------#
# ecp with residuals
Ediv_res <- e.divisive(matrix(res), R=499, alpha=1) 
Ediv_res$k.hat 
Ediv_res$estimates

plot(res, main="E-divisive res, alpha=1", type="l")
abline(v=Ediv_res$estimates[c(-1,-length(Ediv_res$estimates))], col="red", lty=2)


#----------------------#
n=dim(g_new)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=g_new[id,]
test=g_new[-id,]

y_train <- as.matrix(train$`TotCpu%`)
X_train <- as.matrix(subset(train, select=c(18:ncol(g_new)))) 
y_test <- as.matrix(test$`TotCpu%`)
X_test <- as.matrix(subset(test, select=c(18:ncol(g_new)))) 

# y <- as.matrix(g_new$`TotCpu%`)
# X <- as.matrix(subset(g_new, select=c(18:ncol(g_new)))) 


# g_new2 <- g_new
# valid_column_names <- make.names(names=names(g_new2), unique=TRUE, allow_ = TRUE)
# names(g_new2) <- valid_column_names
# X <- dplyr::select(g_new2, DuProdName:NumCells, DownlinkNasTransport:SmcTimeout) # cannot use with the original column name



#----------------------#
# ridge and lasso can not enter factor directly. It needs to transform to dummy variables first
# ridge regression 
ridge <- glmnet(X, y, alpha=0, family="gaussian")
plot(ridge, xvar="lambda", label=TRUE)

set.seed(12345)
ridge_cv <- cv.glmnet(X, y, alpha=0, family = "gaussian")
plot(ridge_cv)
penalty <- ridge_cv$lambda.min
fit_ridge <- glmnet(X, y, alpha=0, lambda=penalty)
coef(fit_ridge) # shrink

#----------------------#
# lasso regression
lasso <- glmnet(X, y, alpha=1, family="gaussian") 
plot(lasso, xvar="lambda", label=TRUE)

set.seed(12345)
lasso_cv <- cv.glmnet(X, y, alpha=1, family = "gaussian")
plot(lasso_cv)
penalty <- lasso_cv$lambda.min
fit_lasso <- glmnet(X, y, alpha=1, lambda=penalty) 
coef(fit_lasso) # 39 variables, highest coef is "PerBbLcgEvent"

# try with train and test set 
lasso <- glmnet(X_train, y_train, alpha=1, family="gaussian") 
plot(lasso, xvar="lambda", label=TRUE)

set.seed(12345)
lasso_cv <- cv.glmnet(X_train, y_train, alpha=1, family = "gaussian")
plot(lasso_cv)
penalty <- lasso_cv$lambda.min
fit_lasso <- glmnet(X_train, y_train, alpha=1, lambda=penalty) 
coef(fit_lasso) # 31 variables
print(fit_lasso)

pred <- predict(fit_lasso, newx=X_test)
r <- y_test - pred
plot(r)

# include test environment variables (factor)
mat <- model.matrix(~., data=subset(g_new, select=c(3,4,5,18:ncol(g_new))))
las <- cv.glmnet(mat, y, alpha=1, family = "gaussian")
plot(las)
penalty <- las$lambda.min
fit_las <- glmnet(mat, y, alpha=1, lambda=penalty) 
coef(fit_las) # 39 variables
print(fit_las)

