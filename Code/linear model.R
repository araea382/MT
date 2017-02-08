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

# fit linear regression
fit <- lm(`TotCpu%` ~ RrcConnectionSetupComplete, data=g_new)
summary(fit)
# significant: there is relationship between these two variables (55% of TotCpu% is due to the Rrc)
cor(g_new$`TotCpu%`, g_new$Normalize) # 0.1810879

fit <- lm(`TotCpu%` ~ factor(NumCells), data=g_new)
summary(fit)

fit <- lm(`TotCpu%` ~ factor(`Fdd/Tdd`), data=g_new)
summary(fit)

fit <- lm(`TotCpu%` ~ factor(DuProdName), data=g_new)
summary(fit)

fit <- lm(`TotCpu%` ~ RrcConnectionSetupComplete + factor(NumCells) + factor(`Fdd/Tdd`) + factor(DuProdName), data=g_new)
summary(fit)

y <- as.matrix(g_new$`TotCpu%`)
X <- as.matrix(subset(g_new, select=c(18:ncol(g_new)))) 

# g_new2 <- g_new
# valid_column_names <- make.names(names=names(g_new2), unique=TRUE, allow_ = TRUE)
# names(g_new2) <- valid_column_names
# X <- dplyr::select(g_new2, DuProdName:NumCells, DownlinkNasTransport:SmcTimeout) # cannot use with the original column name

temp <- subset(g_new, select=c(3:5,14,18:ncol(g_new)))
mod <- lm(`TotCpu%`~., data=temp)

#----------------------#
# stepwise regression (forward-backward)
# step <- stepAIC(mod, direction="both")
step$coefficients
length(step$coefficients) - 1 # 86 variables

#----------------------#
# ridge and lasso can not enter factor
# ridge regression 
ridge <- glmnet(X, y, alpha=0, family="gaussian")
plot(ridge, xvar="lambda", label=TRUE)

set.seed(12345)
ridge_cv <- cv.glmnet(X, y, alpha=0, family = "gaussian")
plot(ridge_cv)
penalty <- ridge_cv$lambda.min
fit2 <- glmnet(X, y, alpha=0, lambda=penalty)
coef(fit2) # shrink

#----------------------#
# lasso regression
lasso <- glmnet(X, y, alpha=1, family="gaussian") 
plot(lasso, xvar="lambda", label=TRUE)

set.seed(12345)
lasso_cv <- cv.glmnet(X, y, alpha=1, family = "gaussian")
plot(lasso_cv) # 55 variables
penalty <- lasso_cv$lambda.min
fit1 <- glmnet(X, y, alpha=1, lambda=penalty)
coef(fit1) # highest coef is "PerBbLcgEvent"



