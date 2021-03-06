# Use data from explore.R
# g2_L16B
# convert DuProdName, Fdd/Tdd, NumCells to factor
g2_L16B$DuProdName <- factor(g2_L16B$DuProdName)
g2_L16B$NumCells <- factor(g2_L16B$NumCells)
g2_L16B$`Fdd/Tdd` <- factor(g2_L16B$`Fdd/Tdd`)

# var_x <- as.matrix(g2_L16B[,3:5])
# fit <- glmnet(var_x, y, alpha=1) 

# fit linear regression
fit_rrc <- lm(`TotCpu%` ~ RrcConnectionSetupComplete, data=g2_L16B)
summary(fit_rrc)
# significant: there is relationship between these two variables (56% of TotCpu% is due to the Rrc)
cor(g2_L16B$`TotCpu%`, g2_L16B$RrcConnectionSetupComplete) # 0.7468125

fit_num <- lm(`TotCpu%` ~ factor(NumCells), data=g2_L16B)
summary(fit_num)
cor(g2_L16B$`TotCpu%`, as.numeric(g2_L16B$NumCells)) # 0.07137698

fit_dd <- lm(`TotCpu%` ~ factor(`Fdd/Tdd`), data=g2_L16B)
summary(fit_dd)
cor(g2_L16B$`TotCpu%`, as.numeric(g2_L16B$`Fdd/Tdd`)) # 0.05899498

fit_du <- lm(`TotCpu%` ~ factor(DuProdName), data=g2_L16B)
summary(fit_du)
cor(g2_L16B$`TotCpu%`, as.numeric(g2_L16B$DuProdName)) # 0.1748102

fit <- lm(`TotCpu%` ~ RrcConnectionSetupComplete + factor(NumCells) + factor(`Fdd/Tdd`) + factor(DuProdName), data=g2_L16B)
summary(fit)

#----------------------#
corr <- apply(subset(g2_L16B, select=c(18:ncol(g2_L16B))), 2, function(x){
  cor(g2_L16B$`TotCpu%`,x)
}) # correlation between TotCpu% and each component in EverntsPerSec
corr2 <- as.data.frame(corr)

corr3 <- corr2
corr3$name <- rownames(corr2)
rownames(corr3) <- NULL

corr4 <- corr3
corr4 <- corr4[order(corr4$corr),]


#----------------------#
# subset include test environment
temp <- subset(g2_L16B, select=c(3:5,14,18:ncol(g2_L16B)))
mod <- lm(`TotCpu%`~., data=temp)
summary(mod)

temp2 <- subset(g2_L16B, select=c(14,18:ncol(g2_L16B)))
mod2 <- lm(`TotCpu%`~., data=temp2)
summary(mod2)

corr1 <- cor(temp2)

# library(car)
# vif(mod2) # collinearity

# stepwise regression (forward-backward)
step <- stepAIC(mod, direction="both")
step$coefficients
length(step$coefficients) - 1 # 86 variables
summary(step)
plot(step$residuals, type="l")

step2 <- stepAIC(mod2, direction="both")
step2$coefficients
length(step2$coefficients) - 1 # 82 variables but different coeff than the first model
summary(step2)
plot(step2$residuals, main="model step 2")
res <- step2$residuals

# # different coeff
# r1 <- names(step$coefficients)
# r2 <- names(step2$coefficients)
# setdiff(r1,r2)
# setdiff(r2,r1)
# 
# # cook's distance
# cook <- cooks.distance(step2)
# plot(cook)
# 
# # Cook's D plot
# # identify D values > 4/(n-k-1) 
# cutoff <- 4/((nrow(g2_L16B)-length(fit$coefficients)-2)) 
# plot(step2, which=4, cook.levels=cutoff) 
# # seem like obs. 643 and 1099 are considered as influential
# # cook's distance refers to how far, on average, predicted y-values will move if the observation is dropped from the data set.
# 
# lev <- hat(model.matrix(step2))
# plot(lev)
# 
# plot(g2_L16B$`TotCpu%`, step2$residuals)

#----------------------#
# ecp with residuals
# Ediv_res <- e.divisive(matrix(res), R=499, alpha=1) 
# Ediv_res$k.hat 
# Ediv_res$estimates
# 
# plot(res, main="E-divisive res, alpha=1", type="l")
# abline(v=Ediv_res$estimates[c(-1,-length(Ediv_res$estimates))], col="red", lty=2)

#----------------------#
# n=dim(g2_L16B)[1]
# set.seed(12345)
# id=sample(1:n, floor(n*0.5))
# train=g2_L16B[id,]
# test=g2_L16B[-id,]
# 
# y_train <- as.matrix(train$`TotCpu%`)
# X_train <- as.matrix(subset(train, select=c(18:ncol(g2_L16B)))) 
# y_test <- as.matrix(test$`TotCpu%`)
# X_test <- as.matrix(subset(test, select=c(18:ncol(g2_L16B)))) 

y <- as.matrix(g2_L16B$`TotCpu%`)
X <- as.matrix(subset(g2_L16B, select=c(18:ncol(g2_L16B))))

# g2_L16B_2 <- g2_L16B
# valid_column_names <- make.names(names=names(g2_L16B_2), unique=TRUE, allow_ = TRUE)
# names(g2_L16B_2) <- valid_column_names
# X <- dplyr::select(g2_L16B_2, DuProdName:NumCells, DownlinkNasTransport:SmcTimeout) # cannot use with the original column name

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
coef(fit_lasso) # 38

# try with train and test set 
# lasso <- glmnet(X_train, y_train, alpha=1, family="gaussian") 
# plot(lasso, xvar="lambda", label=TRUE)
# 
# set.seed(12345)
# lasso_cv <- cv.glmnet(X_train, y_train, alpha=1, family = "gaussian")
# plot(lasso_cv)
# penalty <- lasso_cv$lambda.min
# fit_lasso <- glmnet(X_train, y_train, alpha=1, lambda=penalty) 
# coef(fit_lasso) # 31 variables
# print(fit_lasso)
# 
# pred <- predict(fit_lasso, newx=X_test)
# r <- y_test - pred
# plot(r)

# # include test environment variables (factor)
# mat <- model.matrix(~., data=subset(g2_L16B, select=c(3,4,5,18:ncol(g2_L16B))))
# las <- cv.glmnet(mat, y, alpha=1, family = "gaussian")
# plot(las)
# penalty <- las$lambda.min
# fit_las <- glmnet(mat, y, alpha=1, lambda=penalty) 
# coef(fit_las)
# print(fit_las)

#----------------------#
# elastic net
elastic <- glmnet(X, y, alpha=0.5, family="gaussian") 
plot(elastic, xvar="lambda", label=TRUE)

set.seed(12345)
elastic_cv <- cv.glmnet(X, y, alpha=0.5, family = "gaussian")
plot(elastic_cv)
penalty <- elastic_cv$lambda.min
fit_elastic <- glmnet(X, y, alpha=0.5, lambda=penalty) 
coef(fit_elastic) # 38


#----------------------------------------------------------------------#
#----------------------------------------------------------------------#
#----------------------------------------------------------------------#




