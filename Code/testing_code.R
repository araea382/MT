# sapply(1:length(name_mod1),function(x){
#   if(name_mod1[x] == name_object[x]){
#     Coef[1,x] <- coef(mod1)[x]
#   }
# })

### .MSM.lm.msmFit
Coef=data.frame(matrix(NA,nrow=k,ncol=length(coef(object))))
names(Coef) <- names(coef(object))
ind=sample(1:k,length(object$residuals),replace=T)

for(i in 1:k){
  data1=as.data.frame(object$model[ind==i,,drop=F])
  mod1=update(object,formula=object$terms,data=data1)

  for(a in names(coef(mod1))){
    for(b in names(coef(object))){
      if(a == b){
        Coef[i,a] <- coef(mod1)[a]
      }
    }
  }

  std[i]=summary(mod1)$sigma
}
Coef



##-------------------------------------------------------------------------------------##
### .MSM.lm.msmFit
### probably
# for relevel the reference of the factor level
reref <- function(data, var){
  require(dplyr)
  require(lazyeval)
  count <- sapply(levels(data[,var]), function(value){
    filter_criteria <- interp(~y == x, .values=list(y=as.name(var), x=value))
    nrow(data %>% filter_(filter_criteria))
  })
  ind <- which.max(count)
  maxlev <- levels(data[,var])[ind]
  return(relevel(data[,var], maxlev))
}

train_g2_L16B_min$NumCells <- reref(train_g2_L16B_min, "NumCells")
train_g2_L16B_min$DuProdName <- reref(train_g2_L16B_min, "DuProdName")
train_g2_L16B_min$Fdd.Tdd <- reref(train_g2_L16B_min, "Fdd.Tdd")

train_g2_L16B_min$NumCells <- relevel(train_g2_L16B_min$NumCells, maxlev)

##-----------------------##
reref <- function(data, var){
  # require(dplyr)
  # require(lazyeval)
  # count <- sapply(levels(data[,var]), function(value){
  #   filter_criteria <- interp(~y == x, .values=list(y=as.name(var), x=value))
  #   nrow(data %>% filter_(filter_criteria))
  # })
  count <- sapply(levels(data[,var]), function(x) length(which(data[,var] == x)))
  ind <- which.max(count)
  maxlev <- levels(data[,var])[ind]
  return(relevel(data[,var], maxlev))
}


for(i in names(mod$contrasts)){
  object$model[,i] <- reref(object$model, i)
}
object <- update(object, data=data.frame(object$model))

# another way: change real data
for(i in names(object$contrasts)){
  data[,i]<- reref(object$model, i)
}
object=update(object,data=data)

data$NumCells <- relevel(data$NumCells, ref="12")
mod2 <- lm(fmla, data=data)
mod2
update(mod)


##-------------------------------------------------------------------------------------##
# see the sample of each regime
# c("RrcConnectionSetupComplete","Paging","X2HandoverRequest","Srb1SetupReject")
ind <- c(1,2,3,1,2,1,1,2,1,1,3,3,2,3,3,3,3,2,3,1,1,1,1,1,2,1,2,3,2,2,3,3,2,2,1,3,3,3,2,3,2,2,2,3,3,3,3,1,1,1,1,3,2,2,1,3,3,1,1,2,3,2,3,1,1,1,1,2,2,2,3,2,3,1,3,1,3,2,3,3,2,1,2,1,2,2,1,3,2,2,3,1,3,1,2,2,3,1,2,2,1,1,2,3,2,2,3,2,3,1,3,1,3,1,3,3,3,3,1,2,1,1,3,3,1,3,3,2,2,1,3,1,1,1,3,3,2,2,1,2,3,2,3,3,2,1,2,1,2,1,2,3,3,1,3,1,1,2,2,3,3,3,1,2,3,1,1,3,1,1,1,2,1,1,1,2,3,1,1,2,1,3,1,1,3,1,1,1,1,1,1)

i=1
data1=as.data.frame(object$model[ind==i,,drop=F])
mod1=update(object,formula=object$terms,data=data1)

i=2
data2=as.data.frame(object$model[ind==i,,drop=F])
mod2=update(object,formula=object$terms,data=data2)

i=3
data3=as.data.frame(object$model[ind==i,,drop=F])
mod3=update(object,formula=object$terms,data=data3)

summary(mod1)
summary(mod2)
summary(mod3)

##-------------------------------------------------------------------------------------##
### .MSM.lm.msmFilter
cond_mean <- function(terms, Coef, i){
  ind <- which(is.na(Coef[i,,drop=F]), arr.ind=TRUE)[,2] # get the index of the NA value
  Coef1 <- Coef[i,-ind,drop=F]
  terms_test <- terms[,-ind]
  condmean <- as.matrix(terms_test) %*% t(as.matrix(Coef1))
  return(condmean)
}

condmean <- sapply(1:k, function(x) cond_mean(terms, Coef, x))


##-------------------------------------------------------------------------------------##
### .MSM.lm.msmFit
### probably
# subset contains all factor levels

# categorical variable which has two factor levels
categ2 <- unlist(sapply(1:length(mod$contrasts), function(x){
  if(length(mod$xlevels[[x]]) == 2){
    names(mod$xlevels[x])
  }
}))

# count the second level of each variable
count <- c()
for(i in categ2){
  cnt <- list(sapply(levels(object$model[,i]), function(x) length(which(object$model[,i] == x))))
  count <- c(count,cnt)
}
names(count) <- categ2
var_name <- categ2[round(which.min(unlist(count))/2)]

# sub <- sapply(levels(object$model[,i]), function(x) nrow(subset(object$model, i == x)))
# sub <- lapply(levels(object$model$DuProdName), function(x) filter(object$model, DuProdName == x))
# cnt <- as.data.frame(count(object$model, DuProdName))

# get index for each subset from min variable
# indx <- c()
# for(i in 1:2){
#   ind <- sample(rep(1:k,length.out=sub[i]))
#   indx <- c(indx,ind)
# }

min_var <- count[[var_name]][2]
ind <- sample(rep(1:k, length.out=min_var))
ind <- c(sample(rep(1:k, length.out=(length(object$residuals)-min_var))),ind)
temp <- object$model[order(object$model[,var_name]),]

for(i in 1:k){
  data1=as.data.frame(temp[ind==i,,drop=F])
}

##-------------------------------------------------------------------------------------##
# test whether data can sort before update lm()
X <- c(1,8,2,4,1,5)
y <- c(2,10,6,9,8,4)
indx <- c(1,1,2,1,2,2)
mod <- lm(y~X)
coef(mod)

data1=as.data.frame(mod$model[indx==i,,drop=F])
mod1=update(mod,formula=mod$terms,data=data1)
coef(mod1)

data2=as.data.frame(mod$model[indx==i,,drop=F])
mod2=update(mod,formula=mod$terms,data=data2)
coef(mod2)

set.seed(12345)
df1 <- data1[sample(nrow(data1)),]
m1=update(mod,formula=mod$terms,data=df1)
coef(m1)

set.seed(12345)
df2 <- data2[sample(nrow(data2)),]
m2=update(mod,formula=mod$terms,data=df2)
coef(m2)

# Okkk it's the same! yeahh

##-------------------------------------------------------------------------------------##
# include in the package...? 
# state prediction function
#### NOT DONE ####

newdata <- test_g2_L16B_min[1,]

.MSM.lm.predict=function(object, newdata){
  p <- object@p
  model <- object["model"]
  Coef <- object["Coef"]
  std <- object["std"]
  P <- object["transMat"]
  fProb <- object["Fit"]["filtProb"]
  margLik <- object["Fit"]["margLik"]
  nr <- length(model$model[,1])
  
  if(p > 0){
    ar <- t(model$model[nr:(nr-p+1),1,drop=F]) # lag p
    colnames(ar) <- paste(names(model$model)[1],"_",1:p,sep="") # insert name
  }
  
  var <- colnames(model$model) # all variables name
  var <- var[1:(length(var)-p)] # discard AR term (if any)
  test <- subset(newdata, select=var) # subset (dependent and independent variables)
  test <- cbind(test, ar) # include back AR term
  
  # use reref() to relevel the reference of the factor level 
  for(i in names(model$contrasts)){
      test[,i] <- reref(test, i)
  }
  
  terms <- model.matrix(as.formula(paste(colnames(test)[1], " ~ ", paste(colnames(test)[-1], collapse= "+"))), data=test)
  CondMean <- as.matrix(terms) %*% t(as.matrix(Coef))
  error <- as.matrix(test[,1,drop=F]) %*% matrix(rep(1,k),nrow=1) - CondMean
  Likel <- t(dnorm(t(error),0,std))
  
  # add to original right away
  fProb <- rbind(fProb, t(P %*% t(fProb[nr,,drop=F])) * Likel[1,,drop=F]) 
  margLik <- rbind(margLik, sum(fProb[nr+1,]))
  fProb[nr+1,] <- fProb[nr+1,] / margLik[nr+1,1] # filtered prob of t+1 conditional on the info in t+1
  
  result <- which.max(fProb[nr+1,])
  return(result)
}
setMethod(f="predict",signature=c("MSM.lm","data.frame"),definition=.MSM.lm.predict)


#####
# add to original right away
fProb <- rbind(fProb, t(P %*% t(fProb[nr,,drop=F])) * Likel[1,,drop=F]) 
margLik <- rbind(margLik, sum(fProb[nr+1,]))
fProb[nr+1,] <- fProb[nr+1,] / margLik[nr+1,1] # filtered prob of t+1 conditional on the info in t+1

# add to orginial later
fProb_new <- t(P %*% t(fProb[nr,,drop=F])) * Likel[1,,drop=F]
margLik_new <- sum(fProb_new)
fProb <- rbind(fProb, (fProb_new / margLik_new)) # filtered prob of t+1 conditional on the info in t+1
margLik <- rbind(margLik, margLik_new)

# include CondMean, error, Likel back to the original one...?
# maybe not... seem like there is only fProb that use the previous one


