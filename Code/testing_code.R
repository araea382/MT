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


##-------------------------------------------------------------------------------------##
# see the sample of each regime
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


