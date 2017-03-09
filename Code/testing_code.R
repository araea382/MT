sapply(1:length(name_mod1),function(x){
  if(name_mod1[x] == name_object[x]){
    Coef[1,x] <- coef(mod1)[x]
  }
})

for(a in name_mod1){
  for(b in name_object){
    if(a == b){
      Coef[,a] <- coef(mod1)[a]
    }
  }
}


####
Coef=data.frame(matrix(NA,nrow=k,ncol=length(coef(object))))
names(Coef) <- names(coef(object))

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
