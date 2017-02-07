library(glmnet)

events <- as.character(g2_L16B[1,17]) # get content from EventsPerSec column
st <- unlist(strsplit(events, " "))
value <- str_subset(st, "RrcConnectionSetupComplete")
if(length(value) == 0L){
  0
}else{
  as.numeric(str_replace(value, "RrcConnectionSetupComplete=", "")) # replace name with blank in order to get only value
}



fit <- lm(`TotCpu%` ~ RrcConnectionSetupComplete + factor(NumCells) + factor(`Fdd/Tdd`) + factor(DuProdName), data=g2_L16B)
