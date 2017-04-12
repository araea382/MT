# g2_org <- fread("master-g2.csv.txt") # some lines don't have 17 columns
# g2 <- read.table("master-g2.csv.txt", sep=",") # line 1147 did not have 17 elements



# testing sort: For SW
# x <- c("R2AKX","R18U","R9GT","R10AG")
# sort(x) # "R10AG" "R18U"  "R2AKX" "R9GT" 
# mixedsort(x) # "R2AKX" "R9GT"  "R10AG" "R18U" 
# 
# y <- c("18","2","1")
# z <- c("R18","R2","R1")
# sort(y) # "1" 18" "2" 
# sort(z) # "R1"  "R18" "R2" 
# mixedsort(y)
# mixedsort(z)


# g2_sort <- g2[order(Release, SW)] # problem with some sorting
# g2_sort <- g2[mixedorder(Release, SW)] # error # mixedorder does not support multiple columns

# another way if can't fix the 'mixedorder' is to sepearte Release first then sort by SW
# g2_L16A <- g2[which(Release == "L16A")]
# g2_L16A_sort <- g2_L16A[mixedorder(SW)]
# g2_L16B <- g2[which(Release == "L16B")]
# g2_L16B_sort <- g2_L16B[mixedorder(SW)]






