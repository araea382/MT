# try using k-means clustering algorithm
g <- subset(g_new, select=c(14,18:ncol(g_new)))
set.seed(12345)
clust <- kmeans(g, centers=5, nstart=250, iter.max=1000)
ggplot(g_new, aes(x=SW, y=`TotCpu%`, color=as.factor(clust$cluster))) + geom_point()
ggplot(g_new, aes(x=RrcConnectionReconfiguration, y=`TotCpu%`, color=as.factor(clust$cluster))) + geom_point()
ggplot(g_new, aes(x=seq_along(`TotCpu%`), y=`TotCpu%`,color=as.factor(clust$cluster))) + geom_point()


# to select the K
# http://www.learnbymarketing.com/tutorials/k-means-clustering-in-r-example/
rng <- 2:20 #K from 2 to 20
tries <- 100 #Run the K Means algorithm 100 times
avg.totw.ss <- integer(length(rng)) #Set up an empty vector to hold all of points
for(v in rng){ # For each value of the range variable
  v.totw.ss <-integer(tries) #Set up an empty vector to hold the 100 tries
  for(i in 1:tries){
    k.temp <-kmeans(g,centers=v) #Run kmeans
    v.totw.ss[i] <-k.temp$tot.withinss#Store the total withinss
  }
  avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
     ylab="Average Total Within Sum of Squares",
     xlab="Value of K")
# don't know.... perhaps 5 or 7

mydata <- g
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)


#----------------------#
g$cluster <- clust$cluster
model2 <- depmix(cluster ~ 1, data=g, nstates=3, family=multinomial())

set.seed(12345)
fitted <- fit(model2)
summary(fitted)

# FIXXXXXXXXXXXXXXXXXXX
prob <- posterior(fitted) # Compute probability of being in each state
head(prob) # we can see that we now have the probability for each state for everyday as well as the highest probability class.
# rowSums(head(prob)[,2:4]) # Check that probabilities sum to 1

dat <- data.frame(g_new$SW, g$cluster, prob$state)
cpu <- ggplot(dat,aes(x=dat[,"g_new.SW"],y=dat[,"g.cluster"])) + geom_line(color="darkgreen") + labs(title="TotCpu%",y="TotCpu%",x="SW")
regime <- ggplot(dat,aes(x=dat[,"g_new.SW"],y=dat[,"prob.state"])) + geom_line(color="red") + labs(title="Regime",y="Regime",x="SW")
grid.arrange(cpu, regime)

par(mfrow=c(2,1))
ts.plot(matrix(g$`TotCpu%`))
plot(1:nrow(prob), prob[,1], type='l')

# # The probability of each regime separately
# dat2 <- data.frame(temp2, prob)
# regime1 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,19])) + geom_line(color="purple") + labs(title="Regime 1",y="Probability",x="SW")
# regime2 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,20])) + geom_line(color="orange") + labs(title="Regime 2",y="Probability",x="v")
# regime3 <- ggplot(dat2,aes(x=as.numeric(dat2[,7]),y=dat2[,21])) + geom_line(color="darkblue") + labs(title="Regime 3",y="Probability",x="SW")
# grid.arrange(regime1, regime2, regime3)
# 

#----------------------#
# try pca
g <- subset(g_new, select=c(18:ncol(g_new)))
pca <- prcomp(g)
lambda <- pca$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100) # Variation explained(%)
plot(pca, type="l")
# 3 or 4 
summary(pca)
biplot(pca)

comp <- data.frame(pca$x[,1:4])
plot(comp)

plot(pca$x[,1], pca$x[,2], xlab="PC1", ylab="PC2")
U=pca$rotation
plot(U[,1], main="Traceplot, PC1")
plot(U[,2],main="Traceplot, PC2")
plot(U[,3],main="Traceplot, PC3")
plot(U[,4],main="Traceplot, PC4")

#----------------------#
# try pca first then k-means
wss <- (nrow(comp)-1)*sum(apply(comp,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comp,centers=i, nstart=25, iter.max=1000)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")

k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=k$clust, pch=16)

library(rgl)
# Multi 3D plot
plot3d(comp$PC1, comp$PC2, comp$PC3)
plot3d(comp$PC1, comp$PC3, comp$PC4)

plot3d(comp$PC1, comp$PC2, comp$PC3, col=k$clust)
plot3d(comp$PC1, comp$PC3, comp$PC4, col=k$clust)

# Cluster sizes
sort(table(k$clust))
clust <- names(sort(table(k$clust)))


# First cluster
row.names(g[k$clust==clust[1],])
# Second Cluster
row.names(g[k$clust==clust[2],])
# Third Cluster
row.names(g[k$clust==clust[3],])
# Fourth Cluster
row.names(g[k$clust==clust[4],])

#----------------------------------------------------------------------#
# try new subset with selected components
g_new2 <- subset(g_new, select=c("TotCpu%","RrcConnectionSetupComplete","X2HandoverRequest","Paging","S1InitialUeMessage","ReEstablishmentAttempt","PerBbUeEvent","ErabDrbRelease"))
g_new2 <- cbind(SW=g_new$SW, g_new2)

set.seed(12345)
clust2 <- kmeans(g_new2[,-1], centers=5, nstart=10, iter.max=10)
clust2$centers
clust2$size
ggplot(g_new2, aes(x=SW, y=`TotCpu%`, color=as.factor(clust2$cluster))) + geom_point()
ggplot(g_new2, aes(x=seq_along(`TotCpu%`), y=`TotCpu%`,color=as.factor(clust2$cluster))) + geom_point()

# predict cluster for new observation
library(clue)
test <- g_new2[1,-1]
test <- subset(g, select=c("TotCpu%","RrcConnectionSetupComplete","X2HandoverRequest","Paging","S1InitialUeMessage","ReEstablishmentAttempt","PerBbUeEvent","ErabDrbRelease"))[256,]
cl_predict(clust2, test)
