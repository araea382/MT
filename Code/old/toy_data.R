LRR <- c (0.3362 , 0.2793 , 0.3742 , 0.4424 , 0.1238 , 0.3590 , 0.1655 , 0.3552 ,
          0.1504 , 0.2983 , 0.3173 , 0.1428 , 0.1276 , 0.8824 , 0.3438 , 0.2642 , 0.1390 ,
          0.3211 , 0.4235 , 0.2338 , 0.2983 , 0.2376 , 0.2452 , 0.3362 , 0.3400 , 0.3248 ,
          0.3666 , 0.4766 , 0.7193 , 0.3135 , 0.1883 , 0.1466 , 0.3211 , 0.3779 , 0.2376 ,
          0.1655 , 0.3173 , 0.0252 , 0.2528 , 0.3324 , 0.2528 , 0.2755 , 0.2945 , 0.1997 ,
          0.2376 , 0.1807 , 0.6510 , 0.3552 , 0.1883 , 0.1655 , 0.2680 , 0.2642 , 0.2680 ,
          0.1883 , 0.1997 , 0.1693 , 0.3362 , 0.2111 , 0.2755 , 0.2680 , 0.2680 , 0.2983 ,
          0.1162 , 0.3476 , 0.3817 , 0.8331 , 0.3097 , 0.2376 , 0.0556 , 0.0707 , 0.1655 ,
          0.1769 , 0.2111 , -0.0696 , 0.2149 , 0.0214 , 0.1238 , -0.7333 , -0.7409 ,
          0.0366 , 0.0897 , 0.1769 , 0.0404 , 0.0935 , 0.1466 , 0.6169 , 0.1921 , 0.2300 ,
          0.3476 , 0.1921 , 0.6055 , 0.0935 , 0.0518 , -0.0582 , -0.6423 , 0.4083 ,
          -0.7864 , -0.8964 , -0.6120 , -0.7258 , -0.6347 , -0.7940 , -0.6120 , -0.6006 ,
          -0.1986 , -0.6044 , -0.4754 , -0.6234 , -0.7030 , -0.5323 , 0.3097 , -0.8395 ,
          -1.0064 , -0.8206 , -0.8851 , -0.0506 , -0.7409 , -0.7296 , -1.5526 , -0.1455)


output <- e.divisive(as.matrix(LRR), R=499, alpha=1)
output$k.hat
output$estimates

ts.plot(LRR)
abline(v=output$estimates[c(-1,-4)], col="red", lty=2)

accidents <- c(4, 5, 4, 1, 0, 4, 3, 4, 0, 6, 3, 3, 4, 0, 2, 6, 3, 3, 5,
               4, 5, 3, 1, 4, 4, 1, 5, 5, 3, 4, 2, 5, 2, 2, 3, 4, 2, 1, 3, 2, 2, 1, 1,
               1, 1, 3, 0, 0, 1, 0, 1, 1, 0, 0, 3, 1, 0, 3, 2, 2, 0, 1, 1, 1, 0, 1, 0,
               1, 0, 0, 0, 2, 1, 0, 0, 0, 1, 1, 0, 2, 3, 3, 1, 1, 2, 1, 1, 1, 1, 2, 4,
               2, 0, 0, 0, 1, 4, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1)

output <- e.divisive(as.matrix(accidents), R=499, alpha=1)
output$k.hat
output$estimates

ts.plot(accidents)
abline(v=output$estimates[c(-1,-3)], col="red", lty=2)


data <- read.csv("C:/Users/EARAEAM/Downloads/NCHSData.csv")
res = breakout(data$Percent.of.Deaths.Due.to.P.I, min.size=4, method='multi', percent=0.02, degree=1, plot=TRUE)
res$plot

# create an object of the plot so i dont have to use $
stuff<-res$plot
# create the x labels
data$wkyr<-paste(data$Week, data$Year, sep="-")
# get every 10th observation and put it into a new vector of just the week/year labels for the plot
sub1<- data[seq(1, nrow(data), by=10), ]
wkyr2<-sub1$wkyr
# replot
library(ggplot2)
stuff + labs(y="Percent of All Deaths Due to \nPneumonia and Influenza",
             x="Week-Year") + scale_x_continuous(breaks = c(seq(from = 1, to = 261, by = 10)),
                                                 labels = wkyr2) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

