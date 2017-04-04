# simulation data
# one response xiable
# six predictors variables: three categorical and three continuous variables

set.seed(1)
n <- 400
dat <- data.frame(y=runif(n,0,300),
                  x1=runif(n,0,300),
                  x2=runif(n,0,1400),
                  x3=runif(n,0,100),
                  x4=factor(sample(x=c("A","B"), size=n, replace=TRUE, prob=c(0.1,0.9))),
                  x5=factor(sample(x=c("F","T"), size=n, replace=TRUE, prob=c(0.8,0.2))),
                  x6=factor(sample(x=c("1","2","3"), size=n, replace=TRUE, prob=c(0.7,0.2,0.1))),
                  state=sample(x=c("normal","good","bad"), size=n, replace=TRUE, prob=c(0.4,0.3,0.3)))

dat <- data.frame(x1=runif(n,50,300),
                  x2=sample(x=c(rep(0,0.5*n),runif(0.5*n,1200,1500))),
                  x3=sample(x=c(rep(0,0.8*n),runif(0.2*n,40,60))),
                  state=sample(x=c("normal","good","bad"), size=n, replace=TRUE, prob=c(0.4,0.3,0.3)))
y <- 100 + 0.2*dat$x1
dat <- data.frame(dat,y)

# y_1 <- c(NA,dat$y[-n])
# dat <- cbind(dat,y_1) # first obs will be gone

ind <- which(colnames(dat) == "state")
train_state <- dat[1:360,]
train <- train_state[,-ind]
test_state <- dat[-c(1:360),]
test <- test_state[,-ind]

mod <- lm(y~., data=train)
summary(mod)

##
k=3
p=1
sw=rep(T,length(mod$coefficients)+p+1)
control=list(trace = T,  maxiter = 500, tol = 1e-8, maxiterInner=10, maxiterOuter=5, parallelization=F)
data <- train
object <- mod
##

set.seed(1)
mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
summary(mswm)


pred <- predict(mswm, test)


#---------------------------------------------------#
n <- 400
b0 <- 22
b1 <- 0.8
b2 <- -0.01
b3 <- 0.6
sigma <- 10

set.seed(1)
x1 <- runif(n,50,300)
x2 <- sample(x=c(rep(0,0.5*n),runif(0.5*n,1300,1500)))
x3 <- sample(x=c(rep(0,0.8*n),runif(0.2*n,40,60)))
# state <- sample(x=c("normal","good","bad"), size=n, replace=TRUE, prob=c(0.4,0.3,0.3))
# eps <- rnorm(n, sd=sigma)
# y <- 10 + b1*x1 + b2*x2 + b3*x3 + eps

dat <- data.frame(x1,x2,x3)

ind <- which(colnames(dat) == "state")
train_state <- dat[1:360,]
train <- train_state[,-ind]
test_state <- dat[-c(1:360),]
test <- test_state[,-ind]

mod <- lm(y~., data=train)
summary(mod)

set.seed(1)
mswm <- MSwM2::msmFit(mod, k=3, p=1, sw=rep(TRUE,length(mod$coefficients)+1+1),control=list(trace=TRUE,maxiter=500,parallel=FALSE))
summary(mswm)


#---------------------------------------------------#
# three states
set.seed(1)
n <- 500
x <- runif(n,50,200)
e1 <- rnorm(n,0,1)
e2 <- rnorm(n,2,0.5)
e3 <- rnorm(n,1,2)

y1 <- 10 + 0.6*x + e1
y2 <- 2 + 0.8*x + e2
y3 <- -15 + 0.5*x + e3

plot(y1, type="l", ylim=c(0,300)) # normal
points(y2, type="l", col="red") # bad
points(y3, type="l", col="orange") # good

state <- rep(0,n)
ind_normal <- c(1:50,71:110,161:220,351:370,421:450)
ind_bad <- c(51:70,241:290,371:420,451:491)
ind_good <- c(111:160, 221:240,291:350,491:500)
state[ind_normal] <- "normal"
state[ind_bad] <- "bad"
state[ind_good] <- "good"
state

# y <- c(y1[1:50],y2[51:70],y1[71:110],y3[111:160],y1[161:220],y3[221-240],y2[241:290],y3[291:350],y1[351:370],y2[371:420],y1[421:450],y2[451:491],y3[491:500])
y <- rep(0,n)
y[ind_normal] <- y1[ind_normal]
y[ind_bad] <- y2[ind_bad]
y[ind_good] <- y3[ind_good]

points(y, type="l", col="green")
plot(y, type="l")

dat <- data.frame(x,y)

train <- dat[1:400,]
test <- dat[401:500,]
mod <- lm(y~., data=train)
summary(mod)

