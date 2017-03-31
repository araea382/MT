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



n <- 400
b0=4
b1=0.8
b2=-0.1
b3=0.6
sigma=2
x1 <- runif(n,50,300) ## fix the X's
x2 <- sample(x=c(rep(0,0.5*n),runif(0.5*n,1200,1500)))
x3 <- sample(x=c(rep(0,0.8*n),runif(0.2*n,40,60)))
x4=factor(sample(x=c("A","B"), size=n, replace=TRUE, prob=c(0.1,0.9)))
eps <- rnorm(n, sd=sigma)
y <- b0 + b1*x1 + b2*x2 + b3*x3 -25*x4 + eps

summary(lm(y~x1+x2+x3+x4))


