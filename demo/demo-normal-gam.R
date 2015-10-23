set.seed(0); n <- 400; sig <- 2;
x0 <- runif(n, 0, 1); x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1); x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 * + x)^3 * (1 - x)^10
f3 <- function(x) 0 * x
f <- f0(x0) + f1(x1) + f2(x2)
e <- rnorm(n, 0, sig); y <- f + e
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))

gam(y~s(x0)+s(x1)+s(x2)+s(x3), model="normal.gam", data=my.data)


# Estimate the model, summarize the results, and plot nonlinearities:
z.out <- zelig(y ~ s(x0) + s(x1) + s(x2) + s(x3), model = "normal.gam", data = my.data)

z5 <- znormalgam$new()
z5$zelig(y~s(x0)+s(x1)+s(x2)+s(x3), data=my.data)
z5

z.out <- z5$zelig.out$z.out[[1]]
summary(z.out)
str(z.out$smooth[[1]]$term)
str(z.out$smooth[[1]]$df)

zelig(y~s(x0)+s(x1)+s(x2)+s(x3),min.sp=c(0.001,0.01,0,10),model="normal.gam", data=my.data)

zelig(y~s(x0,k=4,fx=TRUE,bs="tp")+s(x1,k=12)+s(x2,k=15), model="normal.gam", data=my.data)

# z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="normal.gam", data=my.data)
# z5 <- z.out$zelig.out$z.out[[1]]
# dat <- z.out$originaldata
# n <- repeat.data(z5, dat)

z5 <- znormalgam$new()
z5$zelig(y~s(x0)+s(x1)+s(x2)+s(x3), data=my.data)
z5$setx()
z5$setx(x3 = 0)
z5
z5$setx.out$x$mm[[1]]

z5$simparam()

model.matrix(z.out, )

z5$sim()
z5

z.out <- z5$zelig.out$z.out[[1]]
class(z.out)

debug(z5$set)

reduce.gam()

x.out <- setx(z.out)
x.out$setx.out

f <- z5$formula
d <- z5$originaldata
reduce(z5$originaldata, list(x3 = 0), f, d)

model.frame.gam(z5$zelig.out$z.out[[1]], data = data.frame(z5$setx.out$x$mm[[1]]))

model.matrix.gam(z5$zelig.out$z.out[[1]], data = z5$originaldata)

require(mgcv)
n <- 15
x <- runif(n)
y <- sin(x*2*pi) + rnorm(n)*.2
mod <- gam(y~s(x,bs="cc",k=6),knots=list(x=seq(0,1,length=6)))
model.frame(gam)
model.matrix.gam(formula(mod))
