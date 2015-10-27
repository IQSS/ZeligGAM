# TEST POISSON

set.seed(0)
n <- 100
sig <- 2
x0 <- runif(n, 0, 1)
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
f0 <- function(x)
  2 * sin(pi * x)
f1 <- function(x)
  exp(2 * x)
f2 <- function(x)
  0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
f3 <- function(x)
  0 * x
f <- f0(x0) + f1(x1) + f2(x2)
g <- exp(f / 4)
y <- rpois(rep(1, n), g)

my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))

z5 <- zpoissongam$new()
z5$zelig(y ~ s(x0) + s(x1) + s(x2) + s(x3), data = my.data)
z5
z5$setx(x3 = quantile(my.data$x3, 0.2))
z5$setx1(x3 = quantile(my.data$x3, 0.8))
z5$setx.out$x$mm
z5$sim(1000)
z5$sim.out$x$ev
z5$sim.out$x$pv
z5
plot(z5)
