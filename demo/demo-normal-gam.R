set.seed(0); n <- 400; sig <- 2;
x0 <- runif(n, 0, 1); x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1); x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2 * x^11 * (10 * (1 - x))^6 + 10 * (10 *
                                                            + x)^3 * (1 - x)^10
f3 <- function(x) 0 * x
f <- f0(x0) + f1(x1) + f2(x2)
e <- rnorm(n, 0, sig); y <- f + e
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))
# Estimate the model, summarize the results, and plot nonlinearities:
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="normal.gam", data=my.data)

z5 <- znormalgam$new()
z5$zelig(y~s(x0)+s(x1)+s(x2)+s(x3), data=my.data)
z5


z.out <- z5$zelig.out$z.out[[1]]
summary(z.out)
str(z.out$smooth[[1]]$term)
str(z.out$smooth[[1]]$df)
