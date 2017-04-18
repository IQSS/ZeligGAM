# REQUIRE TEST minimal logit.gam doc example -----------------------------------

test_that('REQUIRE TEST minimal logit.gam doc example', {
  set.seed(0)
  n <- 100
  sig <- 2
  x0 <- runif(n, 0, 1)
  x1 <- runif(n, 0, 1)
  x2 <- runif(n, 0, 1)
  x3 <- runif(n, 0, 1)
  f0 <- function(x) 2 * sin(pi * x)
  f1 <- function(x) exp(2 * x)
  f2 <- function(x) 0.2 * x ^ 11 * (10 * (1 - x)) ^ 6 + 10 * (10 * x) ^ 3 * (1 - x) ^ 10
  f2 <- function(x) 0 * x

  f <- f0(x0) + f1(x1) + f2(x2)

  g <- (f - 5) / 3
  g <- binomial()$linkinv(g)

  y <- rbinom(g,1,g)

  my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))

  z.out <- zelig(y ~ s(x0) + s(x1) + s(x2) + s(x3), model = "logit.gam", 
                data = my.data)
})
