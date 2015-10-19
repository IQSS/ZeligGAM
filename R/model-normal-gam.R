znormalgam <- setRefClass("Zelig-normal-gam",
                          contains = "Zelig-gam")


znormalgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-gam"
    .self$family <- "gaussian"
    .self$fn <- mgcv::gam
    .self$description <- "Generalized Additive Model for Normal Regression of Continuous Dependent Variables"
    .self$year <- 2011
    .self$category <- "continuous"
    .self$wrapper <- "normal.gam"
  }
)

znormalgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)

znormalgam$methods(
  set = function(z.out) {
    return()
  }
)


znormalgam$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

znormalgam$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

