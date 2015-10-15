zgam <- setRefClass("Zelig-gam",
                        contains = "Zelig")


zgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gam"
    # .self$fn <- zlavaan
    .self$authors <- "Skyler J. Cranmer"
    # .self$description <- "Structural Equation Model"
    .self$year <- 2011
    .self$category <- "continuous"
    # .self$wrapper <- "gam"
  }
)

zgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)

zgam$methods(
  set = function(z.out) {
    return()
  }
)

##----- QI's need to be defined

zgam$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

zgam$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

