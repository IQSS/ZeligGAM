zpoissongam <- setRefClass("Zelig-poisson-gam",
                           contains = "Zelig")


zpoissongam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gam"
    # .self$fn <- zlavaan
    # .self$authors <- "Christine Choirat"
    # .self$description <- "Structural Equation Model"
    .self$year <- 2015
    .self$category <- "discrete"
    .self$wrapper <- "poisson.gam"
  }
)

zpoissongam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)

zpoissongam$methods(
  set = function(z.out) {
    return()
  }
)

##----- QI's need to be defined

zpoissongam$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

zpoissongam$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

