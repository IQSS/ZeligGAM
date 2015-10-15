zlogitgam <- setRefClass("Zelig-logit-gam",
                        contains = "Zelig")


zlogitgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit.gam"
    # .self$fn <- zlavaan
    # .self$authors <- "Christine Choirat"
    # .self$description <- "Structural Equation Model"
    # .self$year <- 2015
    .self$category <- "continuous"
    .self$wrapper <- "logit.gam"
  }
)

zlogitgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)

zlogitgam$methods(
  set = function(z.out) {
    return()
  }
)

##----- QI's need to be defined

zlogitgam$methods(
  param = function(z.out) {
    return(mvrnorm(.self$num, coef(z.out), vcov(z.out)))
  }
)

zlogitgam$methods(
  qi = function(simparam, mm) {
    ev <- simparam %*% t(mm)
    pv <- ev
    return(list(ev = ev, pv = pv))
  }
)

