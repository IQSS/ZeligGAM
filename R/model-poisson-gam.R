zpoissongam <- setRefClass("Zelig-poisson-gam", contains = "Zelig-gam")

zpoissongam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "poisson-gam"
    .self$family <- "poisson"
    .self$linkinv <- poisson("log")$linkinv
    .self$fn <- mgcv::gam
    .self$description <-
      "Generalized Additive Model for Poisson Regression of Discrete Dependent Variables"
    .self$year <- 2011
    .self$category <- "discrete"
    .self$wrapper <- "poisson.gam"
  }
)

zpoissongam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(
      formula = formula, data = data, ...,
      weights = NULL, by = by
    )
  }
)

zpoissongam$methods(
  qi = function(simparam, mm) {
    x.out <- data.frame(mm)
    pred.link <- mgcv::predict.gam(simparam$simparam, x.out, se.fit = TRUE, type = "link")
    ev <- rnorm(.self$num, .self$linkinv(pred.link$fit), pred.link$se.fit)
    pred.response <- mgcv::predict.gam(simparam$simparam, x.out, se.fit = TRUE, type = "response")
    pv <- rpois(.self$num, pred.response$fit)
    levels(pv) <- min(pv):max(pv)
    return(list(ev = as.matrix(ev), pv = pv))
  }
)
