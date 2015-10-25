znormalgam <- setRefClass("Zelig-normal-gam", contains = "Zelig-gam")

znormalgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-gam"
    .self$family <- "gaussian"
    .self$linkinv <- function(x) x
    .self$fn <- mgcv::gam
    .self$description <-
      "Generalized Additive Model for Normal Regression of Continuous Dependent Variables"
    .self$year <- 2011
    .self$category <- "continuous"
    .self$wrapper <- "normal.gam"
  }
)

znormalgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(
      formula = formula, data = data, ...,
      weights = NULL, by = by
    )
  }
)


znormalgam$methods(
  qi = function(simparam, mm) {
    x.out <- data.frame(mm)
    pred.link <- mgcv::predict.gam(simparam$simparam, x.out, se.fit = TRUE, type = "link")
    ev <- rnorm(.self$num, .self$linkinv(pred.link$fit), pred.link$se.fit)
    pred.response <- mgcv::predict.gam(simparam$simparam, x.out, se.fit = TRUE, type = "response")
    pv <- rnorm(.self$num, pred.response$fit, pred.response$se.fit)
    return(list(ev = as.matrix(ev), pv = as.matrix(pv)))
  }
)
