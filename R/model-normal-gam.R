znormalgam <- setRefClass("Zelig-normal-gam", contains = "Zelig-gam")

#' Compute Predicted Values for \code{normal.gam}
#'
#' @note \code{normal.pv} is not intended to be called directly.
#' @param obj a \code{zelig} object specifying a GAM fit to the x \code{setx}
#'   object
#' @param x a \code{setx} object specifying counterfactuals
#' @param inv the inverse-link function
#' @param num an integer specifying the number of simulations to produce
#' @return the predicted values for the given data and regression
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
normal.pv <- function (obj, x, inv, num) {
  # return NA for bad setx objects
  if (is.null(x))
    return(NA)
  # get actual fitted model object
  obj <- obj$result
  # get appropriate data.frame
  x <- repeat.data(obj, x)
  #
  predictions <- predict.gam(obj, x, se.fit = TRUE, type = "response")
  prfit <- predictions$fit
  prse <- predictions$se.fit
  #
  rnorm(num, prfit, prse)
}

#' Compute Quantities of Interest for the Zelig Model ``normal.gam''
#' @usage \method{qi}{normal.gam}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi normal.gam
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of interest
#'         with their simulations
qi.normal.gam <-
  function(obj, x = NULL, x1 = NULL, y = NULL, num = 1000, param = NULL) {
    inv <- linkinv(param)
    ev1 <- compute.ev(obj, x, inv, num)
    pv1 <- normal.pv(obj, x, inv, num)
    ev2 <- compute.ev(obj, x1, inv, num)
    pv2 <- normal.pv(obj, x1, inv, num)
    list(
      "Expected Value: E(Y|X)" = ev1,
      "Predicted Value: Y|X" = pv1,
      "Expected Value: E(Y|X1)" = ev2,
      "Predicted Value: Y|X1" = pv2,
      "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1
    )
  }

znormalgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "normal-gam"
    .self$family <- "gaussian"
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
