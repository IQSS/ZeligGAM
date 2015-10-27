zlogitgam <- setRefClass("Zelig-logit-gam",
                        contains = "Zelig-binchoice-gam")

zlogitgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "logit.gam"
    .self$family <- "binomial"
    .self$linkinv <- binomial("logit")$linkinv
    .self$category <- "discrete"
    .self$wrapper <- "logit.gam"
  }
)

zlogitgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    # .self$model.call$family <- .self$family
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)
