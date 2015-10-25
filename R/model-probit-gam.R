zprobitgam <- setRefClass("Zelig-probit-gam",
                        contains = "Zelig-binchoice-gam")

zprobitgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "probit.gam"
    .self$family <- "binomial"
    .self$linkinv <- binomial("probit")$linkinv
    .self$category <- "discrete"
    .self$wrapper <- "probit.gam"
  }
)

zprobitgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
  }
)
