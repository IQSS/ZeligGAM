zgam <- setRefClass("Zelig-gam",
                    contains = "Zelig",
                    fields = list(family = "character",
                                  link = "character",
                                  linkinv = "function",
                                  object = "ANY"))

zgam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "gam"
    .self$authors <- "Skyler J. Cranmer"
    .self$description <- "Generalized Additive Model Class"
    .self$year <- 2011
    .self$category <- "continuous"
    .self$acceptweights <- TRUE
  }
)

zgam$methods(
  zelig = function(formula, data, ..., weights = NULL, by = NULL) {
    .self$zelig.call <- match.call(expand.dots = TRUE)
    .self$model.call <- .self$zelig.call
    .self$model.call$family <- .self$family
    callSuper(formula = formula, data = data, ...,
              weights = NULL, by = by)
    .self$object <- .self$zelig.out$z.out[[1]]
  }
)

zgam$methods(
  set = function(...) {
    "Setting Explanatory Variable Values"
    s <- list(...)
    f1 <- mgcv::interpret.gam(formula(.self$object))$fake.formula
    f2 <- update(f1, 1 ~ .)
    # update <- na.omit(.self$data) %>% # remove missing values
    update <- .self$data %>%
      group_by_(.self$by) %>%
      do(mm = reduce(dataset = ., s, 
                     formula = f1,
                     data = .self$data))
#       do(mm = model.matrix(f2, reduce(dataset = ., s, 
#                                      formula = f1,
#                                      data = .self$data)))
    return(update)
  }
)

zgam$methods(
  param = function(z.out, x.out) {
    return(list(simparam = z.out))
  }
)
