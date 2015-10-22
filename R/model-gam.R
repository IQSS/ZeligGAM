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
    .self$object <- .self$zelig.out$z.out[[1]]
  }
)

zgam$methods(
  set = function(...) {
    "Setting Explanatory Variable Values"
    s <-list(...)
    f <- update(.self$formula, 1 ~ .)
    # update <- na.omit(.self$data) %>% # remove missing values
    update <- .self$data %>%
      group_by_(.self$by) %>%
      do(mm = model.matrix.gam(.self$object, reduce.gam(dataset = ., s, 
                                                        formula = .self$formula,
                                                        data = .self$data)))
    return(update)
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

