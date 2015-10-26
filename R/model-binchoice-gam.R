zbinchoicegam <- setRefClass("Zelig-binchoice-gam", contains = "Zelig-gam")

zbinchoicegam$methods(
  initialize = function() {
    callSuper()
    .self$name <- "binchoice-gam"
    .self$fn <- mgcv::gam
    .self$year <- 2011
    .self$category <- "discrete"
  }
)

zbinchoicegam$methods(
  qi = function(simparam, mm) {
    x.out <- data.frame(mm)
    pred.link <- mgcv::predict.gam(simparam$simparam, x.out, se.fit = TRUE, type = "link")
    ev <- rnorm(.self$num, .self$linkinv(pred.link$fit), pred.link$se.fit)
    pred.response <- mgcv::predict.gam(simparam$simparam, x.out, se.fit = TRUE, type = "response")
    pv <- rbinom(.self$num, 1, pred.response$fit)
    levels(pv) <- min(pv):max(pv)
    return(list(ev = as.matrix(ev), pv = as.matrix(pv)))
  }
)
