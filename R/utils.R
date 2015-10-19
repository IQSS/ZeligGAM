#' Add Repittions of \code{data.frame} from GAM-style Formula
#'
#' \code{repeat.data} copies data-columns relative to the number of degrees of
#' freedom in the statistical regression. This allows appropriate simulations
#' of quantities of interest.
#' @note This function is not intended to be computed directly.
#' @param obj a \code{zelig} object specifying a statistical regression
#' @param x a \code{setx} object specifying information about counterfactuals
#' @return a correctly formatted data.frame
#' @author Matt Owen

# object <- z.out
# data <- data.frame(z5$originaldata)

repeat.data <- function (obj, x) {
  if (is.null(x))
    return(NULL)
  
  object <- obj
  data <- x$updated
  pterms <- attr(object$pterms, 'term.labels')
  sterms <- list()
  df.vec <- vector()
  
  # get relevant data from smooth objects
  for (k in 1:length(object$smooth)) {
    sterms[[k]] <- object$smooth[[k]]$term
    df.vec[[k]] <- object$smooth[[k]]$df
  }
  
  # generate new data.frame
  if (attr(object$terms, "intercept") > 0) {
    newx <- data.frame(data[1])
    names(newx) <- names(data)[1]
  }
  else
    # throw error if no intercept column exists
    stop("Cannot simulate quantities of interest without an intercept")
  
  # ignore intercept column. This is ported from original Zelig
  if (ncol(data) > 1) {
    for (k in 2:ncol(data)) {
      name <- names(data)[[k]]
      
      # untested
      if (name %in% pterms) {
        temp <- as.data.frame(data[[k]])
        names(temp) <- name
        newx <- cbind(newx, temp)
      }
      
      # repeat entries in s-term at least as many times as there are degrees of
      # freedom for that term
      if (name %in% sterms) {
        for (j in 1:length(object$smooth)) {
          if (name == object$smooth[[j]]$term)
            repnum <- df.vec[[j]]
        }
        
        # create temporary data.frame with only 1 column
        temp <- as.data.frame(t(rep(data[[k]], repnum)))
        names(temp) <- rep(name, repnum)
        
        # bind it
        newx <- cbind(newx, temp)
      }
    }
  }
  
  # return
  newx
}

# This file documents all expected-value functions for gam.zelig package
#' Simulate Expected Values for GAM Models
#' @note \code{compute.ev} is not intended to be called directly.
#' @param obj a \code{zelig} object specifying a GAM fit to the x \code{setx}
#'   object
#' @param x a \code{setx} object specifying counterfactuals
#' @param inv the inverse-link function
#' @param num an integer specifying the number of simulations to produce
#' @return the predicted values for the given data and regression
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
compute.ev <- function (obj, x, inv, num) {
  # return NA for bad setx objects
  if (is.null(x))
    return(NA)
  
  # get actual fitted model object
  obj <- GetObject(obj)
  
  # get appropriate data.frame
  x <- repeat.data(obj, x)
  
  #
  predictions <- predict(obj, x, se.fit = TRUE, type = "link")
  evfit <- predictions$fit
  evse <- predictions$se.fit
  
  rnorm(num, mean = inv(evfit), sd = evse)
}
