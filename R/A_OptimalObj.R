# October 23, 2018

.validity_OptimalObj <- function(object) {

  # optimal must be OptimalInfo or DecisionPointList of OptimalInfo
  if (!is(object = object@optimal, class2 = "OptimalInfo") &&
      !is(object = object@optimal, class2 = "DecisionPointList") &&
      !is.na(x = object@optimal)) {
    return( "incorrect object for @optimal" )
  }

  if (is(object = object@optimal, class2 = "DecisionPointList")) {
    for (i in 1L:length(x = object@optimal)) {
      if (!is(object = object@optimal[[ i ]], class2 = "OptimalInfo")) {
        return( "incorrect object for @optimal" )
      }
    }
  }

  return( TRUE )
}

#' Class \code{OptimalObj}
#'
#' Class \code{OptimalObj} stores the estimated optimal Tx, decision functions
#' and estimated value under a common name.
#'
#' @name OptimalObj-class
#' @docType class
#'
#' @slot optimal ANY - must be \code{OptimalInfo} or 
#'   \code{DecisionPointList} of \code{OptimalInfo}
#'
#' @include A_OptimalInfo.R A_DecisionPointList.R
#'
#' @keywords internal
setClass(Class = "OptimalObj",
         slots = c(optimal = "ANY"),
         prototype = list(optimal = NA),
         validity = .validity_OptimalObj)

##########
# METHODS
##########

#' Methods Available for Objects of Class \code{OptimalObj}
#'
#' @name OptimalObj-methods
#'
#' @keywords internal
NULL

#' \code{estimator(x)} 
#'   retrieves the estimated value obtained by a statistical method.
#'   Method called determined by class of @optimal.
#
#' @rdname OptimalObj-methods
setMethod(f = "estimator",
          signature = c(x = "OptimalObj"),
          definition = function(x) { 
              if (is(object = x@optimal, class2 = "OptimalInfo")) {
                return( estimator(x = x@optimal) )
              } else if (is(object = x@optimal, class2 = "DecisionPointList")) {
                nDP <- length(x = x@optimal)
                return( estimator(x = x@optimal[[ nDP ]]) )
              } else {
                stop("not allowed")
              }
            })

#' \code{optTx(x)} 
#'   returns the estimated decision function and/or optimal tx
#'   Method called determined by class of @optimal.
#'
#' @rdname OptimalObj-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalObj",
                        newdata = "missing"),
          definition = function(x, newdata, ...) { 
              if (is(object = x@optimal, class2 = "OptimalInfo")) {
                return( optTx(x = x@optimal) )
              } else if (is(object = x@optimal, class2 = "DecisionPointList")) {
                return( .cycleList(object = x@optimal, 
                                   func = 'optTx',  
                                   trm = 'x', ...) )
              } else {
                stop("not allowed")
              }
            })

#' \code{optTx(x, newdata)} 
#'   returns an error
#'
#' @rdname OptimalObj-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalObj",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) { stop("not allowed") })

#' \code{print(x)}
#'   Prints summary information regarding recommended tx and the estimated
#'   value. Method called determined by class of @optimal.
#
#' @rdname OptimalObj-methods 
setMethod(f = "print",
          signature = c(x = "OptimalObj"),
          definition = function(x, ...) { print(x = x@optimal) })

#' \code{show(object)}
#'   Displays summary information regarding recommended tx and the estimated
#'   value. Method called determined by class of @optimal.
#
#' @rdname OptimalObj-methods 
setMethod(f = "show",
          signature = c(object = "OptimalObj"),
          definition = function(object) { show(object = object@optimal) })

#' \code{summary(object)}
#'   Returns a summary of estimated decision functions and/or optimal tx. 
#'   Method called determined by class of @optimal.
#
#' @rdname OptimalObj-methods 
setMethod(f = "summary",
          signature = c(object = "OptimalObj"),
          definition = function(object, ...) { 
              if (is(object = object@optimal, class2 = "OptimalInfo")) {
                return( summary(object = object@optimal, ...) )
              } else if (is(object = object@optimal, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@optimal, 
                                   func = 'summary', ...) )
              } else {
                stop("not allowed")
              }
            })
