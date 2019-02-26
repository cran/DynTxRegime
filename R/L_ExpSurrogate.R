# October 26, 2018

#' Class \code{ExpSurrogate}
#'
#' Exponential surrogate for 0/1 loss.
#'
#' @name ExpSurrogate-class
#'
#' @keywords internal
#'
#' @include L_Surrogate.R
setClass(Class = "ExpSurrogate", contains = "Surrogate")

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{ExpSurrogate}
#'
#' @name ExpSurrogate-methods
#'
#' @keywords internal
NULL

#' \code{.phiFunc}
#'   calculates exponential surrogate loss-function
#'
#' @rdname ExpSurrogate-methods
setMethod(f = ".phiFunc",
          signature = c(surrogate = "ExpSurrogate"),
          definition = function(surrogate, u) { return( exp(x = -u) ) })

#' \code{.dphiFunc}
#'   calculates derivative of exponential surrogate loss-function
#'
#' @rdname ExpSurrogate-methods
setMethod(f = ".dPhiFunc",
          signature = c(surrogate = "ExpSurrogate"),
          definition = function(surrogate, u, du) {
              expu <- exp(x = -u)
              return( -expu * du )
            })
