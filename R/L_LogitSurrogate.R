# October 26, 2018

#' Class \code{LogitSurrogate}
#'
#' Logistic surrogate for 0/1 loss function.
#'
#' @name LogitSurrogate-class
#'
#' @keywords internal
#'
#' @include L_Surrogate.R
setClass(Class = "LogitSurrogate", contains = "Surrogate")

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{LogitSurrogate}
#'
#' @name LogitSurrogate-methods
#'
#' @keywords internal
NULL

#' \code{.phiFunc}
#'   calculates logistic surrogate loss-function
#'
#' @rdname LogitSurrogate-methods
setMethod(f = ".phiFunc",
          signature = c(surrogate = "LogitSurrogate"),
          definition = function(surrogate, u) { 
              return( log(x = 1.0 + exp(x = -u)) ) 
            })

#' \code{.dphiFunc}
#'   calculates derivative of logistic surrogate loss-function
#'
#' @rdname LogitSurrogate-methods
setMethod(f = ".dPhiFunc",
          signature = c(surrogate = "LogitSurrogate"),
          definition = function(surrogate, u, du) {
              expu <- exp(x = -u)
              return( - {expu / {1.0 + expu}} * du )
            })
