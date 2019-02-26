# October 26, 2018

#' Class \code{SqHingeSurrogate}
#'
#' Squared hinge surrogate for 0/1 loss function
#'
#' @name SqHingeSurrogate-class
#'
#' @keywords internal
#'
#' @include L_Surrogate.R
setClass(Class = "SqHingeSurrogate", contains = "Surrogate")

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{SqHingeSurrogate}
#'
#' @name SqHingeSurrogate-methods
#'
#' @keywords internal
NULL

#' \code{.phiFunc}
#'   calculates squared hinge surrogate loss-function
#'
#' @rdname SqHingeSurrogate-methods
setMethod(f = ".phiFunc",
          signature = c(surrogate = "SqHingeSurrogate"),
          definition = function(surrogate, u) {
              t1 <- {1.0 - u}
              t1t <- t1 > 0.0
              return( {t1^2}*t1t )
            })

#' \code{.dphiFunc}
#'   calculates derivative of squared hinge surrogate loss-function
#'
#' @rdname SqHingeSurrogate-methods
setMethod(f = ".dPhiFunc",
          signature = c(surrogate = "SqHingeSurrogate"),
          definition = function(surrogate, u, du) {
              t1 <- -du * 2.0 * {1.0 - drop(u)}
              t1t <- {1.0 - drop(u)} > 0.0
              return( t1*t1t )
            })
