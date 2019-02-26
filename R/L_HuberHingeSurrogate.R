# October 26, 2018

#' Class \code{HuberHingeSurrogate}
#'
#' Huberized hinge surrogate for 0/1 loss function.
#'
#' @name HuberHingeSurrogate-class
#'
#' @keywords internal
#'
#' @include L_Surrogate.R
setClass(Class = "HuberHingeSurrogate", contains = "Surrogate")

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{HuberHingeSurrogate}
#'
#' @name HuberHingeSurrogate-methods
#'
#' @keywords internal
NULL

#' \code{.phiFunc}
#'   calculates huberized hinge surrogate loss-function
#'
#' @rdname HuberHingeSurrogate-methods
setMethod(f = ".phiFunc",
          signature = c(surrogate = "HuberHingeSurrogate"),
          definition = function(surrogate, u){
              res <- 0.25*(1-u)^2*{{u >= -1}&{u<1}} - u*{u < {-1}}
              return( res )
            })

#' \code{.dphiFunc}
#'   calculates derivative of huberized hinge surrogate loss-function
#'
#' @rdname HuberHingeSurrogate-methods
setMethod(f = ".dPhiFunc",
          signature = c(surrogate = "HuberHingeSurrogate"),
          definition = function(surrogate, u, du){
              res <- {-0.5*(1-u)*{{u >= -1}&{u<1}} - {u < {-1}}}*du
            })
