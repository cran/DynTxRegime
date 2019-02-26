# October 26, 2018

#' Class \code{SmoothRampSurrogate}
#'
#' Components of smoothed ramp surrogate for 0/1 loss function.
#'
#' @name SmoothRampSurrogate-class
#'
#' @keywords internal
#'
#' @include L_Surrogate.R
setClass(Class = "SmoothRampSurrogate", contains = "Surrogate")

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{SmoothRampSurrogate}
#'
#' @name SmoothRampSurrogate-methods
#'
#' @keywords internal
NULL

#' \code{.phiFunc}
#'   calculates smoothed ramp surrogate loss-function
#'
#' @rdname SmoothRampSurrogate-methods
setMethod(f = ".phiFunc",
          signature = c(surrogate = "SmoothRampSurrogate"),
          definition = function(surrogate, u, res) {
              lims <- res > 0.0

              phi1 <- {1.0 - u} * {1.0 - u} * {{0.0 <= u} & {u < 1.0}} +
                      {1.0 - 2.0 * u} * {u < 0.0}

              phi0 <- u * u * {{-1.0 <= u} & {u < 0.0}} -
                      {2.0 * u + 1.0} * {u < -1.0}

              result <- phi1 * lims + phi0 * {1.0 - lims}

              return( result )
            })

#' \code{.dphiFunc}
#'   calculates derivative of smoothed ramp surrogate loss-function
#'
#' @rdname SmoothRampSurrogate-methods
setMethod(f = ".dPhiFunc",
          signature = c(surrogate = "SmoothRampSurrogate"),
          definition = function(surrogate, u, du, res) {
              lims <- res > 0.0

              phi1 <- -2.0 * {{1.0 - u} * {{0.0 <= u} & {u < 1.0}} + {u < 0.0}}

              phi0 <- 2.0 * {u * {{-1.0 <= u} & {u < 0.0}} - {u < -1.0}}

              result <- {phi1 * lims + phi0 * {1.0 - lims}}*du

              return( result )
            })
