# October 26, 2018

#' Class \code{HingeSurrogate}
#'
#' Hinge surrogate for 0/1 loss function.
#'
#' @name HingeSurrogate-class
#'
#' @keywords internal
#'
#' @include L_Surrogate.R
setClass(Class = "HingeSurrogate", contains = "Surrogate")

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{HingeSurrogate}
#'
#' @name HingeSurrogate-methods
#'
#' @keywords internal
NULL

#' \code{.phiFunc}
#'   calculates hinge surrogate loss-function
#'
#' @rdname HingeSurrogate-methods
setMethod(f = ".phiFunc",
          signature = c(surrogate = "HingeSurrogate"),
          definition = function(surrogate, u) {
              t1 <- {1.0 - u}
              t1t <- t1 > 0.0
              return( t1*t1t )
            })

#' \code{.dphiFunc}
#'   calculates derivative of hinge surrogate loss-function. Returns NA.
#'
#' @rdname HingeSurrogate-methods
setMethod(f = ".dPhiFunc",
          signature = c(surrogate = "HingeSurrogate"),
          definition = function(surrogate, u, du) { return( NA ) })

#' \code{.optim}
#'   optimize objective function.
#'
#' Utilizes dfoptim::hjk to obtain parameter estimates. Requires that
#' the objective function be defined by the calling learning method.
#' Returns NULL if optimization is not successful due to problems or
#' the list object returned by dfoptim::hjk if optimization is successful.
#'
#' @rdname HingeSurrogate-methods
#' @importFrom dfoptim hjk
setMethod(f = ".optim",
          signature = c(surrogate = "HingeSurrogate"),
          definition = function(surrogate,
                                par,
                                lambda,
                                fn,
                                gr,
                                suppress, ...) {

              # determine if additional arguments to dfoptim::hjk were
              # provided by the user
              argList <- list(...)

              # modify the print control based on suppress if not provided
              # through elipsis
              if ("control" %in% names(x = argList)) {
                if (!{"info" %in% names(x = argList[[ "control" ]])}) {
                  argList[[ "control" ]][[ "info" ]] <- !suppress
                }
              } else {
                argList[[ "control" ]] <- list("info" = !suppress)
              }

              # set additional inputs for dfoptim::hjk
              argList[[ "fn" ]] <- fn
              argList[[ "par" ]] <- par
              argList[[ "lambda" ]] <- lambda

              # call dfoptim::hjk
              test <- do.call(what = dfoptim::hjk, args = argList)

              # if dfoptim::hjk did not converge for other reasons, return
              # NULL
              if (test$convergence > 0.5) {
                cat("hjk() did not converge", test$convergence, "\n")
                return( NULL )
              }

              # if dfoptim::hjk converged, return list object returned by
              # dfoptim::hjk
              return( test )
            })
