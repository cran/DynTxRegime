# October 26, 2018

#' Class \code{Surrogate}
#'
#' General class for surrogate objects.
#'
#' @name Surrogate-class
#'
#' @keywords internal
#'
#' @slot we included to avoid VIRTUAL designation
setClass(Class = "Surrogate", slots = c("we" = "ANY"))

##########
## GENERICS
##########

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".dPhiFunc",
           def = function(surrogate, ...) { standardGeneric(f = ".dPhiFunc") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".optim",
           def = function(surrogate, ...) { standardGeneric(f = ".optim") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".phiFunc",
           def = function(surrogate, ...) { standardGeneric(f = ".phiFunc") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{Surrogate}
#'
#' @name Surrogate-methods
#'
#' @keywords internal
NULL

#' \code{optim}
#'  optimize objective function
#'
#' Utilizes stats::optim to obtain parameter estimates. Requires that
#' the objective function and its derivative are defined by the
#' calling learning method. Returns NULL if optimization is not successful 
#' due to problems;
#' a vector of the current parameter estimates if optimization is not
#' successful because it hit the maximum number if iterations; and
#' the list object returned by stats::optim if optimization is successful
#'
#' @rdname Surrogate-methods
#' @importFrom stats optim
setMethod(f = ".optim",
          signature = c(surrogate = "Surrogate"),
          definition = function(surrogate,
                                par,
                                lambda,
                                fn,
                                gr,
                                suppress, ...) {

              # determine if additional arguments to stats::optim were
              # provided by the user
              argList <- list(...)

              # modify the print control based on suppress if not provided
              # through elipsis
              if ("control" %in% names(x = argList)) {
                if (!{"trace" %in% names(x = argList[[ "control" ]])}) {
                  argList[[ "control" ]][[ "trace" ]] <- !suppress
                }
              } else {
                argList[[ "control" ]] <- list("trace" = !suppress)
              }

              # set additional inputs for stats::optim
              argList[[ "fn" ]] <- fn
              argList[[ "gr" ]] <- gr
              argList[[ "method" ]] <- "BFGS"

              argList[[ "par" ]] <- par
              argList[[ "lambda" ]] <- lambda

              # call stats::optim
              test <- do.call(what = stats::optim, args = argList)

              # if stats::optim did not converge due to maximum iterations
              # return current parameter estimates
              if (test$convergence == 1L) {
                return( test$par )
              }

              # if stats::optim did not converge for other reasonse, return
              # NULL
              if (test$convergence != 0L) {
                cat("stats::optim() did not converge", test$convergence, "\n")
                return( NULL )
              }

              # if stats::optim converged, return list object returned by
              # stats::optim
              return( test )
            })
