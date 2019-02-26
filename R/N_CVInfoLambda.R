# October 26, 2018

#' Class \code{CVInfoLambda}
#'
#' Class \code{CVInfoLambda} holds information regarding cross-validation
#'   procedure when only multiple lambda values are considered.
#'
#' @name CVInfoLambda-class
#'
#' @slot value Array of values at tuning parameters considered
#'
#' @include N_CVInfo.R N_CVBasic.R
#'
#' @keywords internal
setClass(Class = "CVInfoLambda",
         slots = c(value = "array"),
         contains = c("CVInfo"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{CVInfoLambda}
#'
#' @name CVInfoLambda-methods
#'
#' @keywords internal
NULL

#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "numeric",
                        kernel = "Kernel"),
          definition = function(lambdas,
                                kernel,
                                methodObject,
                                cvObject,
                                suppress, ...) {

              nl <- length(x = lambdas)
              if (nl > 1L) stop("not allowed")

              valueLambda <- array(data = rep(x = NA, times = nl),
                                   dimnames = list(round(x = lambdas,
                                                         digits = 3L)))

              result <- new(Class = "CVInfoLambda",
                            "value"   = valueLambda,
                            "params"  = list("lambda" = lambdas,
                                             "kparam" = kernel@kparam),
                            "optimal" = list("lambda" = lambdas,
                                             "kernel" = kernel))

              return( result )
            })

#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "numeric",
                        kernel = "MultiRadialKernel"),
          definition = function(lambdas, kernel, ...) { stop("not allowed") })

#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "array",
                        kernel = "Kernel"),
          definition = function(lambdas,
                                kernel,
                                methodObject,
                                cvObject,
                                suppress, ...) {

              nl <- length(x = lambdas)

              valueLambda <- array(data = rep(x = 0.0, times = nl),
                                   dimnames = list(round(x = lambdas,
                                                         digits = 3L)))

              for (k in 1L:nl) {

                if (suppress != 0L && !is.null(x = cvObject@folds)) {
                  cat("Cross-validation for lambda =", lambdas[k], "\n")
                }

                res <- .newCVStep(cvObject = cvObject,
                                  methodObject = methodObject,
                                  lambda = lambdas[k],
                                  suppress = suppress, ...)

                # .newCVStep return NULL if training not successful
                if (is.null(x = res)) {
                  valueLambda[k] <- NA
                } else {
                  valueLambda[k] <- res
                }
              }

              # if no training was successful return NA
              if (all(is.na(x = valueLambda)) ) return( NA)

              # accept the lambda value yielding the largest value as optimal
              ivl <- which.max(x = valueLambda)

              lambda <- lambdas[ivl]

              if (suppress != 0L) {
                cat("Selected parameter: lambda =", lambda, "\n")
              }

              result <- new(Class = "CVInfoLambda",
                            "value"   = valueLambda,
                            "params"  = list("lambda" = lambdas,
                                             "kparam" = kernel@kparam),
                            "optimal" = list("lambda" = lambda,
                                             "kernel" = kernel))

              return( result )
            })

#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "array",
                        kernel = "MultiRadialKernel"),
          definition = function(lambdas, kernel, ...) { stop("not allowed") })
