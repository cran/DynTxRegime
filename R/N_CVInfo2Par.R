# October 26, 2018

#' Class \code{CVInfo2Par}
#'
#' Class \code{CVInfo2Par} holds information regarding cross-validation
#'  procedure when multiple kernel parameters and tuning parameters are 
#'  considered.
#'
#' @name CVInfo2Par-class
#'
#' @slot value Matrix of values at parameters considered
#'
#' @include N_CVInfo.R N_CVBasic.R N_CVInfoLambda.R N_CVInfokParam.R
#'
#' @keywords internal
setClass(Class = "CVInfo2Par",
         slots = c(value = "matrix"),
         contains = c("CVInfo"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{CVInfo2Par}
#'
#' @name CVInfo2Par-methods
#'
#' @keywords internal
NULL

#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "array",
                        kernel = "MultiRadialKernel"),
          definition = function(lambdas,
                                kernel,
                                methodObject,
                                cvObject,
                                suppress, ...) {

              nl <- length(x = lambdas)
              nk <- length(x = kernel@kparam)

              valuePairs <- matrix(data = 0.0,
                                   nrow = nk,
                                   ncol = nl,
                                   dimnames = list(round(x = kernel@kparam, 
                                                         digits = 3L),
                                                   round(x = lambdas,  
                                                         digits = 3L)))

              for (j in 1L:nk) {

                # replace kernel stored in methodsObject with new one
                methodObject@kernel <- new("RadialKernel", 
                                           model = kernel@model, 
                                           kparam = kernel@kparam[j])
                methodObject@kernel@X <- kernel@X

                for (k in 1L:nl) {

                  if (suppress != 0L) {
                    cat("Cross-validation for kparam =", kernel@kparam[j],
                        "lambda =", lambdas[k], "\n")
                  }

                  # complete cross-validation step
                  res <- .newCVStep(cvObject = cvObject,
                                    methodObject = methodObject,
                                    lambda = lambdas[k],
                                    suppress = suppress, ...)

                  # .newCVStep return NULL if training not successful
                  if (is.null(x = res)) {
                    valuePairs[j,k] <- NA
                  } else {
                    valuePairs[j,k] <- res
                  }
                }
              }

              # if no training was successful return NA
              if (all(is.na(x = valuePairs)) ) return( NA )

              # identify tuning parameter pair that leads to the largest
              # average value; if more than one pair leads to the
              # largest average value, the pair with the smallest lambda and
              # the largest kernel parameter is selected.
              bestKparam <- apply(X = valuePairs,
                                  MARGIN = 2L,
                                  FUN = function(x) {
                                      res <- length(x = x) - which.max(x = rev(x = x)) + 1L
                                      return( res )
                                    })

              ivl <- which.max(x = apply(X = valuePairs,
                                         MARGIN = 2L,
                                         FUN = max,
                                         na.rm = TRUE))

              lambda <- lambdas[ivl]

              kparam <- kernel@kparam[bestKparam[ivl]]

              if (suppress != 0L) {
                cat("Selected parameters\n")
                cat("lambda =", lambda, "kparam =", kparam, "\n")
              }

              optKernel <- as(object = kernel, Class = "Kernel")
              optKernel@kparam <- kparam
              optKernel <- as(object = optKernel, Class = "RadialKernel")

              result <- new(Class = "CVInfo2Par",
                            "value"   = valuePairs,
                            "params"  = list("lambda" = lambdas,
                                             "kparam" = kernel@kparam),
                            "optimal" = list("lambda" = lambda,
                                             "kernel" = optKernel))

              return( result )
            })
