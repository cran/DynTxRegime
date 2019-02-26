# October 26, 2018

#' Class \code{CVInfokParam}
#'
#' Class \code{CVInfokParam} holds information regarding cross-validation
#'  procedure when only multiple kernel parameters values are considered.
#'
#' @name CVInfokParam-class
#'
#' @slot value Array of values at parameters considered
#'
#' @include N_CVInfo.R N_CVBasic.R N_CVInfoLambda.R
#'
#' @keywords internal
setClass(Class = "CVInfokParam",
         slots = c(value = "array"),
         contains = c("CVInfo"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{CVInfokParam}
#'
#' @name CVInfokParam-methods
#'
#' @keywords internal
NULL

#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "numeric",
                        kernel = "MultiRadialKernel"),
          definition = function(lambdas,
                                kernel,
                                methodObject,
                                cvObject,
                                suppress, ...) {

              nk <- length(x = kernel@kparam)

              valueKparam <- array(data = rep(x = 0.0, times = nk),
                                   dimnames = list(round(x = kernel@kparam,
                                                         digits = 3L)))

              kernels <- methodObject@kernel

              for (j in 1L:nk) {

                if (suppress != 0L) {
                  cat("Cross-validation for kparam =", kernel@kparam[j], "\n")
                }

                # replace kernel stored in methodsObject with new one
                methodObject@kernel <- new("RadialKernel", 
                                           model = kernel@model, 
                                           kparam = kernel@kparam[j])
                methodObject@kernel@X <- kernel@X

                # complete cross-validation step
                res <- .newCVStep(cvObject = cvObject,
                                  methodObject = methodObject,
                                  lambda = lambdas[1L],
                                  suppress = suppress, ...)

                # .newCVStep return NULL if training not successful
                 if (is.null(x = res)) {
                   valueKparam[j] <- NA
                 } else {
                   valueKparam[j] <- res
                 }
              }

              # if no training was successful return NA
              if (all(is.na(x = valueKparam)) ) return( NA )

              # accept the parameter value yielding the largest value as optimal
              ivl <- which.max(x = valueKparam)

              kparam <- kernel@kparam[ivl]

              if (suppress != 0L) {
                cat("Selected parameter: kparam =", kparam, "\n")
              }

              optKernel <- as(object = kernel, Class = "Kernel")
              optKernel@kparam <- kparam
              optKernel <- as(object = optKernel, Class = "RadialKernel")

              result <- new(Class = "CVInfokParam",
                            "value"   = valueKparam,
                            "params"  = list("lambda" = lambdas,
                                             "kparam" = kernel@kparam),
                            "optimal" = list("lambda" = lambdas,
                                             "kernel" = optKernel))

              return( result )
            })
