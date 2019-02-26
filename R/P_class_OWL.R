# October 26, 2018

.validity_OWL <- function(object) {

  # analysis must be Learning or LearningMulti
  if (!is(object = object@analysis, class2 = "Learning") &&
      !is(object = object@analysis, class2 = "LearningMulti") &&
      !is.na(x = object@analysis)) {
    return( "incorrect object for @analysis" )
  }

  return( TRUE )
}

#' Class \code{OWL}
#'
#' Class \code{OWL} contains results for an OWL analysis.
#'
#' @name OWL-class
#' 
#' @slot analysis Contains a Learning or LearningMulti object.
#' @slot analysis@txInfo Feasible tx information.
#' @slot analysis@propen Propensity regression analysis.
#' @slot analysis@outcome Outcome regression analysis.
#' @slot analysis@cvInfo Cross-validation analysis if single regime.
#' @slot analysis@optim Optimization analysis if single regime.
#' @slot analysis@optimResult list of cross-validation and optimization results
#'   if multiple regimes. optimResult[[i]]@cvInfo and optimResult[[i]]@optim.
#' @slot analysis@optimal Estimated optimal Tx and value.
#' @slot analysis@call Unevaluated call to statistical method.
#'
#' @include P_class_.owl.R
#'
#' @template propenOnly
#' @template regression
#' @template optimOnly
#' @template DynTxRegime_methods
setClass(Class = "OWL",
         slot = c("analysis" = "ANY"),
         prototype = list(analysis = NA),
         validity = .validity_OWL)

#' Complete an OWL Analysis
#'
#' @name newOWL
#'
#' @param moPropen modelObj for propensity modeling
#' @param data data.frame of covariates
#' @param response Vector of responses
#' @param txName Tx variable column header in data
#' @param lambdas Tuning parameter(s)
#' @param cvFolds Number of cross-validation folds
#' @param kernel Kernel object or SubsetList
#' @param fSet NULL or function defining subset rules
#' @param txVec Tx vector recast as +/- 1
#' @param surrogate Surrogate object
#' @param suppress T/F indicating if prints to screen are executed
#' @param guess optional numeric vector providing starting values for
#'   optimization methods
#' @param ...  Additional inputs for optimization
#'
#' @return An OWL object
#'
#' @keywords internal
.newOWL <- function(moPropen,
                    data,
                    response,
                    txName,
                    lambdas,
                    cvFolds,
                    kernel,
                    fSet, 
                    surrogate,
                    suppress,
                    guess, ...) {

  if (suppress != 0L) cat("Outcome Weighted Learning\n")

  analysis <- .newLearning(moPropen = moPropen,
                           moMain = NULL,
                           moCont = NULL,
                           data = data,
                           response = response,
                           txName = txName,
                           lambdas = lambdas,
                           cvFolds = cvFolds,
                           kernel = kernel,
                           fSet = fSet,
                           iter = NULL,
                           surrogate = surrogate,
                           suppress = suppress,
                           guess = guess,
                           createObj = '.createowl', 
                           prodPi = rep(x = 1.0, times = nrow(x = data)),
                           index = rep(x = TRUE, times = nrow(x = data)), ...)

  return( new("OWL", "analysis" = analysis) )
}

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OWL}
#'
#' @name OWL-methods
#'
#' @keywords internal
NULL

#' @rdname DynTxRegime-internal-api
setMethod(f = "Call",
          signature = c(name = "OWL"),
          definition = function(name, ...) {
              return( Call(name = name@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "coef",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( coef(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "cvInfo",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( cvInfo(object = object@analysis, ...)$cvInfo )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep",
          signature = c(object = "OWL"),
          definition = function(object) { cat("Outcome Weighted Learning\n") })

#' @rdname DynTxRegime-internal-api
setMethod(f = "estimator",
          signature = c(x = "OWL"),
          definition = function(x, ...) {
              return( estimator(x = x@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "fitObject",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( fitObject(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optimObj",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( optimObj(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "OWL",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, newdata = newdata, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "OWL",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "OWL"),
          definition = function(object, ...) { return( NA) })

#' @rdname DynTxRegime-internal-api
setMethod(f = "plot",
          signature = c(x = "OWL", y = "missing"),
          definition = function(x, y, suppress = FALSE, ...) {
              plot(x = x@analysis@propen, suppress = suppress, ...)
            })

#' @rdname OWL-methods
setMethod(f = "print",
          signature = c(x = "OWL"),
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = x@analysis, ...)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "propen",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( propen(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "regimeCoef",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@analysis, ...) )
            })

#' @rdname OWL-methods
setMethod(f = "show",
          signature = c(object = "OWL"),
          definition = function(object) {
              DTRstep(object = object)
              show(object = object@analysis)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "summary",
          signature = c(object = "OWL"),
          definition = function(object, ...) {
              return( summary(object = object@analysis, ...) )
            })
