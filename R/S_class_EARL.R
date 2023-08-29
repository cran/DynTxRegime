# October 26, 2018

.validity_EARL <- function(object) {

  # @analysis must be Learning or LearningMulti
  if (!is(object = object@analysis, class2 = "Learning") &&
      !is(object = object@analysis, class2 = "LearningMulti") &&
      !is.na(x = object@analysis)) {
    return( "incorrect object for @analysis" )
  }

  return( TRUE )
}

#' Class \code{EARL}
#'
#' Class \code{EARL} contains results for an EARL analysis.
#'
#' @name EARL-class
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
#' @template outcomeOnly
#' @template propenOnly
#' @template regression
#' @template optimOnly
#' @template DynTxRegime_methods
#' @include S_class_.earl.R
#'
setClass(Class = "EARL",
         slot = c("analysis" = "ANY"),
         prototype = list(analysis = NA),
         validity = .validity_EARL)

#' Complete an EARL Analysis
#'
#' @rdname newEARL
#'
#' @param moPropen modelObj for propensity modeling
#' @param moMain modelObj for main effects of outcome model
#' @param moCont modelObj for contrasts of outcome model
#' @param data data.frame of covariates
#' @param response Vector of responses
#' @param txName Tx variable column header in data
#' @param lambdas Tuning parameter(s)
#' @param cvFolds Number of cross-validation folds
#' @param kernel Kernel object or SubsetList
#' @param fSet NULL or function defining subset rules
#' @param surrogate Surrogate object
#' @param iter Maximum iterations for outcome regression
#' @param suppress T/F indicating if prints to screen are executed
#' @param guess optional numeric vector providing starting values for
#'   optimization methods
#' @param ...  Additional inputs for optimization
#'
#' @return An EARL object
#'
#' @keywords internal
.newEARL <- function(moPropen,
                     moMain,
                     moCont,
                     data,
                     response,
                     txName,
                     lambdas,
                     cvFolds,
                     surrogate,
                     iter,
                     guess,
                     kernel,
                     fSet,
                     suppress, ...) {

  if (suppress != 0L) {
    cat("Efficient Augmentation and Relaxation Learning\n")
  }

  analysis <- .newLearning(moPropen = moPropen,
                           moMain = moMain,
                           moCont = moCont,
                           data = data,
                           response = response,
                           txName = txName,
                           lambdas = lambdas,
                           cvFolds = cvFolds,
                           kernel = kernel,
                           fSet = fSet,
                           iter = iter,
                           surrogate = surrogate,
                           suppress = suppress,
                           guess = guess,
                           createObj = '.createearl', 
                           prodPi = rep(x = 1.0, times = nrow(x = data)),
                           index = rep(x = TRUE, times = nrow(x = data)), ...)

  return( new("EARL", "analysis" = analysis) )
}

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{EARL}
#'
#' @name EARL-methods
#'
#' @keywords internal
NULL

#' @rdname DynTxRegime-internal-api
setMethod(f = "Call",
          signature = c(name = "EARL"),
          definition = function(name, ...) {
              return( Call(name = name@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "coef",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( coef(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "cvInfo",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( cvInfo(object = object@analysis, ...)$cvInfo )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep",
          signature = c(object = "EARL"),
          definition = function(object) {
              cat("Efficient Augmentation and Relaxation Learning\n")
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "estimator",
          signature = c(x = "EARL"),
          definition = function(x, ...) {
              return( estimator(x = x@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "fitObject",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( fitObject(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optimObj",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( optimObj(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "EARL",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, newdata = newdata, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "EARL",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "EARL"),
          definition = function(object, ...) { 
              outcome(object = object@analysis, ...)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "plot",
          signature = c(x = "EARL", y = "missing"),
          definition = function(x, y, suppress = FALSE, ...) {
              plot(x = x@analysis, suppress = suppress, ...)
            })

#' @rdname EARL-methods
setMethod(f = "print",
          signature = c(x = "EARL"),
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = x@analysis, ...)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "propen",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( propen(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "regimeCoef",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@analysis, ...) )
            })

#' @rdname EARL-methods
setMethod(f = "show",
          signature = c(object = "EARL"),
          definition = function(object) {
              DTRstep(object = object)
              show(object = object@analysis)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "summary",
          signature = c(object = "EARL"),
          definition = function(object, ...) {
              return( summary(object = object@analysis, ...) )
            })
