# October 26, 2018

#' Class \code{BOWLObj}
#'
#' Class \code{BOWLObj} contains product and sum information required for 
#'   iteration
#'
#' @name BOWLObj-class
#'
#' @slot prodPi Vector of the products of the propensity for the tx received 
#' @slot sumR Vector of the sum of the rewards
#' @slot index Vector indicating compliance with estimated optimal regime
#'
#' @keywords internal
setClass(Class = "BOWLObj",
         slots = c(prodPi = "vector",
                   sumR   = "vector",
                   index  = "vector"))

.validity_BOWL <- function(object) {

  # analysis must be Learning or LearningMulti
  if (!is(object = object@analysis, class2 = "Learning") &&
      !is(object = object@analysis, class2 = "LearningMulti") &&
      !is.na(x = object@analysis)) {
    return( "incorrect object for @analysis" )
  }

  return( TRUE )
}

#' Class \code{BOWLBasic}
#'
#' Class \code{BOWLBasic} contains the results for a single OWL analysis and the
#'   weights needed for next iteration
#'
#' @name BOWLBasic-class
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
#' @slot prodPi Vector of the products of the propensity for the tx received 
#' @slot sumR Vector of the sum of the rewards
#' @slot index Vector indicating compliance with estimated optimal regime
#'
#' @keywords internal
#'
setClass(Class = "BOWLBasic",
         slots = c("analysis" = "ANY"),
         contains = c("BOWLObj"),
         prototype = list("prodPi" = numeric(),
                          "sumR" = numeric(),
                          "index" = logical(),
                          "analysis" = NA),
         validity = .validity_BOWL)

#' Create a BOWL Object
#'
#' @name newBOWLStep
#'
#' @param moPropen model object for propensity
#' @param fSet function specifying subsets or NULL
#' @param data data.frame of covariates and tx
#' @param response vector of responses
#' @param txName character indicating tx column in data
#' @param lambdas vector of tuning parameters
#' @param cvFolds number of cross-validation folds or NULL
#' @param kernel Kernel object
#' @param surrogate Surrogate object
#' @param guess vector of starting value for regime parameterse
#' @param prodPi vector of previous step propensity weights
#' @param index vector indicating previous compliance with regime
#' @param ... additional inputs sent to optimization method
#'
#' @return BOWLBasic object
#'
#' @keywords internal
.newBOWLStep <- function(moPropen,
                         fSet,
                         data,
                         response,
                         txName,
                         lambdas,
                         cvFolds,
                         kernel,
                         surrogate,
                         suppress,
                         guess,
                         prodPi,
                         index, ...) {

  if (sum(index) <= 1L) stop("Too few patients to perform analysis")

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
                           createObj = ".createowl", 
                           prodPi = prodPi,
                           index = index, ...)

  # predict propensity for tx received
  prWgt <- .getPrWgt(propenObj = analysis@propen, 
                     txObj = analysis@txInfo,  
                     data = data)

  # include previous propensity weights
  prWgt <- prWgt * prodPi

  # retrieve estimated optimal tx and decision function
  opt <- optTx(x = analysis)

  # reset index to indicate those that followed the optimal tx
  index <- index & {data[,txName] == opt$optimalTx}
  index[is.na(x = index)] <- FALSE

  if (suppress != 0L) cat(sum(index), "followed estimated regime.\n")

  bowlObj <- new("BOWLObj",
                 "prodPi" = prWgt,
                 "sumR"   = response,
                 "index"  = index)

  return( new("BOWLBasic", bowlObj, "analysis" = analysis) )
}

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{BOWLBasic}
#'
#' @name BOWLBasic-methods
#'
#' @keywords internal
NULL

#' @rdname BOWLBasic-methods
setMethod(f = "Call",
          signature = c(name = "BOWLBasic"),
          definition = function(name, ...) {
              return( Call(name = name@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "coef",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( coef(object = object@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "cvInfo",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( cvInfo(object = object@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "estimator",
          signature = c(x = "BOWLBasic"),
          definition = function(x, ...) {
              return( estimator(x = x@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "fitObject",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( fitObject(object = object@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "optimObj",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( optimObj(object = object@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "optTx",
          signature = c(x = "BOWLBasic",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, newdata = newdata, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "optTx",
          signature = c(x = "BOWLBasic",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "outcome",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) { return( NA ) })

#' @rdname BOWLBasic-methods
setMethod(f = "plot",
          signature = c(x = "BOWLBasic"),
          definition = function(x, suppress = FALSE, ...) {
              plot(x = x@analysis, suppress = suppress, ...)
            })

#' @rdname BOWLBasic-methods
setMethod(f = "print",
          signature = c(x = "BOWLBasic"),
          definition = function(x, ...) { print(x = x@analysis, ...) })

#' @rdname BOWLBasic-methods
setMethod(f = "propen",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( propen(object = object@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "regimeCoef",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@analysis, ...) )
            })

#' @rdname BOWLBasic-methods
setMethod(f = "show",
          signature = c(object = "BOWLBasic"),
          definition = function(object) { show(object = object@analysis) })

#' @rdname BOWLBasic-methods
setMethod(f = "summary",
          signature = c(object = "BOWLBasic"),
          definition = function(object, ...) {
              return( summary(object = object@analysis, ...) )
            })
