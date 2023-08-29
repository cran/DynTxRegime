# October 23, 2018

#' Class \code{QLearnObj}
#'
#' Class \code{QLearnObj} contains the results for a Q-Learning step
#'
#' @name QLearnObj-class
#' @docType class
#'
#' @slot outcome The outcome regression analysis
#' @slot txInfo The feasible tx information
#' @slot optimal The estimated optimal tx, decision function, and value
#'
#' @template outcomeOnly
#' @template regression
#' @template DynTxRegime_methods
setClass(Class = "QLearnObj",
         contains = c("OutcomeObj", "TxObj", "DynTxRegime"))

#' Class \code{QLearn}
#'
#' Class \code{QLearn} contains the results for a Q-Learning step
#'
#' @name QLearn-class
#' @docType class
#'
#' @slot step An integer indicating the step of the Q-Learning algorithm.
#' @slot outcome The outcome regression analysis
#' @slot txInfo The feasible tx information
#' @slot optimal The estimated optimal tx, decision function, and value
#'
#' @template outcomeOnly
#' @template regression
#' @template DynTxRegime_methods
setClass(Class = "QLearn",
         slots = c(step = "integer",
                   analysis = "ANY"))

#' Perform a Step of the Q-Learning Algorithm
#'
#' Method performs all necessary regression and predictions steps for a
#'   single step of the Q-learning algorithm.
#'
#' @return an object of class QLearn.
#'
#' @rdname newQLearn
#'
#' @keywords internal
setGeneric(name = ".newQLearn",
           def = function(response, ...) { standardGeneric(".newQLearn") })

#' Identify Statistical Method Used to Obtain Result
#'
#' Prints are displays a brief description of the statistical method used to
#'   obtain the input object.
#'
#' Methods are defined for all statistical methods implemented in DynTxRegime.
#'
#' @param object Value object returned by any statistical method of DynTxRegime
#'
#' @name DTRstep
#'
#' @usage
#'
#' DTRstep(object)
#' 
#' @export
setGeneric(name = "DTRstep",
           def = function(object) { standardGeneric("DTRstep") })

#' Perform the First Step of the Q-Learning Algorithm
#'
#' @param moMain modeling object specifying the main effects component of the
#'   outcome model
#' @param moCont modeling object specifying the contrasts component of the
#'   outcome model
#' @param fSet function defining the feasible tx subsets
#' @param response a vector or the value object returned by a prior call to 
#'   qlearn()
#' @param data data.frame of covariates and tx received
#' @param txName character name of tx variable in data
#' @param iter the maximum number of iterations in the iterative algorithm
#' @param suppress logical indicating user's screen printing preference
#'
#' @rdname newQLearn
setMethod(f = ".newQLearn",
          signature = c(response = "vector"),
          definition = function(moMain,
                                moCont,
                                fSet,
                                response,
                                data,
                                txName,
                                iter,
                                suppress) {

              if (!suppress) cat("First step of the Q-Learning Algorithm.\n")

              step <- 1L

              result <- .qLearn(moMain = moMain,
                                moCont = moCont,
                                data = data,
                                response = response,
                                txName = txName,
                                fSet = fSet,
                                iter = iter,
                                step = step,
                                suppress = suppress)

              return(result)

            })

#' Perform a Subsequent Step (non-first) of the Q-Learning Algorithm
#'
#' @rdname newQLearn
setMethod(f = ".newQLearn",
          signature = c(response = "QLearn"),
          definition = function(moMain,
                                moCont,
                                fSet,
                                response,
                                data,
                                txName,
                                iter,
                                suppress) {

              step <- response@step + 1L

              if (!suppress) {
                cat("Step", step, "of the Q-Learning Algorithm.\n")
              }

              # response is value function of previous
              response <- response@analysis@optimal@estimatedValue

              result <- .qLearn(moMain = moMain,
                                moCont = moCont,
                                data = data,
                                response = response,
                                txName = txName,
                                fSet = fSet,
                                iter = iter,
                                step = step,
                                suppress = suppress)

              return(result)

            })

#' @rdname DynTxRegime-internal-api
#' @export
setMethod(f = "Call",
          signature = c(name = "QLearn"),
          definition = function(name, ...) {
              return( Call(name = as(object = name@analysis, 
                                     Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
#' @export
setMethod(f = "coef",
          signature = c(object = "QLearn"),
          definition = function(object, ...) {
              return( coef(object = as(object = object@analysis, 
                                       Class = "OutcomeObj"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep",
          signature = c(object = "QLearn"),
          definition = function(object) {
              cat("Q-Learning: step", object@step,"\n")
            })

#' @rdname DynTxRegime-internal-api
#' @export
setMethod(f = "estimator",
          signature = c(x = "QLearn"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x@analysis, 
                                       Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
#' @export
setMethod(f = "fitObject",
          signature = c(object = "QLearn"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object@analysis, 
                                            Class = "OutcomeObj"), ...) )
            })

#' Uses .predictAll() defined for OutcomeObj objects
#'
#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "QLearn",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( .predictAll(object = as(object = x@analysis, Class = "OutcomeObj"), 
                                  newdata = newdata, ...) )
            })

#' Uses optTx defined for DynTxRegime objects
#'
#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "QLearn",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x@analysis, Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "QLearn"),
          definition = function(object, ...) {
              return( outcome(object = as(object = object@analysis, 
                                          Class = "OutcomeObj"), ...) )
            })

#' @rdname DynTxRegime-internal-api
#' @export
setMethod(f = "plot",
          signature = c(x = "QLearn", y = "missing"),
          definition = function(x, y, suppress = FALSE, ...) {
              plot(x = as(object = x@analysis, Class = "OutcomeObj"), 
                   suppress = suppress, ...)
            })

#' Print Q-Learning Information
#'
#' @rdname DynTxRegime-internal-api
setMethod(f = "print",
          signature = c(x = "QLearn"),
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = as(object = x@analysis, Class = "OutcomeObj"))
              print(x = as(object = x@analysis, Class = "DynTxRegime"))
            })

#' Show Q-Learning Information
#'
#' @rdname DynTxRegime-internal-api
setMethod(f = "show",
          signature = c(object = "QLearn"),
          definition = function(object) {
              DTRstep(object = object)
              show(as(object = object@analysis, Class = "OutcomeObj"))
              show(as(object = object@analysis, Class = "DynTxRegime"))
            })

#' @rdname DynTxRegime-internal-api
#' @export
setMethod(f = "summary",
          signature = c(object = "QLearn"),
          definition = function(object, ...) {
              res1 <- summary(as(object = object@analysis, Class = "OutcomeObj"))
              res2 <- summary(as(object = object@analysis, Class = "DynTxRegime"))
              return( c(res1, res2) )
            })

# @param moMain an object of class modelObj, ModelObj_SubsetList, or NULL
# @param moCont an object of class modelObj, ModelObj_SubsetList, or NULL
#    note that at least 1 of moMain and moCont must be defined
# @param data data.frame of covariates
# @param response response vector
# @param txName string; column header of treatment variable in data
# @param fSet a function or NULL
# @param iter maximum number of iterations if > 0 or NULL
# @param step integer step of the Q-learning algorithm 
# @param suppress TRUE/FALSE indicating of screen prints are generated
# @returns an object of class QLearn.
.qLearn <- function(moMain, 
                    moCont, 
                    data, 
                    response,
                    txName, 
                    fSet, 
                    iter, 
                    step, 
                    suppress) {

  txObj <- .newTxObj(fSet = fSet,
                     txName = txName,
                     data = data,
                     suppress = suppress)

  outcomeObj <- .newOutcomeObj(moMain = moMain,
                               moCont = moCont,
                               data = data,
                               response = response,
                               iter = iter,
                               txObj = txObj,
                               suppress = suppress)

  qfunc <- .predictAll(object = outcomeObj, 
                       newdata = data)

  value <- qfunc$decisionFunc
  value[is.na(x = value)] <- -Inf
  value <- apply(X = value, 
                 MARGIN = 1L, 
                 FUN = max, 
                 na.rm = TRUE)

  value[is.infinite(x = value)] <- response[is.infinite(x = value)]

  optObj <- new(Class = "OptimalObj",
                "optimal" = new(Class = "OptimalInfo",
                                "decisionFunc"   = qfunc$decisionFunc, 
                                "optimalTx"      = qfunc$optimalTx,
                                "estimatedValue" = value))

  if (!suppress) {
    cat("\n")
    print(x = optObj)
  }

  dtrObj <- new(Class = "DynTxRegime",
                optObj,
                "call" = NULL)

  analysis <- new(Class = "QLearnObj", txObj, outcomeObj, dtrObj)

  return( new(Class = "QLearn", "step" = step, "analysis" = analysis) )

}
