# October 25, 2018

#' Class \code{OutcomeNoFit}
#'
#' Class \code{OutcomeNoFit} designates that an outcome regression step
#'   was not performed. This acts as a place holder for IPW based methods.
#'
#' @name OutcomeNoFit-class
#'
#' @keywords internal
setClass(Class = "OutcomeNoFit", contains = c("TxObj"))

##########
## GENERICS
##########

#' Perform an Outcome Regression Step
#'
#' Dispatch appropriate methods to perform outcome regression step.
#'
#' @name newOutcomeFit
#'
#' @param moMain A modeling object for main effects or NULL
#' @param moCont A modeling object for contrasts or NULL
#' @param txObj A TxObj object
#' @param iter NULL or numeric
#' @param ...  Any optional additional input.
#' 
#' @keywords internal
setGeneric(name = ".newOutcomeFit",
           def = function(moMain, moCont, txObj, iter, ...) {
               standardGeneric(".newOutcomeFit")
             })

#' Make Predictions for all Treatments.
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".predictAll",
           def = function(object, newdata, ...) {
               standardGeneric(".predictAll")
             })

#' Make Predictions for all Treatments.
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".predictMu",
           def = function(object, data, ...) {
               standardGeneric(".predictMu")
             })

#' Retrieve Outcome Regression Analysis
#'
#' For statistical methods that require an outcome regression analysis,
#'   the value object returned by the modeling function(s) is retrieved.
#'
#' Methods are defined for all statistical methods implemented in DynTxRegime
#'   that use outcome regression. 
#' 
#' @name outcome
#'
#' @param object A value object returned by a statistical method of DynTxRegime.
#' @param ... Ignored.
#'
#' @usage
#' outcome(object, ...)
#'
#' @exportMethod outcome
setGeneric(name = "outcome",
           def = function(object, ...) { standardGeneric("outcome") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OutcomeNoFit}
#'
#' Methods return NULL, NA or zero values.
#'
#' @name OutcomeNoFit-methods
#'
#' @keywords internal
NULL

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "NULL",
                        moCont = "NULL",
                        txObj = "TxObj",
                        iter = "NULL"),
          definition = function(moMain,
                                moCont,
                                txObj,
                                iter,
                                data,
                                response,
                                suppress) {
              if (!suppress ) cat("No outcome regression performed.\n")

              return( new(Class = "OutcomeNoFit", txObj) )
            })

#' \code{.predictAll(object, newdata)}
#'
#' \code{.predictAll(object, newdata)} returns a list containing the optimal
#'   tx as a vector of NA values and the decision function as a matrix of 0
#'
#' @rdname OutcomeNoFit-methods
setMethod(f = ".predictAll",
          signature = c(object = "OutcomeNoFit",
                        newdata = "data.frame"),
          definition = function(object, newdata) {

              superset <- .getSuperset(object = object@txInfo)

              prediction <- matrix(data = 0.0,
                                   nrow = nrow(x = newdata),
                                   ncol = length(x = superset),
                                   dimnames = list(NULL, superset))

              optimalTx <- rep(x = NA, times = nrow(x = newdata))

              return( list("optimalTx"    = optimalTx,
                           "decisionFunc" = prediction) )
            })

#' Make Predictions Regression for All Tx
#'
#' \code{.predictMu(object, newdata)}
#'   predicts outcome for all tx options.
#'   Returns the matrix of outcomes predicted for all tx. 
#'   Predicted outcomes for tx not available to a pt are NA.
#'
#' @rdname OutcomeNoFit-methods
setMethod(f = ".predictMu",
          signature = c(object = "OutcomeNoFit",
                        data = "data.frame"),
          definition = function(object, data, ...) {

              return( .predictAll(object = object, 
                                  newdata = data)$decisionFunc )
            })

#' @rdname OutcomeNoFit-methods
setMethod(f = "outcome",
          signature = c(object = "OutcomeNoFit"),
          definition = function(object, ...) { return( NA ) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "coef",
          signature = c(object = "OutcomeNoFit"),
          definition = function(object, ...) { return( NA ) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "fitObject",
          signature = c(object = "OutcomeNoFit"),
          definition = function(object, ...) { return( NA ) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "plot",
          signature = c(x = "OutcomeNoFit"),
          definition = function(x, suppress=FALSE, ...) { return( NULL ) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "predict",
          signature = c(object = "OutcomeNoFit"),
          definition = function(object, ...) { return( NULL ) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "print",
          signature = c(x = "OutcomeNoFit"),
          definition = function(x, ...) { print(x = NA) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "show",
          signature = c(object = "OutcomeNoFit"),
          definition = function(object) { show(object = NA) })

#' @rdname OutcomeNoFit-methods
setMethod(f = "summary",
          signature = c(object = "OutcomeNoFit"),
          definition = function(object, ...) { return( NA ) })
