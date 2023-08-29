# October 25, 2018

.validity_OutcomeObj <- function(object) {

  # outcome must be OutcomeSimpleFit, OutcomeSimpleFit_fSet, 
  # OutcomeSimpleFit_SubsetList, OutcomeIterateFit, or 
  # DecisionPointList
  if (!is(object = object@outcome, class2 = "OutcomeNoFit") &&
      !is(object = object@outcome, class2 = "OutcomeSimpleFit") &&
      !is(object = object@outcome, class2 = "OutcomeSimpleFit_fSet") &&
      !is(object = object@outcome, class2 = "OutcomeSimpleFit_SubsetList") &&
      !is(object = object@outcome, class2 = "OutcomeIterateFit") &&
      !is(object = object@outcome, class2 = "DecisionPointList") &&
      !is.na(x = object@outcome)) {
    return( "incorrect object for @outcome" )
  }

  return( TRUE )
}

#' Class \code{OutcomeObj}
#'
#' Class \code{OutcomeObj} groups outcome regression results under a common
#'  name
#'
#' @name OutcomeObj-class
#'
#' @slot outcome ANY - expected to be \code{OutcomeNoFit}, 
#'   \code{OutcomeSimpleFit},
#'   \code{OutcomeSimpleFit_fSet}, \code{OutcomeSimpleFit_SubsetList}, 
#'   \code{OutcomeIterateFit}, or \code{DecisionPointList}.
#'
#' @include D_newModel.R D_OutcomeSimpleFit.R D_OutcomeSimpleFit_fSet.R
#' @include D_OutcomeSimpleFit_SubsetList.R D_OutcomeIterateFit.R
#'
#' @keywords internal
setClass(Class = "OutcomeObj",
         slots = c(outcome = "ANY"),
         prototype = list(outcome = NA),
         validity = .validity_OutcomeObj)

##########
## GENERICS
##########

#' Create a new \code{OutcomeObj} object
#'
#' Calls newOutcomeFit and stores in @outcome.
#'
#' @rdname newOutcomeObj
#'
#' @param moMain A modeling object for main effects
#' @param moCont A modeling object for contrasts
#' @param txObj A TxObj object
#' @param iter NULL or integer
#' @param ...  Any optional additional input.
#' 
#' @keywords internal
setGeneric(name = ".newOutcomeObj",
           def = function(moMain, moCont, txObj, iter, ...) {
                   standardGeneric(f = ".newOutcomeObj")
                 })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OutcomeObj}
#'
#' Most value objects returned are a list with one element 'outcome'.
#' Methods dispatched and objects returned in the element 'outcome'
#'   depend on class of @outcome.
#' Exceptions are noted below.
#'
#' @name OutcomeObj-methods
#'
#' @keywords internal
NULL

#' @rdname newOutcomeObj
setMethod(f = ".newOutcomeObj",
          signature = c(moMain = "ANY",
                        moCont = "ANY",
                        txObj = "ANY",
                        iter = "ANY"),
          definition = function(moMain,  
                                moCont,  
                                txObj, 
                                data,  
                                response,  
                                iter,  
                                suppress) {

              if (!suppress ) cat("\nOutcome regression.\n")

              .checkFSetAndOutcomeModels(txObj = txObj, 
                                         moMain = moMain,  
                                         moCont = moCont,
                                         data = data)

              return( new(Class = "OutcomeObj",
                          outcome = .newOutcomeFit(moMain = moMain,
                                                   moCont = moCont,
                                                   txObj = txObj,
                                                   data = data,
                                                   response = response,
                                                   iter = iter,
                                                   suppress = suppress)) )
            })


.qLearnDP1 <- function(moMain,
                       moCont,
                       txObj,
                       data,
                       response,
                       iter,
                       suppress){

  nDP <- length(x = txObj@txInfo)

  QfitObj <- list()
  fitObj <- list()

  while (nDP > 0L) {

    if (!suppress) cat("Decision Point", nDP, "\n")

    .checkFSetAndOutcomeModels(txObj = txObj@txInfo[[ nDP ]], 
                               moMain = moMain[[ nDP ]],  
                               moCont = moCont[[ nDP ]],
                               data = data)

    QfitObj[[ nDP ]] <- .newOutcomeFit(moMain = moMain[[ nDP ]],
                                       moCont = moCont[[ nDP ]],
                                       response = response,
                                       txObj = txObj@txInfo[[ nDP ]],
                                       data = data,
                                       iter = iter,
                                       suppress = suppress)

    pred <- .predictAll(QfitObj[[ nDP ]], data)
    vals <- pred$decisionFunc

    takeResp <- apply(X = vals, 
                      MARGIN = 1L, 
                      FUN = function(x){all(is.na(x = x))})

    hold <- response
    hold[!takeResp] <- apply(X = vals[!takeResp,,drop=FALSE],
                             MARGIN = 1L,
                             FUN = max,
                             na.rm = TRUE)
    response <- hold

    nDP <- nDP - 1L

  }


  QfitObj <- new(Class = "DecisionPointList", QfitObj)

  return( QfitObj )
}

.qLearnDP <- function(moMain,
                      moCont,
                      txObj,
                      data,
                      response,
                      iter,
                      suppress, ...){

  if (!suppress ) cat("\nOutcome regression.\n")

  QfitObj <- .qLearnDP1(moMain = moMain,
                        moCont = moCont,
                        txObj = txObj,
                        data = data,
                        response = response,
                        iter = iter,
                        suppress = suppress)

  result <- new(Class = "OutcomeObj",
                outcome = QfitObj)

  return( result )

}

#' @rdname newOutcomeObj
setMethod(f = ".newOutcomeObj",
          signature = c(moMain = "ModelObj_DecisionPointList",
                        moCont = "ModelObj_DecisionPointList",
                        txObj   = "TxInfoList",
                        iter = "ANY"),
          definition = .qLearnDP )

#' @rdname newOutcomeObj
setMethod(f = ".newOutcomeObj",
          signature = c(moMain = "ModelObj_DecisionPointList",
                        moCont = NULL,
                        txObj   = "TxInfoList",
                        iter = "ANY"),
          definition = .qLearnDP )

#' @rdname newOutcomeObj
setMethod(f = ".newOutcomeObj",
          signature = c(moMain = "NULL",
                        moCont = "ModelObj_DecisionPointList",
                        txObj   = "TxInfoList",
                        iter = "ANY"),
          definition = .qLearnDP )

#' @rdname OutcomeObj-methods
setMethod(f = "coef",
          signature = c(object = "OutcomeObj"),
          definition = function(object, ...) {
              if (is(object = object@outcome, class2 = "DecisionPointList")) {
                return( list("outcome" = .cycleList(object = object@outcome,
                                                    func = 'coef')) )
              } else if (!is(object = object@outcome, class2 = "OutcomeNoFit")){
                return( list("outcome" = coef(object = object@outcome)) )
              } else {
                return( NULL )
              }
            })

#' @rdname OutcomeObj-methods
setMethod(f = "fitObject",
          signature = c(object = "OutcomeObj"),
          definition = function(object, ...) {
              if (is(object = object@outcome, class2 = "DecisionPointList")) {
                return( list("outcome" = .cycleList(object = object@outcome,
                                                    func = 'fitObject')) )
              } else if (!is(object = object@outcome, class2 = "OutcomeNoFit")) {
                return( list("outcome" = fitObject(object = object@outcome)) )
              } else {
                return( NULL )
              }
            })

#' Retrieve Regression Analysis
#'
#' \code{outcome(object)} does not return the overarching list
#'   structure, but only the contents of list[[ outcome ]].
#'
#' @rdname OutcomeObj-methods
setMethod(f = "outcome",
          signature = c(object = "OutcomeObj"),
          definition = function(object, ...) {
              if (is(object = object@outcome, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@outcome, func = 'outcome') )
              } else {
                return( outcome(object = object@outcome) )
              }
            } )

#' Plot regression result
#'
#' \code{plot(x)} concatenated 'outcome' to the title if suppress = FALSE.
#'
#' @rdname OutcomeObj-methods
setMethod(f = "plot",
          signature = c(x = "OutcomeObj"),
          definition = function(x, suppress = FALSE, ...) {
              if (is(object = x@outcome, class2 = "OutcomeNoFit")) {
                return("no outcome object")
              }

              argList <- list(...)
              if (!suppress) {
                argList <- .titleIt(argList, "outcome")
              }
              argList[[ "x" ]] <- x@outcome
              argList[[ "suppress" ]] <- suppress

              do.call(what = plot, args = argList)
            })

#' Make Predictions for All Tx.
#'
#' \code{.predictAll(object, newdata)} does not return the overarching list
#'   structure, but only the contents of list[[ outcome ]].
#'
#' @rdname OutcomeObj-methods
setMethod(f = ".predictAll",
          signature = c(object = "OutcomeObj",
                        newdata = "data.frame"),
          definition = function(object, newdata, ...) {
              if (is(object = object@outcome, class2 = "DecisionPointList")) {
                stop("not allowed")
              } else {
                return( .predictAll(object = object@outcome, 
                                    newdata = newdata, ...) )
              }
            })

#' Make Predictions Regression for All Tx
#'
#' \code{.predictMu(object, newdata)}
#'   predicts outcome for all tx options.
#'   Returns the matrix of outcomes predicted for all tx. 
#'   Predicted outcomes for tx not available to a pt are NA.
#'
#' @rdname OutcomeObj-methods
setMethod(f = ".predictMu",
          signature = c(object = "OutcomeObj",
                        data = "data.frame"),
          definition = function(object, data, ...) {
              return( .predictMu(object = object@outcome, data = data) )
            })


#' Make Predictions
#'
#' \code{predict(object)} does not return the overarching list
#'   structure, but only the contents of list[[ outcome ]].
#'
#' @rdname OutcomeObj-methods
setMethod(f = "predict",
          signature = c(object = "OutcomeObj"),
          definition = function(object, ...) {
              if (is(object = object@outcome, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@outcome, func = 'predict') )
              } else {
                return( predict(object = object@outcome, ...) )
              }
            })

#' @rdname OutcomeObj-methods
setMethod(f = "print",
          signature = c(x = "OutcomeObj"),
          definition = function(x, ...) {
              cat("Outcome Regression Analysis\n")
              print(x = x@outcome, ...)
            })

#' @rdname OutcomeObj-methods
setMethod(f = "show",
          signature = c(object = "OutcomeObj"),
          definition = function(object) {
              cat("Outcome Regression Analysis\n")
              show(object = object@outcome)
            })

#' @rdname OutcomeObj-methods
setMethod(f = "summary",
          signature = c(object = "OutcomeObj"),
          definition = function(object, ...) {
              if (is(object = object@outcome, class2 = "DecisionPointList")) {
                return( list("outcome" = .cycleList(object = object@outcome,
                                                    func = 'summary')) )
              } else {
                return( list("outcome" = summary(object = object@outcome, ...)) )
              }
            })
