# October 25, 2018

#' Class \code{OutcomeSimpleFit}
#'
#' Class \code{OutcomeSimpleFit} is a \code{TypedFit} identified as being
#'   for an outcome regression step.
#'
#' @name OutcomeSimpleFit-class
#'
#' @include D_newModel.R D_OutcomeNoFit.R
#'
#' @keywords internal
setClass(Class = "OutcomeSimpleFit",
         contains = c("TypedFit", "TxInfoNoSubsets"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OutcomeSimpleFit}
#'
#' Methods call equivalently named methods defined for \code{TypedFit}
#'
#' @name OutcomeSimpleFit-methods
#'
#' @keywords internal
NULL

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txObj = "TxInfoNoSubsets",
                        iter = "NULL"),
          definition = function(moMain,
                                moCont,
                                txObj,
                                iter,
                                data,
                                response,
                                suppress) {

              mobj <- .newModel(moMain = moMain,
                                moCont = moCont,
                                txName = .getTxName(object = txObj),
                                suppress = suppress)

              result <- .newTypedFit(modelObj = mobj,
                                     data = data,
                                     response = response,
                                     type = "Combined",
                                     txObj = txObj,
                                     suppress = suppress)

              return( new(Class = "OutcomeSimpleFit", txObj, result) )
            })

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        txObj = "TxInfoNoSubsets",
                        iter = "NULL"),
          definition = function(moMain,
                                moCont,
                                txObj,
                                iter,
                                data,
                                response,
                                suppress) {

              mobj <- .newModel(moMain = moMain,
                                moCont = moCont,
                                txName = .getTxName(object = txObj),
                                suppress = suppress)

              result <- .newTypedFit(modelObj = mobj,
                                     data = data,
                                     response = response,
                                     type = "moMain",
                                     txObj = txObj,
                                     suppress = suppress)

              return( new(Class = "OutcomeSimpleFit", txObj, result) )
            })


#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        txObj = "TxInfoNoSubsets",
                        iter = "NULL"),
          definition = function(moMain,
                                moCont,
                                txObj,
                                iter,
                                data,
                                response,
                                suppress) {

              mobj <- .newModel(moMain = moMain,
                                moCont = moCont,
                                txName = .getTxName(object = txObj),
                                suppress = suppress)

              result <- .newTypedFit(modelObj = mobj,
                                     data = data,
                                     response = response,
                                     type = "moCont",
                                     txObj = txObj,
                                     suppress = suppress)

              return( new(Class = "OutcomeSimpleFit", txObj, result) )
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "coef",
          signature = c(object = "OutcomeSimpleFit"),
          definition = function(object, ...) {
              return( coef(object = as(object = object, 
                                       Class = "TypedFit"), ...) )
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "fitObject",
          signature = c(object = "OutcomeSimpleFit"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object,  
                                            Class = "TypedFit"), ...) )
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "outcome",
          signature = c(object = "OutcomeSimpleFit"),

          definition = function(object, ...) {
              return( fitObject(object = as(object = object, 
                                            Class = "TypedFit")) )
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "plot",
          signature = c(x = "OutcomeSimpleFit"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = as(object = x, Class = "TypedFit"),
                   suppress = suppress, ...)
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "predict",
          signature = c(object = "OutcomeSimpleFit"),
          definition = function(object, ...) {
              return( predict(object = as(object = object, 
                                          Class = "TypedFit"), ...)) 
            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts outcome for all tx options.
#'   Returns a list containing 'optimalTx' the tx yielding the largest 
#'   predicted outcome and 'decisionFunc' the matrix of outcomes predicted
#'   for all tx. 
#'
#' @rdname OutcomeSimpleFit-methods
setMethod(f = ".predictAll",
          signature = c(object = "OutcomeSimpleFit",
                        newdata = "data.frame"),
          definition = function(object, newdata) {

              superset <- .getSuperset(object = object@txInfo)

              txName <- .getTxName(object = object@txInfo)

              prediction <- matrix(data = NA,
                                   nrow = nrow(x = newdata),
                                   ncol = length(x = superset),
                                   dimnames = list(NULL, superset))

              for (i in 1L:length(x = superset)) {
                newdata[,txName] <- superset[i]
                prediction[,i] <- predict(object = as(object,"TypedFit"), 
                                          newdata = newdata)
              }

              optimalTx <- apply(X = prediction, MARGIN = 1L, FUN = which.max)

              optimalTx <- superset[optimalTx]
              optimalTx <- .convertTx(object = object@txInfo, txVec = optimalTx)

              return( list("optimalTx"    = optimalTx,
                           "decisionFunc" = prediction) )
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "print",
          signature = c(x = "OutcomeSimpleFit"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "TypedFit"))
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "show",
          signature = c(object = "OutcomeSimpleFit"),
          definition = function(object) {
              show(object = as(object = object, Class = "TypedFit"))
            })

#' @rdname OutcomeSimpleFit-methods
setMethod(f = "summary",
          signature = c(object = "OutcomeSimpleFit"),
          definition = function(object, ...) {
              return( summary(object = as(object = object,  
                                          Class = "TypedFit"), ...) )
            })
