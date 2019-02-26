# October 25, 2018

#' Class \code{OutcomeSimpleFit_fSet}
#'
#' Class \code{OutcomeSimpleFit_fSet} is a \code{TypedFit_fSet} identified as
#'   being for an outcome regression step.
#'
#' @name OutcomeSimpleFit_fSet-class
#'
#' @include D_newModel.R D_OutcomeSimpleFit.R
#'
#' @keywords internal
setClass(Class = "OutcomeSimpleFit_fSet",
         contains = c("TypedFit_fSet"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OutcomeSimpleFit_fSet}
#'
#' Methods call equivalently named methods defined for \code{TypedFit_fSet}
#'
#' @name OutcomeSimpleFit_fSet-methods
#'
#' @keywords internal
NULL

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txObj = "TxInfoWithSubsets",
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
                                     txObj = txObj,
                                     data = data,
                                     response = response,
                                     type = "Combined",
                                     suppress = suppress)

              result <- new(Class = "OutcomeSimpleFit_fSet", result)

              return( result )
            })

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        txObj = "TxInfoWithSubsets",
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
                                     txObj = txObj,
                                     data = data,
                                     response = response,
                                     type = "moMain",
                                     suppress = suppress)

              result <- new(Class = "OutcomeSimpleFit_fSet", result)

              return( result )
            })


#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        txObj = "TxInfoWithSubsets",
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
                                     txObj = txObj,
                                     data = data,
                                     response = response,
                                     type = "moCont",
                                     suppress = suppress)

              result <- new(Class = "OutcomeSimpleFit_fSet", result)

              return( result )
            })

#' @rdname OutcomeSimpleFit_fSet-methods
setMethod(f = "outcome",
          signature = c(object = "OutcomeSimpleFit_fSet"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object, Class = "TypedFit_fSet")) )
            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts outcome for all tx options.
#'   Returns a list containing 'optimalTx' the tx yielding the largest 
#'   predicted outcome and 'decisionFunc' the matrix of outcomes predicted
#'   for all tx. 
#'   Predicted outcomes for tx not available to a pt are NA.
#'
#' @rdname OutcomeSimpleFit_fSet-methods
setMethod(f = ".predictAll",
          signature = c(object = "OutcomeSimpleFit_fSet",
                        newdata = "data.frame"),
          definition = function(object, newdata, ...) {

              superset <- .getSuperset(object = object@txInfo)

              txName <- .getTxName(object = object@txInfo)

              if (!any(colnames(x = newdata) %in% txName)) {
                newdata[,txName] <- superset[1L]
              }

              txNew <- .newTxObj(fSet = .getSubsetRule(object = object@txInfo),
                                 txName = txName,
                                 data = newdata,
                                 suppress = TRUE,
                                 verify = FALSE)

              dFunc <- matrix(data = NA,
                              nrow = nrow(x = newdata),
                              ncol = length(x = superset),
                              dimnames = list(NULL, superset))

              optimalTx <- rep(x = NA, times = nrow(x = newdata))

              subsets <- .getSubsets(object = object@txInfo)

              ptsSubset <- .getPtsSubset(object = txNew)

              for (i in 1L:length(x = subsets)) {

                nms <- unlist(x = strsplit(x = names(x = subsets)[i], split = ","))

                usePts <- ptsSubset %in% nms
                if (!any(usePts) ) next

                if (length(x = subsets[[ i ]]) == 1L) {
                  optimalTx[usePts] <- subsets[[ i ]]
                  next
                }

                for (j in 1L:length(x = superset)) {
                  if (!(superset[j] %in% subsets[[ i ]]) ) next

                  newdata[usePts,txName] <- superset[j]
                  dFunc[usePts,j] <- predict(object = as(object = object, 
                                                         Class = "TypedFit_fSet"),
                                             newdata = newdata[usePts,])
                }

                optimalTx[usePts] <- superset[apply(X = dFunc[usePts,,drop=FALSE],
                                                    MARGIN = 1L,
                                                    FUN = which.max)]
              }

              optimalTx <- .convertTx(object = object@txInfo, txVec = optimalTx)

              return( list("optimalTx"    = optimalTx,
                           "decisionFunc" = dFunc) )
            })
