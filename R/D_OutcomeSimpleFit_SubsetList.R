# October 25, 2018

#' Class \code{OutcomeSimpleFit_SubsetList}
#'
#' Class \code{OutcomeSimpleFit_SubsetList} is a \code{TypedFit_SubsetList} 
#'   identified as being for an outcome regression step.
#'
#' @name OutcomeSimpleFit_SubsetList-class
#'
#' @include D_newModel.R D_OutcomeSimpleFit.R
#'
#' @keywords internal
setClass(Class = "OutcomeSimpleFit_SubsetList",
         contains = c("TypedFit_SubsetList"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OutcomeSimpleFit_SubsetList}
#'
#' Methods call equivalently named methods defined for \code{TypedFit_SubsetList}
#'
#' @name OutcomeSimpleFit_SubsetList-methods
#'
#' @keywords internal
NULL

#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "ModelObj_SubsetList",
                        txObj = "TxInfoWithSubsets",
                        iter = NULL),
          definition = function(moMain,
                                moCont,
                                txObj,
                                data,
                                response,
                                iter,
                                suppress) {

              nmsMoMain <- names(x = moMain)
              nmsMoCont <- names(x = moCont)

              nms <- sort(x = unique(x = c(nmsMoMain, nmsMoCont)))

              txName <- .getTxName(object = txObj)

              type <- rep(x = "moMain", times = length(x = nms))

              newMO <- list()

              for (i in 1L:length(x = nms)) {
                # identify which contrast model partners this main effects
                if ({nms[i] %in% nmsMoCont} && {nms[i] %in% nmsMoMain}) {
                  newMO[[ nms[i] ]] <- .newModel(moMain = moMain[[ nms[i] ]],
                                                 moCont = moCont[[ nms[i] ]],
                                                 txName = txName,
                                                 suppress = TRUE)
                  type[i] <- "Combined"
                } else if (nms[i] %in% nmsMoCont) {
                  newMO[[ nms[i] ]] <- .newModel(moMain = NULL,
                                                 moCont = moCont[[ nms[i] ]],
                                                 txName = txName,
                                                 suppress = TRUE)
                  type[i] <- "moCont"
                } else if (nms[i] %in% nmsMoMain) {
                  newMO[[ nms[i] ]] <- .newModel(moMain = moMain[[ nms[i] ]],
                                                 moCont = NULL,
                                                 txName = txName,
                                                 suppress = TRUE)
                } else {
                  stop("this should never happen")
                }
              }

              newMO <- new(Class = "ModelObj_SubsetList", newMO)

              res <- .newTypedFit(modelObj = newMO,
                                  txObj = txObj,
                                  data = data,
                                  response = response,
                                  type = type,
                                  suppress = suppress)

              return( new(Class = "OutcomeSimpleFit_SubsetList", res) )
            })


#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "ModelObj_SubsetList",
                        moCont = "NULL",
                        txObj = "TxInfoWithSubsets",
                        iter = NULL),
          definition = function(moMain,
                                moCont,
                                txObj,
                                data,
                                response,
                                iter,
                                suppress) {

              type <- rep(x = "moMain", times = length(x = moMain))

              res <- .newTypedFit(modelObj = moMain,
                                  txObj = txObj,
                                  data = data,
                                  response = response,
                                  type = type,
                                  suppress = suppress)

              return( new(Class = "OutcomeSimpleFit_SubsetList", res) )
            })


#' @rdname newOutcomeFit
setMethod(f = ".newOutcomeFit",
          signature = c(moMain = "NULL",
                        moCont = "ModelObj_SubsetList",
                        txObj = "TxInfoWithSubsets",
                        iter = NULL),
          definition = function(moMain,
                                moCont,
                                txObj,
                                data,
                                response,
                                iter,
                                suppress) {

              txName <- .getTxName(object = txObj)

              type <- rep(x = "moCont", times = length(x = moCont))

              newC <- list()

              for (i in names(x = moCont)) {
                  newC[[ i ]] <- .newModel(moMain = NULL,
                                           moCont = moCont[[ i ]],
                                           txName = txName,
                                           suppress = TRUE)
              }

              moCont <- new(Class = "ModelObj_SubsetList", newC)

              res <- .newTypedFit(modelObj = moCont,
                                  txObj = txObj,
                                  data = data,
                                  response = response,
                                  type = type,
                                  suppress = suppress)

              return( new(Class = "OutcomeSimpleFit_SubsetList", res) )
            })

#' @rdname OutcomeSimpleFit_SubsetList-methods
setMethod(f = "outcome",
          signature = c(object = "OutcomeSimpleFit_SubsetList"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object, Class = "TypedFit_SubsetList")) )
            })

#' @rdname OutcomeSimpleFit_SubsetList-methods
setMethod(f = "predict",
          signature = c(object = "OutcomeSimpleFit_SubsetList"),
          definition = function(object, newdata, ...) {
              pred <- predict(object = as(object = object, 
                                          Class = "TypedFit_SubsetList"), 
                              newdata = newdata)
              preds <- NULL
              for (i in 1L:length(x = pred)) {
                preds <- cbind(preds, pred[[ i ]])
              }
              preds <- rowSums(preds, na.rm=TRUE)
              return( preds )
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
#' @rdname OutcomeSimpleFit_SubsetList-methods
setMethod(f = ".predictAll",
          signature = c(object = "OutcomeSimpleFit_SubsetList",
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

                nms <- names(x = subsets)[i]
                usePts <- ptsSubset %in% nms
                if (!any(usePts) ) next

                if (length(x = subsets[[ i ]]) == 1L) {
                  # this combination of ModelObjSubset and TxInfoWithSubsets
                  # can be used when singletons are to be included in
                  # models; and thus every subset must be sent to prediction 
                  # methods (why we don't cycle here as opposed to fSet method)
                  optimalTx[usePts] <- subsets[[ i ]][1L]
                }

                useFit <- NULL
                for (k in 1L:length(x = object@fits)) {
                  fitName <- unlist(x = strsplit(x = names(x = object@fits)[k], 
                                                 split = ","))
                  if (nms %in% fitName) {
                    useFit <- k
                    break
                  }
                }

                # in instances when a singleton is not included in fits,
                # useFit will never be reset from NULL, thus cycle
                # if used for fits, useFit will be TRUE for those patients
                if (is.null(x = useFit) ) next

                for (j in 1L:length(x = superset)) {
                  # in instances when a singleton is included in fit,
                  # the following if statement ensures that the outcome
                  # for only the allowed treatment is estimated
                  if (!(superset[j] %in% subsets[[ i ]]) ) next
                  newdata[usePts,txName] <- superset[j]
                  dFunc[usePts,j] <- predict(object = object@fits[[ useFit ]],
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

#' Make Predictions Regression for All Tx
#'
#' \code{.predictMu(object, data)}
#'   predicts outcome for all tx options.
#'   Returns the matrix of outcomes predicted for all tx. 
#'   Predicted outcomes for tx not available to a pt are NA.
#'
#' @rdname OutcomeSimpleFit_SubsetList-methods
setMethod(f = ".predictMu",
          signature = c(object = "OutcomeSimpleFit_SubsetList",
                        data = "data.frame"),
          definition = function(object, data, ...) {

              # predict outcome for all tx
              superset <- .getSuperset(object = object@txInfo)

              or <- matrix(data = NA,
                           nrow = nrow(x = data),
                           ncol = length(x = superset),
                           dimnames = list(NULL, superset))

              txName <- .getTxName(object = object@txInfo)

              subsets <- .getSubsets(object = object@txInfo)
              ptsSubset <- .getPtsSubset(object = object@txInfo)

              for (i in 1L:length(x = object@fits)) {

                nms <- names(x = object@fits)[i]
                fitNames <- unlist(x = strsplit(x = nms, split = ","))

                usePts <- ptsSubset %in% fitNames

                if (!any(usePts) ) next

                txOpts <- NULL
                for (j in 1L:length(x = fitNames)) {
                  txOpts <- c(txOpts, subsets[[ fitNames[j] ]])
                }
                txOpts <- sort(x = unique(x = txOpts))

                for (j in 1L:length(x = superset)) {
                  if (!(superset[j] %in% txOpts)) next
                  data[usePts,txName] <- superset[j]
                  or[usePts,j] <- predict(object = object@fits[[ i ]],
                                          newdata = data[usePts,])
                }

              }

              return( or )
            })
