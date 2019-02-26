# October 23, 2018

#' Class \code{ClassificationFit_SubsetList}
#'
#' Class \code{ClassificationFit_SubsetList} contains a 
#'   \code{TypedFit_SubsetList} object to define classification regression
#'   results when subsets are identified and modeled uniquely.
#'
#' @name ClassificationFit_SubsetList-class
#' @docType class
#'
#' @keywords internal
#'
#' @include I_ClassificationFit.R
setClass(Class = "ClassificationFit_SubsetList",
         contains = c("TypedFit_SubsetList"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{ClassificationFit_SubsetList}
#'
#' @name ClassificationFit_SubsetList-methods
#'
#' @keywords internal
NULL

#' @rdname newClassificationFit
setMethod(f = ".newClassificationFit",
          signature = c(moClass = "ModelObj_SubsetList",
                        txObj   = "TxInfoWithSubsets"),
          definition = function(moClass,
                                data,
                                response,
                                txObj,
                                suppress, ...) {

              if (!suppress) cat("\nClassification Analysis\n")

              # retrieve subset membership for pts
              ptsSubset <- .getPtsSubset(object = txObj)

              for (i in 1L:length(x = moClass)) {
                # extract subset names to be included for ith model object
                nms <- unlist(x = strsplit(x = names(x = moClass)[i], 
                                           split = ","))

                # identify pts that are in these subsets
                usePts <- ptsSubset %in% nms

                # normalize the weight using only these pts
                data$wgt[usePts] <- data$wgt[usePts] / sum(data$wgt[usePts])
              }

              # perform classification regressions
              typedFit <- .newTypedFit(modelObj = moClass,
                                       txObj = txObj,
                                       response = response,
                                       data = data,
                                       type = "moClass",
                                       suppress = suppress)

              res <- new(Class = "ClassificationFit_SubsetList", typedFit)

              return( res )
            })

#' \code{classif(object)}
#'   retrieves the object returned by the classification regression 
#'   analysis. A list is returned, one element for each subset modeled.
#'   Calls method defined for \code{TypedFit_SubsetList}.
#'
#' @rdname ClassificationFit_SubsetList-methods
setMethod(f = "classif",
          signature = c(object = "ClassificationFit_SubsetList"),
          definition = function(object, ...) {
              res <- fitObject(object = as(object = object, 
                                           Class = "TypedFit_SubsetList"), ...)
              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moClass" ]]
              }
              return( res2 )
            })

#' \code{coef(object)} 
#'   calls coef method defined for the objects returned by the classification
#'   regression analyses. A list is returned, one element for each subset
#'   modeled. Calls method defined for \code{TypedFit_SubsetList}.
#'
#' @rdname ClassificationFit_SubsetList-methods
setMethod(f = "coef",
          signature = c(object = "ClassificationFit_SubsetList"),
          definition = function(object, ...) {
              res <- callNextMethod()
              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moClass" ]]
              }
              return( res2 )
            })

#' \code{fitObject(object)}
#'   retrieves the objects returned by the classification regression 
#'   analyses. A list is returned, one element for each subset modeled.
#'   Calls method defined for \code{TypedFit_SubsetList}.
#'
#' @rdname ClassificationFit_SubsetList-methods
setMethod(f = "fitObject",
          signature = c(object = "ClassificationFit_SubsetList"),
          definition = function(object, ...) {
              res <- callNextMethod()
              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moClass" ]]
              }
              return( res2 )
            })

#' \code{predict(object, ...)}
#'   calls predict method defined for the objects returned by the classification
#'   regression analyses.  Calls method defined for \code{TypedFit_SubsetList}.
#'
#' @rdname ClassificationFit_SubsetList-methods
setMethod(f = "predict",
          signature = c(object = "ClassificationFit_SubsetList"),
          definition = function(object, ...) {

              predL <- callNextMethod()

              pred <- rep(x = NA, times = length(x = predL[[ 1L ]]))

              for (i in 1L:length(x = predL)) {
                if (length(x = predL[[ i ]]) != length(x = pred)) {
                  stop("dim problem")
                }
                usePts <- !is.na(x = predL[[ i ]])
                pred[usePts] <- predL[[ i ]][usePts]
              }

              return( pred )
            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts optimal treatment
#'
#' @rdname ClassificationFit_SubsetList-methods
setMethod(f = ".predictAll",
          signature = c(object = "ClassificationFit_SubsetList",
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

              dFunc <- NA

              optimalTx <- rep(x = NA, times = nrow(x = newdata))

              subsets <- .getSubsets(object = object@txInfo)
              ptsSubset <- .getPtsSubset(object = txNew)

              for (i in 1L:length(x = subsets)) {

                nms <- names(x = subsets)[i]
                usePts <- ptsSubset %in% nms
                if (!any(usePts) ) next

                if (length(x = subsets[[ i ]]) == 1L) {
                  optimalTx[usePts] <- subsets[[ i ]][1L]
                  next
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

                if (is.null(x = useFit) ) next

                pred <- predict(object = object@fits[[ useFit ]],
                                newdata = newdata[usePts,])

                isBase <- pred %in% c(0,"0")

                optimalTx[usePts][ isBase] <- subsets[[ i ]][1L]
                optimalTx[usePts][!isBase] <- subsets[[ i ]][2L]
              }

              optimalTx <- .convertTx(object = object@txInfo, txVec = optimalTx)

              return( list("optimalTx"    = optimalTx,
                           "decisionFunc" = dFunc) )
            })

#' \code{summary(object)}
#'   calls the summary method defined for the objects returned by the
#'   classification regression analyses. A list is returned, one element for
#'   each subset modeled.  Calls method defined for \code{TypedFit_SubsetList}.
#'
#' @rdname ClassificationFit_SubsetList-methods
setMethod(f = "summary",
          signature = c(object = "ClassificationFit_SubsetList"),
          definition = function(object, ...) {
              res <- callNextMethod()
              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moClass" ]]
              }
              return( res2 )
            })
