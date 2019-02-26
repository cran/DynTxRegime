# October 26, 2018

#' Class \code{ClassificationFit_fSet}
#'
#' Class \code{ClassificationFit_fSet} contains a \code{TypedFit_fSet} object to
#'   define a classification regression result when subsets are identified but
#'   not modeled uniquely.
#'
#' @name ClassificationFit_fSet-class
#' @docType class
#'
#' @keywords internal
#'
#' @include I_ClassificationFit.R
setClass(Class = 'ClassificationFit_fSet',
         contains = c("TypedFit_fSet"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{ClassificationFit_fSet}
#'
#' @name ClassificationFit_fSet-methods
#'
#' @keywords internal
NULL

#' @rdname newClassificationFit
setMethod(f = ".newClassificationFit",
          signature = c(moClass = "modelObj",
                        txObj   = "TxInfoWithSubsets"),
          definition = function(moClass, 
                                txObj,  
                                response,  
                                data,  
                                suppress, ...) {

              if (!suppress) cat("\nClassification Analysis\n")

              # retrieve indicator for single tx option
              singles <- .getSingleton(object = txObj)

              # the weights for the classification are the normalized
              # absolute contrast. use only pts with >1 tx option
              data$wgt <- data$wgt / sum(data$wgt[!singles])

              # perform classification regression for pts with >1 tx option
              typedFit <- .newTypedFit(modelObj = moClass,
                                       txObj = txObj,
                                       response = response,
                                       data = data,
                                       type = "moClass",
                                       suppress = suppress)

              return( new(Class = "ClassificationFit_fSet", typedFit) )
            })

#' \code{classif(object)}
#'   retrieves the object returned by the classification regression 
#'   analysis. Calls method defined for \code{TypedFit_fSet}.
#'
#' @rdname ClassificationFit_fSet-methods
setMethod(f = "classif",
          signature = c(object = "ClassificationFit_fSet"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object, 
                                            Class = "TypedFit_fSet"))$moClass )
            })

#' \code{coef(object)} 
#'   calls coef method defined for the object returned by the classification
#'   regression analysis. Calls method defined for \code{TypedFit_fSet}.
#'
#' @rdname ClassificationFit_fSet-methods
setMethod(f = "coef",
          signature = c(object = "ClassificationFit_fSet"),
          definition = function(object, ...) {
              return( callNextMethod()$moClass )
            })

#' \code{fitObject(object)}
#'   retrieves the object returned by the classification regression 
#'   analysis. Calls method defined for \code{TypedFit_fSet}.
#'
#' @rdname ClassificationFit_fSet-methods
setMethod(f = "fitObject",
          signature = c(object = "ClassificationFit_fSet"),
          definition = function(object, ...) {
              return( callNextMethod()$moClass )
            })

#' Make Predictions for Optimal Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts optimal treatment
#'
#' @rdname ClassificationFit_fSet-methods
setMethod(f = ".predictAll",
          signature = c(object = "ClassificationFit_fSet",
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

                nms <- unlist(x = strsplit(x = names(x = subsets)[i], split = ","))

                usePts <- ptsSubset %in% nms
                if (!any(usePts) ) next

                if (length(x = subsets[[ i ]]) == 1L) {
                  optimalTx[usePts] <- subsets[[ i ]]
                  next
                }

                pred <- predict(object = object, newdata = newdata[usePts,])
                isBase <- pred %in% c(0,"0")

                optimalTx[usePts][ isBase] <- subsets[[ i ]][1L]
                optimalTx[usePts][!isBase] <- subsets[[ i ]][2L]
              }

              optimalTx <- .convertTx(object = object@txInfo, txVec = optimalTx)

              return( list("optimalTx"    = optimalTx,
                           "decisionFunc" = dFunc) )

            })

#' \code{summary(object)}
#'   calls the summary method defined for the object returned by the
#'   classification regression analysis.  Calls method defined for 
#'   \code{TypedFit_fSet}.
#'
#' @rdname ClassificationFit_fSet-methods
setMethod(f = "summary",
          signature = c(object = "ClassificationFit_fSet"),
          definition = function(object, ...) {
              return( callNextMethod()$moClass )
            })
