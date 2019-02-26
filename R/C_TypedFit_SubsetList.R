# October 25, 2018

.validity_TypedFit_SubsetList <- function(object) {

  # each element must be an object of class TypedFit
  for (i in 1L:length(x = object@fits)) {
    if (!is(object = object@fits[[ i ]], class2 = "TypedFit")) {
      return( "all elements of TypedFit_SubsetList must be TypedFit" )
    }
  }

  return( TRUE )
}

#' Class \code{TypedFit_SubsetList}
#'
#' Class \code{TypedFit_SubsetList} is \code{SubsetList} of \code{TypedFit} 
#' used when subsets are identified and modeled independently.
#'
#' @name TypedFit_SubsetList-class
#'
#' @include C_TypedFit.R
#'
#' @keywords internal
setClass(Class = "TypedFit_SubsetList",
         slots = c(fits = "SubsetList"),
         contains = c("TxObj"),
         validity = .validity_TypedFit_SubsetList)

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{TypedFit_SubsetList}
#'
#' Methods call equivalently named methods defined for \code{TypedFit}
#'   objects. When a value object is returned, it is a list. The element
#'   names of the list are the subset names to which the result pertains.
#'
#' @name TypedFit_SubsetList-methods
#'
#' @keywords internal
NULL


#' @rdname newTypedFit
setMethod(f = ".newTypedFit", 
          signature = c(modelObj = "ModelObj_SubsetList",
                        txObj = "TxInfoWithSubsets"), 
          definition = function(modelObj,
                                txObj,
                                data,
                                response,
                                type,
                                suppress) {

            res <- list()

            nms <- names(x = modelObj)

            if (length(x = type) == 1L) {
              type <- rep(x = type, times = length(x = modelObj))
            }

            subsets <- .getSubsets(object = txObj)
            ptsSubset <- .getPtsSubset(object = txObj)

            for (i in 1L:length(x = modelObj)) {

              modelSubset <- unlist(x = strsplit(x = nms[i], split = ","))

              use4fit <- ptsSubset %in% modelSubset

              if (!any(use4fit)) {
                stop(paste("no observations match", modelSubset))
              }

              if (!suppress) {
                cat("Fitting models for ", paste(modelSubset, collapse=" ,"),
                    "using", sum(use4fit), "patient records.\n")
              }

              txNew <- .newTxObj(fSet = NULL,
                                 txName = .getTxName(object = txObj),
                                 data = data[use4fit,],
                                 suppress = TRUE,
                                 verify = FALSE)

              res[[ nms[i] ]] <- .newTypedFit(modelObj = modelObj[[ i ]],
                                              data = data[use4fit,],
                                              response = response[use4fit],
                                              type = type[i],
                                              txObj = txNew,
                                              suppress = suppress)

            }

            ssList <- new(Class = "SubsetList", res)

            return( new(Class = "TypedFit_SubsetList", txObj, fits = ssList) )

          })

#' \code{coef(object)} 
#'   retrieves the estimated coefficients for each subset regression. 
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "coef",
          signature = c(object = "TypedFit_SubsetList"),
          definition = function(object, ...) {
              return( .cycleList(object = object@fits, func = 'coef') )
            })

#' \code{fitObject(object)}
#'   retrieves the regression objects for each subset.  
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "fitObject",
          signature = c(object = "TypedFit_SubsetList"),
          definition = function(object, ...) {
              return( .cycleList(object = object@fits, func = 'fitObject') )
            })

#' \code{plot(x, ...)}
#'   calls plot method(s) for each regression object.
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "plot",
          signature = c(x = "TypedFit_SubsetList"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = x@fits, suppress = suppress, ...)
            })

#' \code{predict(object, ...)}
#'   calls predict method for the regression object(s). 
#'
#' \code{predict(object, ...)}
#'   Patients not in subset are NA.
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "predict",
          signature = c(object = "TypedFit_SubsetList"),
          definition = function(object, newdata, ...) {

              if (missing(x = newdata)) {

                pred <- .cycleList(object = object@fits, func = 'predict', ...)

                ptsSubset <- .getPtsSubset(object = object@txInfo)
                nms <- names(x = object@fits)
                res <- list()

                for (i in 1L:length(x = nms)) {
                  objName <- unlist(x = strsplit(x = nms[i], split = ","))
                  tst <- ptsSubset %in% objName
                  if (is.matrix(x = pred[[ i ]]) & ncol(x = pred[[ i ]]) > 1L) {
                    temp <- matrix(data = NA, 
                                   nrow = length(x = ptsSubset), 
                                   ncol = ncol(x = pred[[ i ]]))
                    temp[tst,] <- pred[[ i ]]
                  } else {
                    temp <- rep(x = NA, times = length(x = ptsSubset))
                    temp[tst] <- pred[[ i ]]
                  }
                  res[[ nms[i] ]] <- temp
                }
              } else {
                res <- list()

                txObj <- .newTxObj(fSet = .getSubsetRule(object = object@txInfo),
                                   txName = .getTxName(object = object@txInfo),
                                   data = newdata,
                                   suppress = TRUE,
                                   verify = FALSE)

                ptsSubset <- .getPtsSubset(object = txObj)
                nms <- names(x = object@fits)

                for (i in 1L:length(x = nms)) {
                  objName <- unlist(x = strsplit(x = nms[i], split = ","))
                  tst <- ptsSubset %in% objName
                  if (!any(tst) ) next

                  pred <- predict(object = object@fits[[ i ]], 
                                  newdata = newdata[tst,], ...)
                  if (is.matrix(x = pred) & ncol(x = pred) > 1L) {
                    temp <- matrix(data = NA, 
                                   nrow = length(x = ptsSubset), 
                                   ncol = ncol(x = pred))
                    temp[tst,] <- pred
                  } else {
                    temp <- rep(x = NA, times = length(x = ptsSubset))
                    temp[tst] <- pred
                  }
                  res[[ nms[i] ]] <- temp
                }
              }

              return( res )
            })

#' \code{print(x)}
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "print",
          signature = c(x = "TypedFit_SubsetList"),
          definition = function(x, ...) { print(x = x@fits, ...) })

#' \code{show(object)}
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "show",
          signature = c(object = "TypedFit_SubsetList"),
          definition = function(object) { show(object = object@fits) })

#' \code{summary(object)}
#'   calls summary method(s) for each regression object. 
#'
#' @rdname TypedFit_SubsetList-methods
setMethod(f = "summary",
          signature = c(object = "TypedFit_SubsetList"),
          definition = function(object, ...) {
              return( .cycleList(object = object@fits, func = 'summary', ...) )
            })
