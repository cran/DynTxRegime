# October 25, 2018

#' Class \code{TypedFit_fSet}
#'
#' Class \code{TypedFit_fSet} is a \code{TypedFit} when subsets are identified
#'   but not modeled independently.
#'
#' @name TypedFit_fSet-class
#'
#' @include C_TypedFit.R
#'
#' @keywords internal
setClass(Class = "TypedFit_fSet",
         contains = c("TxObj","TypedFit"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{TypedFit_fSet}
#'
#' Methods call equivalently named methods defined for \code{TypedFit}
#'   objects.
#'
#' @name TypedFit_fSet-methods
#'
#' @keywords internal
NULL

#' @rdname newTypedFit
setMethod(f = ".newTypedFit", 
          signature = c(modelObj = "modelObj",
                        txObj = "TxInfoWithSubsets"), 
          definition = function(modelObj,
                              txObj,
                              data,
                              response,
                              type,
                              suppress) {

              singles <- .getSingleton(object = txObj)

              if (all(singles) ) stop("no data provided")

              if (!suppress) {
                cat(sum(!singles), "included in analysis\n")
              }

              txNew <- .newTxObj(fSet = NULL,
                                 txName = .getTxName(object = txObj),
                                 data = data[!singles,],
                                 suppress = TRUE,
                                 verify = FALSE)

              fitObj <- .newTypedFit(modelObj = modelObj,
                                     txObj = txNew,
                                     data = data[!singles,],
                                     response = response[!singles],
                                     type = type,
                                     suppress = suppress)

              result <- new(Class = "TypedFit_fSet", txObj, fitObj)

              return( result )
            })


#' \code{coef(object)} 
#'   retrieves the estimated coefficients. 
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "coef",
          signature = c(object = "TypedFit_fSet"),
          definition = function(object, ...) {
              return( coef(object = as(object = object, Class = "TypedFit")) )
            })

#' \code{fitObject(object)}
#'   retrieves the regression objects.  
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "fitObject",
          signature = c(object = "TypedFit_fSet"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object, 
                                            Class = "TypedFit")) )
            })

#' \code{plot(x, ...)}
#'   calls plot method(s) for a regression object.
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "plot",
          signature = c(x = "TypedFit_fSet"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = as(object = x, Class = "TypedFit"), suppress, ...)
            })

#' \code{predict(object, ...)}
#'   calls predict method for the regression object. Patients with only 1
#'   tx option are NA.
#'
#' \code{predict(object, ...)}
#'   Patients with only 1 tx option are NA.
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "predict",
          signature = c(object = "TypedFit_fSet"),
          definition = function(object, newdata, ...) {

              if (!missing(newdata)) {
                txNew <- .newTxObj(fSet = .getSubsetRule(object = object@txInfo),
                                   txName = .getTxName(object = object@txInfo),
                                   data = newdata,
                                   suppress = TRUE,
                                   verify = FALSE)

                singles <- .getSingleton(object = txNew)

                pred <- predict(object = as(object = object, Class = "TypedFit"),
                                newdata = newdata[!singles,,drop=FALSE], ...)
              } else {
                singles <- .getSingleton(object = object@txInfo)

                pred <- predict(object = as(object = object, Class = "TypedFit"))
              }

              if (is.null(x = ncol(x = pred))) {
                pred <- matrix(data = pred, ncol = 1L)
              }

              vals <- matrix(data = NA, 
                             nrow = length(x = singles), 
                             ncol = ncol(x = pred))
              vals[!singles,] <- pred

              if (ncol(x = vals) == 1L) vals <- drop(x = vals)

              return( vals )

            })

#' \code{print(x)}
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "print",
          signature = c(x = "TypedFit_fSet"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "TypedFit"))
            })

#' \code{show(object)}
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "show",
          signature = c(object = "TypedFit_fSet"),
          definition = function(object) {
              show(object = as(object = object, Class = "TypedFit"))
            })

#' \code{summary(object)}
#'   calls summary method(s) for regression object. 
#'
#' @rdname TypedFit_fSet-methods
setMethod(f = "summary",
          signature = c(object = "TypedFit_fSet"),
          definition = function(object, ...) {
              return( summary(object = as(object = object,Class = "TypedFit")) )
            })
