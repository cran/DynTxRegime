# October 26, 2018

.validity_PropensityObj <- function(object) {

  # @propen must be NA, PropensitySimpleFit, PropensitySimpleFit_fSet, 
  # PropensitySimpleFit_SubsetList, or DecisionPointList
  if (!is(object = object@propen, class2 = "PropensityFit") &&
      !is(object = object@propen, class2 = "PropensityFit_fSet") &&
      !is(object = object@propen, class2 = "PropensityFit_SubsetList") &&
      !is(object = object@propen, class2 = "DecisionPointList") &&
      !is.na(x = object@propen)) {
    return( "incorrect object for @propen" )
  }

  # elements of @propen must be PropensitySimpleFit, PropensitySimpleFit_fSet, 
  # or PropensitySimpleFit_SubsetList
  if (is(object = object@propen, class2 = "DecisionPointList")) {
    for (i in 1L:length(x = object@propen)) {
      if (!is(object = object@propen[[ i ]], class2 = "PropensityFit") &&
          !is(object = object@propen[[ i ]], class2 = "PropensityFit_fSet") &&
          !is(object = object@propen[[ i ]], class2 = "PropensityFit_SubsetList")) {
        return( "incorrect object for @propen" )
      }
    }
  }

  return( TRUE )
}

#' Class \code{PropensityObj}
#'
#' Class \code{PropensityObj} groups Propensity regression results under a 
#'   common name.
#'
#' @name PropensityObj-class
#'
#' @slot Propensity ANY - expected to be \code{PropensitySimpleFit},
#'   \code{PropensitySimpleFit_fSet}, \code{PropensitySimpleFit_SubsetList}, 
#'   or \code{DecisionPointList}.
#'
#' @include F_PropensityFit.R F_PropensityFit_fSet.R
#'   F_PropensityFit_SubsetList.R 
#'
#' @keywords internal
setClass(Class = "PropensityObj",
         slots = c(propen = "ANY"),
         prototype = list(propen = NA),
         validity = .validity_PropensityObj)

##########
## GENERICS
##########

#' Create a new \code{PropensityObj} object
#'
#' Calls newPropensityFit and stores result in @propen.
#'
#' @name newPropensityObj
#'
#' @param moPropen A modeling object
#' @param txObj A TxObj object
#' @param ...  Any optional additional input.
#' 
#' @keywords internal
setGeneric(name = ".newPropensityObj",
           def = function(moPropen, txObj, data, suppress, ...) {
               standardGeneric(".newPropensityObj")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{PropensityObj}
#'
#' Most value objects returned are a list with one element 'propen'.
#' Methods dispatched and objects returned in the element 'propen'
#'   depend on class of @propen.
#' Exceptions are noted below.
#'
#' @name PropensityObj-methods
#'
#' @keywords internal
NULL

#' @rdname newPropensityObj
setMethod(f = ".newPropensityObj",
          signature = c(moPropen = "ANY",
                        txObj = "ANY"),
          definition = function(moPropen,  
                                txObj, 
                                data,  
                                suppress) {

              if (!suppress ) cat("\nPropensity for treatment regression.\n")

              return( new(Class = "PropensityObj",
                          propen = .newPropensityFit(moPropen = moPropen,
                                                     txObj = txObj,
                                                     data = data,
                                                     suppress = suppress)) )
            })


#' @rdname newPropensityObj
setMethod(f = ".newPropensityObj",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        txObj   = "TxInfoList"),
          definition = function(moPropen,
                                txObj,
                                data,
                                suppress) {

              if (!suppress ) cat("\nPropensity for treatment regression.\n")

              nDP <- length(x = moPropen)

              fitObj <- list()

              for (i in 1L:nDP) {

                if (!suppress && nDP > 1L) cat("Decision point", i, "\n")

                fitObj[[ i ]] <- .newPropensityFit(moPropen = moPropen[[ i ]],
                                                   txObj = txObj@txInfo[[ i ]],
                                                   data = data,
                                                   suppress = suppress)

              }

              result <- new(Class = "DecisionPointList", fitObj)

              result <- new(Class = "PropensityObj",
                            "propen" = result)

              return( result )
            })

#' @rdname PropensityObj-methods
setMethod(f = "coef",
          signature = c(object = "PropensityObj"),
          definition = function(object, ...) {
              if (is(object = object@propen, class2 = "DecisionPointList")) {
                return( list("propensity" = .cycleList(object = object@propen,
                                                       func = 'coef')) )
              } else {
                return( list("propensity" = coef(object = object@propen)) )
              }
            })

#' @rdname PropensityObj-methods
setMethod(f = "fitObject",
          signature = c(object = "PropensityObj"),
          definition = function(object, ...) {
              if (is(object = object@propen, class2 = "DecisionPointList")) {
                return( list("propensity" = .cycleList(object = object@propen,
                                                       func = 'fitObject', ...)) )
              } else {
                return( list("propensity" = fitObject(object = object@propen)) )
              }
            })

#' Plot regression result
#'
#' \code{plot(x)} concatenates 'Propensity' to the title if suppress = FALSE.
#'
#' @rdname PropensityObj-methods
setMethod(f = "plot",
          signature = c(x = "PropensityObj"),
          definition = function(x, suppress = FALSE, ...) {

              argList <- list(...)
              if (!suppress) {
                argList <- .titleIt(argList, "Propensity")
              }
              argList[[ "x" ]] <- x@propen
              argList[[ "suppress" ]] <- suppress

              do.call(what = plot, args = argList)
            })

#' Make Predictions for All Tx.
#'
#' \code{.predictAll(object, newdata)} does not return the overarching list
#'   structure, but only the contents of list[[ propen ]].
#'
#' @rdname PropensityObj-methods
setMethod(f = ".predictAll",
          signature = c(object = "PropensityObj",
                        newdata = "data.frame"),
          definition = function(object, newdata, ...) {
              if (is(object = object@propen, class2 = "DecisionPointList")) {
                stop("not allowed")
              } else {
                return( .predictAll(object = object@propen, 
                                    newdata = newdata, ...) )
              }
            })

#' Make Predictions
#'
#' \code{predict(object)} does not return the overarching list
#'   structure, but only the contents of list[[ propen ]].
#'
#' @rdname PropensityObj-methods
setMethod(f = "predict",
          signature = c(object = "PropensityObj"),
          definition = function(object, ...) {
              if (is(object = object@propen, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@propen,
                                   func = 'predict', ...) )
              } else {
                return( predict(object = object@propen, ...) )
              }
            })

#' @rdname PropensityObj-methods
setMethod(f = "print",
          signature = c(x = "PropensityObj"),
          definition = function(x, ...) {
              cat("Propensity Regression Analysis\n")
              print(x = x@propen, ...)
            })

#' Retrieve Regression Analysis
#'
#' \code{propen(object)} does not return the overarching list
#'   structure, but only the contents of list[[ propen ]].
#'
#' @rdname PropensityObj-methods
setMethod(f = "propen",
          signature = c(object = "PropensityObj"),
          definition = function(object, ...) {
              if (is(object = object@propen, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@propen, func = 'propen', ...) )
              } else {
                return( propen(object = object@propen) )
              }
            } )

#' @rdname PropensityObj-methods
setMethod(f = "show",
          signature = c(object = "PropensityObj"),
          definition = function(object) {
              cat("Propensity Regression Analysis\n")
              show(object = object@propen)
            })

#' @rdname PropensityObj-methods
setMethod(f = "summary",
          signature = c(object = "PropensityObj"),
          definition = function(object, ...) {
              if (is(object = object@propen, class2 = "DecisionPointList")) {
                return( list("propensity" = .cycleList(object = object@propen,
                                                       func = 'summary')) )
              } else {
                return( list("propensity" = summary(object = object@propen, ...)) )
              }
            })
