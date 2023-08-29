# October 25, 2018

.validity_TypedFitObj <- function(object) {

  # fit must be NA, TypedFit, TypedFit_fSet, TypedFit_SubsetList or 
  # DecisionPointList
  if (!is(object = object@fit, class2 = "TypedFit") &&
      !is(object = object@fit, class2 = "TypedFit_fSet") &&
      !is(object = object@fit, class2 = "TypedFit_SubsetList") &&
      !is(object = object@fit, class2 = "DecisionPointList") &&
      !is.na(x = object@fit)) {
    return( "incorrect object for @fit" )
  }

  # if fit is DecisionPointList, elements must TypedFit, TypedFit_fSet, or
  # TypedFit_SubsetList
  if (is(object = object@fit, class2 = "DecisionPointList")) {
    for (i in 1L:length(x = object@fit)) {
      if (!is(object = object@fit[[ i ]], class2 = "TypedFit") &&
          !is(object = object@fit[[ i ]], class2 = "TypedFit_fSet") &&
          !is(object = object@fit[[ i ]], class2 = "TypedFit_SubsetList")) {
        return( "incorrect object for @fit" )
      }
    }
  }

  return( TRUE )
}

#' Class \code{TypedFitObj}
#'
#' Class \code{TypedFit_SubsetList} allows for TypedFit based objects to be 
#' grouped under a common name.
#'
#' @slot fit ANY - expected to be \code{TypedFit}, \code{TypedFit_fSet}.
#'   \code{TypedFit_SubsetList} or \code{DecisionPointList} of these.
#'
#' @name TypedFitObj-class
#'
#' @keywords internal
#' @include C_TypedFit.R C_TypedFit_fSet.R C_TypedFit_SubsetList.R
setClass(Class = "TypedFitObj",
         slots = c(fit = "ANY"),
         prototype = list(fit = NA),
         validity = .validity_TypedFitObj)

##########
## GENERICS
##########

#' Create a new \code{TypedFitObj} object
#'
#' Call newTypedFit and stores result in @fit
#'
#' @rdname newTypedFitObj
#'
#' @param modelObj A modeling object
#' @param txObj A TxObj object
#' @param ...  Any optional additional input.
#' 
#' @keywords internal
setGeneric(name = ".newTypedFitObj", 
           def = function(modelObj, txObj, ...){
                   standardGeneric(".newTypedFitObj")
                 })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{TypedFitObj}
#'
#' Methods call equivalently named methods defined for \code{TypedFit},
#'   \code{TypedFit_fSet}, \code{TypedFit_SubsetList} or
#'   \code{DecisionPointList} objects.
#' The methods dispatched and objects returned depend on the class of @fit.
#'
#' @name TypedFitObj-methods
#'
#' @keywords internal
NULL

#' @rdname newTypedFitObj
setMethod(f = ".newTypedFitObj",
          signature = c(modelObj = "ANY",
                        txObj = "ANY"),
          definition = function(modelObj, 
                                txObj,
                                response,  
                                data,
                                type,
                                suppress){

              res <- .newTypedFit(modelObj = modelObj,
                                  txObj = txObj,
                                  response = response,
                                  data = data,
                                  type = type,
                                  suppress = suppress)

              return( new("TypedFitObj", "fit" = res) )

            })

#' \code{coef(object)} 
#'   retrieves the estimated coefficients for each regression. 
#'
#' @rdname TypedFitObj-methods
setMethod(f = "coef",
          signature = c(object = "TypedFitObj"),
          definition = function(object, ...){
              if (is(object = object@fit, class2 = "TypedFit") ||
                  is(object = object@fit, class2 = "TypedFit_fSet") ||
                  is(object = object@fit, class2 = "TypedFit_SubsetList")) {
                return( coef(object = object@fit) )
              } else if (is(object = object@fit, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@fit, func = 'coef') )
              } else {
                stop("not allowed")
              }
            })

#' \code{fitObject(object)}
#'   retrieves the regression objects.  
#'
#' @rdname TypedFitObj-methods
setMethod(f = "fitObject",
          signature = c(object = "TypedFitObj"),
          definition = function(object, ...){
              if (is(object = object@fit, class2 = "TypedFit") ||
                  is(object = object@fit, class2 = "TypedFit_fSet") ||
                  is(object = object@fit, class2 = "TypedFit_SubsetList")) {
                return( fitObject(object = object@fit) )
              } else if (is(object = object@fit, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@fit, func = 'fitObject', ...) )
              } else {
                stop("not allowed")
              }
            })

#' \code{plot(x, ...)}
#'   calls plot method(s) for regression object(s).
#'
#' @rdname TypedFitObj-methods
setMethod(f = "plot",
          signature = c(x = "TypedFitObj"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = x@fit, suppress = suppress, ...)
            })

#' \code{predict(object, ...)}
#'   calls predict method for the regression object(s).
#'
#' @rdname TypedFitObj-methods
setMethod(f = "predict",
          signature = c(object = "TypedFitObj"),
          definition = function(object, ...) {
              if (is(object = object@fit, class2 = "TypedFit") ||
                  is(object = object@fit, class2 = "TypedFit_fSet") ||
                  is(object = object@fit, class2 = "TypedFit_SubsetList")) {
                return( predict(object = object@fit, ...) )
              } else if (is(object = object@fit, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@fit, func = 'predict', ...) )
              } else {
                stop("not allowed")
              }
            })

#' \code{print(x)}
#'
#' @rdname TypedFitObj-methods
setMethod(f = "print",
          signature = c(x = "TypedFitObj"),
          definition = function(x, ...) { print(x = x@fit) })

#' \code{show(object)}
#'
#' @rdname TypedFitObj-methods
setMethod(f = "show",
          signature = c(object = "TypedFitObj"),
          definition = function(object) { show(object = object@fit) })

#' \code{summary(object)}
#'   calls summary method(s) for regression object(s). 
#'
#' @rdname TypedFitObj-methods
setMethod(f = "summary",
          signature = c(object = "TypedFitObj"),
          definition = function(object, ...){
              if (is(object = object@fit, class2 = "TypedFit") ||
                  is(object = object@fit, class2 = "TypedFit_fSet") ||
                  is(object = object@fit, class2 = "TypedFit_SubsetList")) {
                return( summary(object = object@fit, ...) )
              } else if (is(object = object@fit, class2 = "DecisionPointList")) {
                return( .cycleList(object = object@fit, func = 'summary', ...) )
              } else {
                stop("not allowed")
              }
            })
