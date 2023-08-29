# October 26, 2018

.validity_CVInfoObj <- function(object) {

  # @cvInfo must be CVInfo or NA
  if (!is(object = object@cvInfo, class2 = "CVInfo") &&
      !is.na(x = object@cvInfo)) {
    return( "incorrect object for @cvInfo" )
  }

  return( TRUE )
}

#' Class \code{CVInfoObj}
#'
#' Class \code{CVInfoObj} holds information regarding cross-validation
#'   procedure under a common name.
#'
#' @name CVInfoObj-class
#'
#' @slot cvInfo ANY expected to be CVInfo or NULL
#'
#' @keywords internal
#'
#' @include N_CVInfo.R N_CVBasic.R
#' @include N_CVInfo2Par.R N_CVInfokParam.R N_CVInfoLambda.R
#'
setClass(Class = "CVInfoObj",
         slots = c(cvInfo = "ANY"),
         prototype = list(cvInfo = NA),
         validity = .validity_CVInfoObj)

##########
## GENERICS
##########

#' Create a New CVInfoObj Object
#'
#' Call newCVInfo and stores result in @cvInfo
#'
#' @param lambdas Tuning parameters to be considered
#' @param kernel Kernel (w/kernel parameters) to be considered
#' @param ... Additional arguments as needed
#'
#' @rdname newCVInfoObj
#'
#' @keywords internal
setGeneric(name = ".newCVInfoObj",
           def = function(lambdas, kernel, ...) {
               standardGeneric(f = ".newCVInfoObj")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{CVInfoObj}
#'
#' Call methods equivalently named for object inheriting from CVInfo.
#' Methods dispached depend on object in @cvInfo.
#'
#' @name CVInfoObj-methods
#'
#' @keywords internal
NULL

#' @rdname newCVInfoObj
#' @param methodObject Object parameters for weighted learning method
#' @param cvObject Cross-Validation object
#' @param suppress T/F indicating if screen prints are generated
setMethod(f = ".newCVInfoObj",
          signature = c(lambdas = "ANY",
                        kernel = "Kernel"),
          definition = function(lambdas,  
                                kernel, 
                                methodObject, 
                                cvObject,  
                                suppress, ...) {
              return( new(Class = "CVInfoObj",
                          cvInfo = .newCVInfo(lambdas = lambdas,
                                              kernel = kernel,
                                              methodObject = methodObject,
                                              cvObject = cvObject,
                                              suppress = suppress, ...)) )
            })

#' \code{.getPars}
#'   retrieves parameters considered in cross-validation.
#'
#' @rdname CVInfoObj-methods
setMethod(f = ".getPars",
          signature = c(object = "CVInfoObj"),
          definition = function(object) {
              return( .getPars(object = object@cvInfo) )
            })

#' \code{.getOptimal}
#'   retrieves optimal parameters identified in cross-validation.
#'
#' @rdname CVInfoObj-methods
setMethod(f = ".getOptimal",
          signature = c(object = "CVInfoObj"),
          definition = function(object) {
              return( .getOptimal(object = object@cvInfo) )
            })

#' \code{.getValue}
#'   retrieves values obtained in cross-validation.
#'
#' @rdname CVInfoObj-methods
setMethod(f = ".getValue",
          signature = c(object = "CVInfoObj"),
          definition = function(object) {
              return( .getValue(object = object@cvInfo) )
            })

#' \code{cvInfo}
#'   retrieves cross-validation information.
#'
#' @rdname CVInfoObj-methods
setMethod(f = "cvInfo",
          signature = c(object = "CVInfoObj"),
          definition = function(object) {
              if (is(object = object@cvInfo, class2 = "CVInfo")) {
                return( list("cvInfo" = cvInfo(object = object@cvInfo)) )
              }
            })
#' \code{print}
#'   print cross-validation results.
#'
#' @rdname CVInfoObj-methods
setMethod(f = "print",
          signature = c(x = "CVInfoObj"),
          definition = function(x, ...) {
              if (is(object = x@cvInfo, class2 = "CVInfo")) {
                cat("\nCross Validation\n")
                print(x = x@cvInfo, ...)
              }
            })

#' \code{show}
#'   display cross-validation results.
#'
#' @rdname CVInfoObj-methods
setMethod(f = "show",
          signature = c(object = "CVInfoObj"),
          definition = function(object) {
              if (is(object = object@cvInfo, class2 = "CVInfo")) {
                cat("\nCross Validation\n")
                show(object = object@cvInfo)
              }
            })

#' \code{summary}
#'   summarize cross-validation results.
#'
#' @rdname CVInfoObj-methods
setMethod(f = "summary",
          signature = c(object = "CVInfoObj"),
          definition = function(object, ...) {
              if (is(object = object@cvInfo, class2 = "CVInfo")) {
                return( list("cvInfo" = summary(object = object@cvInfo, ...)) )
              }
            })
