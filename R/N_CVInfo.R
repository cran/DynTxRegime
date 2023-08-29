# October 26, 2018

#' Class \code{CVInfo}
#'
#' Class \code{CVInfo} holds cross-validation procedure results
#'
#' @name CVInfo-class
#'
#' @slot value Values obtained for each parameter combination
#' @slot params list of parameter values considered
#' @slot optimal list of optimal parameter values
#'
#' @keywords internal
#'
#' @include N_CVBasic.R
setClass(Class = "CVInfo",
         slots = c(value   = "ANY",
                   params  = "list",
                   optimal = "list"))

#' Create a CVInfo Object
#'
#' Dispatch appropriate cross-validation procedure.
#'
#' @rdname newCVInfo
#'
#' @param lambdas tuning parameters
#' @param kernel kernel object
#'
#' @keywords internal
setGeneric(name = ".newCVInfo",
           def = function(lambdas, kernel, ...) {
                   standardGeneric(f = ".newCVInfo")
                 })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getValue",
           def = function(object) { standardGeneric(f = ".getValue") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getOptimal",
           def = function(object) { standardGeneric(f = ".getOptimal") })

#' Extract Cross-Validation Results
#'
#' Extract cross-validation results from the value object returned by a 
#'   weighted learning statistical method of DynTxRegime.
#'
#' Methods are developed for all weighted learning methods implemented in
#'  DynTxRegime. Specifically, OWL, RWL, BOWL, and EARL.
#'
#' @param object A value object returned by a weighted learning statistical 
#'   method of DynTxRegime
#' @param ... Ignored.
#'
#' @usage
#' 
#' cvInfo(object, ...)
#'
#' @name cvInfo
#'
#' @exportMethod cvInfo
setGeneric(name = "cvInfo",
           def = function(object, ...) { standardGeneric(f = "cvInfo") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{CVInfo}
#'
#' @name CVInfo-methods
#'
#' @keywords internal
NULL


#' @rdname newCVInfo
setMethod(f = ".newCVInfo",
          signature = c(lambdas = "ANY",
                        kernel = "ANY"),
          definition = function(lambdas, kernel, ...) { stop("not allowed") })

#' \code{.getPars}
#'   retrieves parameters considered in cross-validation.
#'
#' @rdname CVInfo-methods
setMethod(f = ".getPars",
          signature = c(object = "CVInfo"),
          definition = function(object) { return(object@params) })

#' \code{.getOptimal}
#'   retrieves optimal parameters identified in cross-validation.
#'
#' @rdname CVInfo-methods
setMethod(f = ".getOptimal",
          signature = c(object = "CVInfo"),
          definition = function(object) { return(object@optimal) })

#' \code{.getValue}
#'   retrieves values obtained in cross-validation.
#'
#' @rdname CVInfo-methods
setMethod(f = ".getValue",
          signature = c(object = "CVInfo"),
          definition = function(object) { return( object@value ) })

#' \code{cvInfo}
#'   retrieves cross-validation information.
#'
#' @rdname CVInfo-methods
setMethod(f = "cvInfo",
          signature = c(object = "CVInfo"),
          definition = function(object) { return( object@value ) })

#' \code{print}
#'   print cross-validation results.
#'
#' @rdname CVInfo-methods
setMethod(f = "print",
          signature = c(x = "CVInfo"),
          definition = function(x, ...) { print(x = x@value, ...) })

#' \code{show}
#'   display cross-validation results.
#'
#' @rdname CVInfo-methods
setMethod(f = "show",
          signature = c(object = "CVInfo"),
          definition = function(object) { show(object@value) })

#' \code{summary}
#'   summarize cross-validation results.
#'
#' @rdname CVInfo-methods
setMethod(f = "summary",
          signature = c(object = "CVInfo"),
          definition = function(object, ...) { return( object@value ) })
