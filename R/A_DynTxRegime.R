# October 23, 2018

setClassUnion("CallOrNull",
              members = c("call","NULL"))

#' Class \code{DynTxRegime}
#'
#' Class \code{DynTxRegime} is a component of all statistical methods 
#' implemented in the package. This class contains the estimated optimal Tx, 
#' decision functions if applicable, the estimated value and the original 
#' unevaluated call. It extends internal class \code{OptimalObj}.
#'
#' @name DynTxRegime-class
#'
#' @slot call object of class call or NULL
#'
#' @include A_OptimalObj.R
#'
#' @keywords internal
setClass(Class = "DynTxRegime",
         slots = c(call = "CallOrNull"),
         contains = c("OptimalObj"),
         prototype = list(call = NULL))

##########
# GENERICS
##########

#' Retrieve Unevaluated Original Call
#'
#' Returns the unevaluated original call to a DynTxRegime statistical method.
#'
#' Methods are defined for all statistical methods implemented in DynTxRegime.
#'
#' @name Call
#' @exportMethod Call
#'
#' @param name Object for which call is desired
#' @param ...  Optional additional input required by R's base call().
#'
#' @usage
#'   Call(name, ...)
#'
setGeneric(name = "Call",
           def = function(name, ...){ standardGeneric(f = "Call") })

##########
# METHODS
##########

#' Methods Available for Objects of Class \code{DynTxRegime}
#'
#' @name DynTxRegime-methods
#'
#' @keywords internal
NULL

#' \code{Call(name)}
#'   retrieves the unevaluated call to the original statistical method
#'
#' @rdname DynTxRegime-methods
setMethod(f = "Call",
          signature = c(name = "DynTxRegime"),
          definition = function(name, ...) { return( name@call ) })
