# October 26, 2018

#' Class \code{Regime}
#'
#' Class \code{Regime} holds information regarding regimes communicated 
#'   through functions.
#'
#' @name Regime-class
#'
#' @slot nVars An integer. The number of parameters to be estimated
#' @slot vNames A character. The names of the parameters to be estimated
#' @slot func A function. The user specified function that defines the regime
#' @slot pars A numeric. The estimated parameters
#'
#' @keywords internal
setClass("Regime",
         slots = c( nVars = "integer",
                   vNames = "character",
                     func = "function",
                     pars = "numeric"))   

##########
## GENERICS
##########

#' Create a new \code{Regime} object
#'
#' @rdname newRegime
#'
#' @param object A function defining the treatment regime
#'
#' @keywords internal
setGeneric(name = ".newRegime", 
           def = function(object) { standardGeneric(f = ".newRegime") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getNumPars", 
           def = function(object) { standardGeneric(f = ".getNumPars") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getParNames", 
           def = function(object) { standardGeneric(f = ".getParNames") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getPars", 
           def = function(object) { standardGeneric(f = ".getPars") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getRegimeFunction", 
           def = function(object) { standardGeneric(f = ".getRegimeFunction") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".predictOptimalTx",
           def = function(x, newdata, ...) {
               standardGeneric(f = ".predictOptimalTx")
             })

#' Extract Regime Parameters
#'
#' Extract the estimated regime parameters.
#'
#' Methods are defined for all statistical methods implemented in DynTxRegime
#'   that use a non-regression based regime. Specifically, OptimalSeq, OWL,
#'   BOWL, RWL, and EARL.
#' 
#' @name regimeCoef
#'
#' @param object A value object returned by a statistical method of DynTxRegime.
#' @param ... Ignored.
#'
#' @usage
#' regimeCoef(object, ...)
#'
#' @exportMethod regimeCoef
setGeneric(name = "regimeCoef", 
           def = function(object, ...) { standardGeneric(f = "regimeCoef") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".setPars", 
           def = function(object, pars) { standardGeneric(f = ".setPars") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{Regime}
#'
#' @name Regime-methods
#'
#' @keywords internal
NULL

#' \code{.getNumPars}
#'   retrieves the number of parameters in the regime to be estimated.
#'
#' @rdname Regime-methods
setMethod(f = ".getNumPars", 
          signature = c(object = "Regime"), 
          definition = function(object) { return( object@nVars ) })

#' \code{.getParNames}
#'   retrieves the parameter names in the regime.
#'
#' @rdname Regime-methods
setMethod(f = ".getParNames",  
          signature = c(object = "Regime"), 
          definition = function(object) { return( object@vNames ) })

#' \code{.getPars}
#'   retrieves current estimates for regime parameters.
#'
#' @rdname Regime-methods
setMethod(f = ".getPars",  
          signature = c(object = "Regime"), 
          definition = function(object) { 
              res <- object@pars
              names(x = res) <- object@vNames
              return( res ) 
            })

#' \code{.getRegimeFunction}
#'   retrieves the user specified function definition of the regime.
#'
#' @rdname Regime-methods
setMethod(f = ".getRegimeFunction",   
          signature = c(object = "Regime"), 
          definition = function(object) { return( object@func ) })

#' @rdname newRegime
setMethod(f = ".newRegime",   
          signature = c(object = "function"), 
          definition = function(object) { 

              # retrieve the formal arguments of user provided function
              nms <- names(x = formals(fun = object))

              # function must include data as an input argument, thus number
              # of variables is 1 less than the number of formal args.
              nVars <- length(x = nms) - 1L

              # if only data is provided, or data is not included in function
              # definition, throw error
              if (nVars <= 0L || !('data' %in% nms)) {
                stop(paste("formal arguments of function input through regimes",
                           "must contain all regime parameters and 'data'"))
              }

              tst <- nms %in% c('data')
              nms <- nms[!tst]

              obj <- new("Regime",
                         "nVars"  = nVars,
                         "vNames" = nms,
                         "func"   = object,
                         "pars"   = numeric(length = nVars))

              return( obj )
            })

#' \code{.predictOptimalTx}
#'   executes user specified function using current estimated parameters and
#'   provided data to determine recommended tx.
#'
#' @rdname Regime-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "Regime", 
                        newdata = "data.frame"),
          definition = function (x, newdata, ...) {

              argList <- list()
              for (j in 1L:x@nVars) {
                argList[[ x@vNames[j] ]] <- x@pars[j]
              }
              argList[[ 'data' ]] <- newdata

              reg.g <- do.call(what = x@func, args = argList)

              return( drop(x = reg.g) )
            })

#' \code{print}
#'   prints the current estimates for the regime parameters.
#'
#' @rdname Regime-methods
setMethod(f = "print",
          signature = c(x = "Regime"),
          definition = function(x, ...) { print(x = .getPars(object = x)) })

#' \code{regimeCoef}
#'   retrieves the current estimates for the regime parameters
#'
#' @rdname Regime-methods
setMethod(f = "regimeCoef",    
          signature = c(object = "Regime"), 
          definition = function(object, ...) { 
              return( .getPars(object = object) )
            })

#' \code{.setPars}
#'   sets the parameter estimates to the provided values.
#'
#' @rdname Regime-methods
setMethod(f = ".setPars",  
          signature = c(object = "Regime",
                        pars = "numeric"), 
          definition = function(object, pars) { 
              if (length(x = pars) != object@nVars) stop("verify pars")
              object@pars <- pars
              return( object ) 
            })

#' \code{show}
#'   displays the current estimates for the regime parameters.
#'
#' @rdname Regime-methods
setMethod(f = "show",
          signature = c(object = "Regime"),
          definition = function(object) {
              show(object = .getPars(object = object))
            })

#' \code{summary}
#'   retrieves the current estimates for the regime parameters
#'
#' @rdname Regime-methods
setMethod(f = "summary",
          signature = c(object = "Regime"),
          definition = function(object, ...) {
              return( .getPars(object = object) )
            })
