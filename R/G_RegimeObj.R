# October 26, 2018

.validity_RegimeObj <- function(object) {

  # @regime must be a Regime or a DecisionPointList
  if (!is(object = object@regime, class2 = "Regime") &&
      !is(object = object@regime, class2 = "DecisionPointList") &&
      !is.na(x = object@regime)) {
    return( "incorrect object for @regime" )
  }

  # if @regime is a DecisionPointList, each element must be a Regime
  if (is(object = object@regime, class2 = "DecisionPointList")) {
    for (i in 1L:length(x = object@regime)) {
      if (!is(object = object@regime[[ i ]], class2 = "Regime")) {
        return( "incorrect object for @regime" )
      }
    }
  }

  return( TRUE )
}

#' Class \code{RegimeObj}
#'
#' Class \code{RegimeObj} holds information regarding regimes communicated 
#'   through functions under a common name. 
#'
#' @name RegimeObj-class
#'
#' @slot regime ANY expected to be \code{Regime} or \code{DecisionPointList}
#'
#' @keywords internal
#'
#' @include G_Regime.R
setClass(Class = "RegimeObj",
         slots = c(regime = "ANY"),
         prototype = list(regime = NA),
         validity = .validity_RegimeObj)

##########
## GENERICS
##########

#' Create a New \code{RegimeObj} Object
#'
#' Calls newRegime and stores object in @regime.
#'
#' @rdname newRegimeObj
#'
#' @param object A function defining the treatment regime
#'
#' @keywords internal
setGeneric(name = ".newRegimeObj",
           def = function(object) { standardGeneric(f = ".newRegimeObj") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{RegimeObj}
#'
#' Methods dispatch equivalantly named functions defined for Regime or
#'   DecisionPointList objects. Method dispatched dictated by object stored
#'   in @regime.
#'
#' @name RegimeObj-methods
#'
#' @keywords internal
NULL

#' @rdname newRegimeObj
setMethod(f = ".newRegimeObj",
          signature = c(object = "function"),
          definition = function(object) {
              return( new(Class = "RegimeObj",
                          regime = .newRegime(object = object)) )
            })


#' @rdname newRegimeObj
setMethod(f = ".newRegimeObj",   
          signature = c(object = "list"), 
          definition = function(object) { 

              nDP <- length(x = object)

              obj <- list()

              for (i in 1L:nDP) {
                if (!is.function(x = object[[ i ]])) {
                  stop("each regime must be a function")
                }
                obj[[ i ]] <- .newRegime(object = object[[ i ]])
              }

              obj <- new(Class = "DecisionPointList", obj)

              obj <- new(Class = "RegimeObj", regime = obj)

              return( obj )
            })

#' @rdname RegimeObj-methods
setMethod(f = ".getNumPars",   
          signature = c(object="RegimeObj"), 
          definition = function(object) { 
              if (is(object = object@regime, class2 = "Regime")) {
                return( .getNumPars(object = object@regime) ) 
              } else {
                pars <- .cycleList(object = object@regime, func = '.getNumPars')
                return( sum(unlist(x = pars)) )
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = ".getParNames",  
          signature = c(object = "RegimeObj"), 
          definition = function(object) { 
              if (is(object = object@regime, class2 = "Regime")) {
                return( .getParNames(object = object@regime) ) 
              } else {
                return( .cycleList(object = object@regime, 
                                   func = '.getParNames') )
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = ".getPars",  
          signature = c(object = "RegimeObj"), 
          definition = function(object) { 
              if (is(object = object@regime, class2 = "Regime")) {
                return( .getPars(object = object@regime) ) 
              } else {
                return( .cycleList(object = object@regime, func = '.getPars') )
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = ".getRegimeFunction",   
          signature = c(object = "RegimeObj"), 
          definition = function(object) { 
              if (is(object = object@regime, class2 = "Regime")) {
                return( .getRegimeFunction(object = object@regime) ) 
              } else {
                return( .cycleList(object = object@regime, 
                                   func = '.getRegimeFunction') )
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "RegimeObj", 
                        newdata = "data.frame"),
          definition = function (x, newdata, dp = 1L, ...) {
              if (is(object = x@regime, class2 = "Regime")) {
                return( .predictOptimalTx(x = x@regime, newdata = newdata, ...) ) 
              } else {
                if (dp > length(x = x@regime) || dp <= 0L) stop("invalid dp")
                return( .predictOptimalTx(x = x@regime[[ dp ]], 
                                          newdata = newdata, ...) )
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = "print",
          signature = c(x = "RegimeObj"),
          definition = function (x, ...) {
              cat("Regime\n")
              print(x = x@regime, ...) 
            })

#' @rdname RegimeObj-methods
setMethod(f = "regimeCoef",    
          signature = c(object = "RegimeObj"), 
          definition = function(object, ...) {
              if (is(object = object@regime, class2 = "Regime")) {
                return( regimeCoef(object = object@regime) ) 
              } else {
                return( .cycleList(object = object@regime, 
                                   func = 'regimeCoef') )
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = ".setPars",  
          signature = c(object = "RegimeObj",
                        pars = "numeric"), 
          definition = function(object, pars) { 
              if (length(x = pars) != .getNumPars(object)) stop("verify pars")
              if (is(object = object@regime, class2 = "Regime")) {
                object@regime <- .setPars(object = object@regime, pars = pars)
                return( object ) 
              } else {
                ni <- 1L
                for (i in 1L:length(x = object@regime)) {
                  nf <- ni + .getNumPars(object = object@regime[[ i ]]) - 1L
                  object@regime[[ i ]] <- .setPars(object = object@regime[[ i ]],
                                                   pars = pars[ni:nf])
                  ni <- nf + 1L
                }
                return( object ) 
              }
            })

#' @rdname RegimeObj-methods
setMethod(f = "show",
          signature = c(object = "RegimeObj"),
          definition = function (object) {
              cat("Regime\n")
              show(object = object@regime) 
            })

#' @rdname RegimeObj-methods
setMethod(f = "summary",
          signature = c(object = "RegimeObj"),
          definition = function (object, ...) {
              if (is(object = object@regime, class2 = "Regime")) {
                return( list("regime" = summary(object = object@regime, ...)) ) 
              } else {
                return( list("regime" = .cycleList(object = object@regime, 
                                                   func = 'summary', ...)) )
              }
            })
