# October 26, 2018

.validity_OptimObj <- function(object) {

  # @optim must be NA, OptimBasic, or OptimKernel
  if (!is(object = object@optim, class2 = "OptimBasic") &&
      !is(object = object@optim, class2 = "OptimKernel") &&
      !is.na(x = object@optim)) {
    return( "incorrect object for @optim" )
  }

  return( TRUE )
}

#' Class \code{OptimObj}
#'
#' Class \code{OptimObj} stores the optimization results under a common name
#'   for weighted learning methods.
#'
#' @name OptimObj-class
#'
#' @slot optim ANY - expected to be \code{OptimBasic} or \code{OptimKernel}
#'
#' @keywords internal
#'
#' @include M_OptimBasic.R M_OptimKernel.R
setClass(Class = "OptimObj",
         slots = c(optim = "ANY"),
         prototype = list(optim = NA),
         validity = .validity_OptimObj)

##########
## GENERICS
##########

#' Create an OptimObj Object
#'
#' Call newOptim and stores result under common name
#'
#' @name newOptimObj
#'
#' @keywords internal
setGeneric(name = ".newOptimObj",
           def = function(methodObject, kernel, ...) {
               standardGeneric(f = ".newOptimObj")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OptimObj}
#'
#' @name OptimObj-methods
#'
#' @keywords internal
NULL


#' @param methodObject object containing parameters needed by a weighted 
#'   learning method
#' @param lambda tuning parameters
#' @param suppress integer indicating screen print preferences
#' @param ... additional inputs passed to optimization routine.
#'
#' @rdname newOptimObj
setMethod(f = ".newOptimObj",
          signature = c(methodObject = "ANY"),
          definition = function(methodObject, 
          	                lambda,
          	                suppress, ...) {

              return( new(Class = "OptimObj",
                          optim = .newOptim(kernel = methodObject@kernel,
                                            lambda = lambda,
                                            methodObject = methodObject,
                                            suppress = suppress, ...)) )
            })

#' @rdname OptimObj-methods
setMethod(f = "optimObj",
          signature = c(object = "OptimObj"),
          definition = function(object, ...) { 
              return( optimObj(object = object@optim ) ) 
            })

#' @rdname OptimObj-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimObj",
                        newdata = "matrix"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = x@optim, newdata = newdata) )
            })

#' @rdname OptimObj-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimObj",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = x@optim, newdata = newdata) )
             })

#' @rdname OptimObj-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimObj",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = x@optim) )
            })

#' @rdname OptimObj-methods
setMethod(f = "print",
          signature = c(x = "OptimObj"),
          definition = function(x, ...) {
              cat("Optimization Results\n")
              print(x = x@optim, ...)
            })

#' @rdname OptimObj-methods
setMethod(f = "regimeCoef",
          signature = c(object = "OptimObj"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@optim) )
            })

#' @rdname OptimObj-methods
setMethod(f = "show",
          signature = c(object = "OptimObj"),
          definition = function(object) {
              cat("Optimization Results\n")
              show(object = object@optim)
            })

#' @rdname OptimObj-methods
setMethod(f = "summary",
          signature = c(object = "OptimObj"),
          definition = function(object, ...) {
              return( list("optim" = summary(object = object@optim, ...)) )
            })
