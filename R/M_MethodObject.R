# November 28, 2018

#' Class \code{MethodObject}
#'
#' Class \code{MethodObject} stores parameters required for optimization step
#'
#' @name MethodObject-class
#'
#' @keywords internal
#'
#' @slot x Matrix of covariates for kernel
#' @slot surrogate The Surrogate for the loss-function
#' @slot pars Vector of regime parameters
#' @slot kernel The Kernel defining the decision function
setClass(Class = 'MethodObject',
         slots = c(x = "matrix",
                   surrogate = "Surrogate",
                   pars = "ANY",
                   kernel = "Kernel"))

##########
## GENERICS
##########

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".dobjFn",
           def = function(par, methodObject, kernel, ...) {
               standardGeneric(".dobjFn")
             })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".objFn",
           def = function(par, methodObject, kernel, ...) {
               standardGeneric(".objFn")
             })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".subsetObject",
           def = function(methodObject, ...) { 
               standardGeneric(".subsetObject") 
             })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".valueFunc",
           def = function(methodObject, ...) { standardGeneric(".valueFunc") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".optimFunc",
           def = function(methodObject, ...) { standardGeneric(".optimFunc") })


##########
## METHODS
##########
#' Methods Available for Objects of Class \code{MethodObject}
#'
#' @name MethodObject-methods
#'
#' @keywords internal
NULL


#' @rdname MethodObject-methods
setMethod(f = ".subsetObject",
          signature = c(methodObject = "MethodObject"),
          definition = function(methodObject, subset) {

              methodObject@kernel@X <- methodObject@kernel@X[subset,,drop=FALSE]

              return( new("MethodObject",
                          "x"         = methodObject@x[subset,,drop=FALSE],
                          "surrogate" = methodObject@surrogate,
                          "pars"      = methodObject@pars,
                          "kernel"    = methodObject@kernel) )
            })


#' @rdname MethodObject-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = "MethodObject",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' @rdname MethodObject-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = "MethodObject",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' @rdname MethodObject-methods
setMethod(f = ".valueFunc",
          signature = c("methodObject" = "MethodObject"),
          definition = function(methodObject, optTx, ...) { stop("not allowed") })

#' Create method object
#'
#' @param kernel Kernel object
#' @param surrogate Surrogate object indicating surrogate loss-function
#' @param guess Vector of estimated regime parameters
#'
#' @return A MethodObject object
#'
#' @rdname MethodObject-methods
#'
#' @keywords internal
.createMethodObject <- function(kernel, 
                                surrogate,  
                                guess = NULL, ...) {

  if (is.null(x = guess)) {
    if (is(object = kernel, class2 = "LinearKernel")) {
      guess <- rep(x = 1e-6, times = {ncol(x = kernel@X)+1})
    } else {
      guess <- rep(x = 1e-6, times = {nrow(x = kernel@X)+1})
    }
  }

  return( new(Class = "MethodObject",
              "x"         = kernel@X,
              "surrogate" = surrogate,
              "pars"      = guess,
              "kernel"    = kernel) )

}

setMethod(f = ".optimFunc",
          signature = c("methodObject" = "MethodObject"),
          definition = function(methodObject, 
                                par,
                                lambda,
                                suppress, ...) {

            return( .optim(surrogate = methodObject@surrogate,
                           par = par,
                           lambda = lambda,
                           fn = .objFn,
                           gr = .dobjFn,
                           suppress = suppress, 
                           methodObject = methodObject, 
                           kernel = methodObject@kernel, ...) )

          })
