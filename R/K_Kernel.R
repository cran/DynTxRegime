# October 26, 2018

#' Class \code{Kernel}
#'
#' Class \code{Kernel} holds information regarding the decision function kernel
#'
#' @name Kernel-class
#'
#' @slot model An formula. Defines the covariates of the kernel.
#' @slot X A matrix. The covariates of the kernel
#' @slot kparam ANY. The kernel parameter
#'
#' @keywords internal
setClass(Class = "Kernel",
         slots = c("model" = "formula",
                   "X" = "matrix",
                   "kparam" = "ANY"),
         prototype = prototype(model = ~1, X = matrix(), kparam = NULL) )

#' @rdname DynTxRegime-internal-api
#' @importFrom stats terms model.matrix update.formula
setMethod(f = "initialize",
          signature = c(.Object = "Kernel"),
          definition = function(.Object, data, model, kparam, ...) {

              if (!missing(model)) {
                .Object@model <- model
                if (attr(stats::terms(.Object@model),"intercept") == 1L) {
                  .Object@model <- stats::update.formula(.Object@model, ~.-1)
                }
              }
              if (!missing(data)) {
                .Object@X <- tryCatch(expr = stats::model.matrix(object = .Object@model,
                                                                 data = data),
                                      error = function(e) {
                                          print(x = e$message)
                                          stop("unable to identify needed variables in data")
                                          return(e)
                                        })

                attr(.Object@X, "assign") <- NULL

                if (nrow(x = data) != nrow(x = .Object@X)) stop("missing data")

              }
              if (!missing(kparam)) .Object@kparam <- kparam

              return( .Object )
            })

##########
## GENERICS
##########

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getKernelX",
           def = function(data, object, ...) { 
               standardGeneric(f = ".getKernelX") 
             })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".kernel",
           def = function(object, x1, x2, ...) { 
               standardGeneric(f = ".kernel") 
             })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".kernelNumPars",
           def = function(object, ...) { 
               standardGeneric(f = ".kernelNumPars") 
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{Kernel}
#'
#' @name Kernel-methods
#'
#' @keywords internal
NULL

#' \code{.getKernelX}
#'   retrieves the covariates matrix of the kernel.
#'
#' @rdname Kernel-methods
#' @importFrom stats model.matrix
setMethod(f = ".getKernelX",
          signature = c(data = "data.frame",
                        object = "Kernel"),
          definition = function(data, object) {
              x <- tryCatch(expr = stats::model.matrix(object = object@model,
                                                       data = data),
                            error = function(e) {
                                print(x = e$message)
                                stop("unable to identify needed variables in newdata")
                                return( e )
                              })

              attr(x, "assign") <- NULL
              if (nrow(x = data) != nrow(x = x)) stop("missing data")

              return( x )
            })

#' \code{.kernelNumPars}
#'   retrieves the number of covariates of the kernel.
#'
#' @rdname Kernel-methods
setMethod(f = ".kernelNumPars",
          signature = c(object = "Kernel"),
          definition = function(object, ...) { 
              return( nrow(x = object@X) + 1L ) 
            })

#' \code{.kernel}
#'   calculates the kernel
#'
#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "missing",
                        x2 = "missing"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object, x1 = object@X, x2 = object@X) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "data.frame",
                        x2 = "data.frame"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = .getKernelX(data = x1, object = object),
                              x2 = .getKernelX(data = x2, object = object), ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "vector",
                        x2 = "vector"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = matrix(data = x1, ncol = 1L),
                              x2 = matrix(data = x2, ncol = 1L), ...) )
            })


#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "vector",
                        x2 = "data.frame"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = matrix(data = x1, ncol = 1L),
                              x2 = .getKernelX(data = x2, object = object), ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "data.frame",
                        x2 = "vector"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = .getKernelX(data = x1, object = object),
                              x2 = matrix(data = x2, ncol = 1L), ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "matrix",
                        x2 = "data.frame"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = x1,
                              x2 = .getKernelX(data = x2, object = object), ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "data.frame",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = .getKernelX(data = x1, object = object),
                              x2 = x2, ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "vector",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = matrix(data = x1, ncol = 1L),
                              x2 = x2, ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "matrix",
                        x2 = "vector"),
          definition = function(object, x1, x2, ...) {
              return( .kernel(object = object,
                              x1 = x1,
                              x2 = matrix(data = x2, ncol = 1L), ...) )
            })

#' @rdname Kernel-methods
setMethod(f = ".kernel",
          signature = c(object = "Kernel",
                        x1 = "matrix",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) { stop("not allowed") })

#' \code{print}
#'   prints kernel model.
#'
#' @rdname Kernel-methods
setMethod(f = "print",
          signature = c(x = "Kernel"),
          definition = function(x, ...) {
              cat("kernel model = ")
              print(x = x@model)
            })

#' \code{show}
#'   displays kernel model.
#'
#' @rdname Kernel-methods
setMethod(f = "show",
          signature = c(object = "Kernel"),
          definition = function(object) {
              cat("kernel model = ")
              show(object = object@model)
            })

#' \code{summary}
#'   returns a list containing the kernel model.
#'
#' @rdname Kernel-methods
setMethod(f = "summary",
          signature = c(object = "Kernel"),
          definition = function(object, ...) {
              return( list("kernelModel" = object@model) )
            })
