# October 26, 2018

.validity_KernelObj <- function(object) {

  # @kernel must be Kernel or SubsetList of Kernel
  if (!is(object = object@kernel, class2 = "Kernel") &&
      !is(object = object@kernel, class2 = "SubsetList") &&
      !is.na(x = object@kernel)) {
    return( "incorrect object for @kernel" )
  }

  # if @kernel is SubsetList elements must be Kernel
  if (is(object = object@kernel, class2 = "SubsetList")) {
    for (i in 1L:length(x = object@kernel)) {
      if (!is(object = object@kernel, class2 = "Kernel")) {
        return( "incorrect object for @kernel" )
      }
    }
  }

  return( TRUE )
}

#' Class \code{KernelObj}
#'
#' Class \code{KernelObj} holds decision function kernel information
#'   under a common name.
#'
#' @name KernelObj-class
#'
#' @slot kernel ANY expected to be \code{Kernel} or \code{SubsetList}
#'
#' @keywords internal
#'
#' @include K_Kernel.R K_LinearKernel.R K_PolyKernel.R K_RadialKernel.R
#' @include K_MultiRadialKernel.R
setClass(Class = "KernelObj",
         slots = c(kernel = "ANY"),
         prototype = list(kernel = NA),
         validity = .validity_KernelObj)

##########
## GENERICS
##########

#' Create a KernelObj
#'
#' Processes input to determine type of kernel, creates it, and stores in 
#'   @slot kernel.
#'
#' @rdname newKernelObj
#'
#' @param kernel A character. Name of kernel
#' @param model A formula or list of formula
#'
#' @keywords internal
setGeneric(name = ".newKernelObj",
           def = function(kernel, model, ...) { 
               standardGeneric(f = ".newKernelObj") 
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{KernelObj}
#'
#' @name KernelObj-methods
#'
#' @keywords internal
NULL


#' @rdname newKernelObj
setMethod(f = ".newKernelObj",
          signature = c(kernel = "character",
                        model = "formula"),
          definition = function(kernel, model, data, kparam = NULL, ...) {

              if (kernel == 'linear') {
                obj <- new(Class = "LinearKernel", model = model, data = data)
              } else if (kernel == "poly") {
                obj <- new(Class = "PolyKernel",  
                           model = model,  
                           data = data,  
                           kparam = kparam)
              } else if (kernel == "radial" & !is.array(x = kparam)) {
                obj <- new(Class = "RadialKernel",  
                           model = model,  
                           data = data,  
                           kparam = kparam)
              } else if (kernel == "radial" & is.array(x = kparam)) {
                obj <- new(Class = "MultiRadialKernel",  
                           model = model,  
                           data = data,  
                           kparam = kparam)
              } else {
                stop("not allowed")
              }

              return( new(Class = "KernelObj", kernel = obj) )
            })

#' @rdname newKernelObj
setMethod(f = ".newKernelObj",
          signature = c(kernel = "list",
                        model = "list"),
          definition = function(kernel, model, data, kparam=NULL, ...) {

              mNames <- names(x = model)
              kNames <- names(x = kernel)

              if (!all(mNames %in% kNames) ||
                  !all(kNames %in% mNames)) {
                stop("kernel and model names do not agree")
              }

              if (is.list(x = kparam)) {
                pNames <- names(x = kparam)
                if (!all(pNames %in% kNames) ||
                    !all(kNames %in% pNames)) {
                  stop("kernel and kparam names do not agree")
                }
              } else {
                plist <- kernel
                plist[] <- kparam
                kparam <- plist
              }

              obj <- list()
              for (i in 1L:length(x = kernel)) {

                obj[[ kNames[i] ]] <- .newKernelObj(kernel = kernel[[ i ]], 
                                                    model[[ kNames[i] ]], 
                                                    data = data, 
                                                    kparam = kparam[[ kNames[i] ]])
              }

              obj <- new(Class = "SubsetList", obj)

              return( new(Class = "KernelObj", kernel = obj) )
            })

#' \code{.getKernelX}
#'   not allowed.
#'
#' @rdname KernelObj-methods
setMethod(f = ".getKernelX",
          signature = c(data = "data.frame",
                        object = "KernelObj"),
          definition = function(data, object) { stop("not allowed") })

#' \code{.kernel}
#'   not allowed.
#'
#' @rdname KernelObj-methods
setMethod(f = ".kernel",
          signature = c(object = "KernelObj",
                        x1 = "ANY",
                        x2 = "ANY"),
          definition = function(object, x1, x2, ...) { stop("not allowed") })

#' @rdname KernelObj-methods
setMethod(f = ".kernel",
          signature = c(object = "KernelObj",
                        x1 = "missing",
                        x2 = "missing"),
          definition = function(object, x1, x2, ...) { stop("not allowed") })

#' @rdname KernelObj-methods
setMethod(f = ".kernel",
          signature = c(object = "KernelObj",
                        x1 = "missing",
                        x2 = "ANY"),
          definition = function(object, x1, x2, ...) { stop("not allowed") })

#' @rdname KernelObj-methods
setMethod(f = ".kernel",
          signature = c(object = "KernelObj",
                        x1 = "ANY",
                        x2 = "missing"),
          definition = function(object, x1, x2, ...) { stop("not allowed") })

#' \code{.kernelNumPars}
#'   not allowed.
#'
#' @rdname KernelObj-class
setMethod(f = ".kernelNumPars",
          signature = c(object = "KernelObj"),
          definition = function(object, ...) { stop("not allowed") })

#' \code{print}
#'   prints kernel model. Includes "Kernel" as header.
#'
#' @rdname KernelObj-methods
setMethod(f = "print",
          signature = c(x = "KernelObj"),
          definition = function(x, ...) {
              cat("\nKernel\n")
              print(x = x@kernel, ...)
            })

#' \code{show}
#'   displays kernel model. Includes "Kernel" as header.
#'
#' @rdname KernelObj-methods
setMethod(f = "show",
          signature = c(object = "KernelObj"),
          definition = function(object) {
              cat("\nKernel\n")
              show(object = object@kernel)
            })

#' \code{summary}
#'   not allowed.
#'
#' @rdname KernelObj-methods
setMethod(f = "summary",
          signature = c(object = "KernelObj"),
          definition = function(object, ...) { 
              if (is(object = object@kernel, class2 = "Kernel")) {
                return( summary(object = object@kernel, ...) )
              } else if (is(object = object@kernel, class2 = "SubsetList")) {
                return( .cycleList(object = object@kernel, func = "summary") )
              }
            })
