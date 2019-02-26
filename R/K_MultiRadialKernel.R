# October 26, 2018

#' Class \code{MultiRadialKernel}
#'
#' Class \code{MultiRadialKernel} holds information regarding decision function 
#'   when kernel is radial and multiple kernel parameters
#'
#' @name MultiRadialKernel-class
#'
#' @keywords internal
#'
#' @include K_Kernel.R
setClass(Class = "MultiRadialKernel",
         slots = c(kparam = "array"),
         contains = "Kernel",
         prototype = prototype(model = ~1, X = matrix(), kparam = array()))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{MultiRadialKernel}
#'
#' @name MultiRadialKernel-methods
#'
#' @keywords internal
NULL

#' \code{.kernel}
#'   not allowed.
#'
#' @rdname MultiRadialKernel-methods
setMethod(f = ".kernel",
          signature = c(object = "MultiRadialKernel",
                        x1 = "matrix",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) { stop("not allowed") })

#' \code{print}
#'   not allowed.
#'
#' @rdname MultiRadialKernel-methods
setMethod(f = "print",
          signature = c(x = "MultiRadialKernel"),
          definition = function(x, ...){ stop("not allowed") })

#' \code{show}
#'   not allowed.
#'
#' @rdname MultiRadialKernel-methods
setMethod(f = "show",
          signature = c(object = "MultiRadialKernel"),
          definition = function(object){ stop("not allowed") })

#' \code{summary}
#'   not allowed.
#'
#' @rdname MultiRadialKernel-methods
setMethod(f = "summary",
          signature = c(object = "MultiRadialKernel"),
          definition = function(object, ...){ stop("not allowed") })
