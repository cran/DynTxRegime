# October 26, 2018

#' Class \code{LinearKernel}
#'
#' Class \code{LinearKernel} holds information regarding decision function 
#'   when kernel is linear
#'
#' @name LinearKernel-class
#'
#' @keywords internal
#'
#' @include K_Kernel.R
setClass(Class = "LinearKernel",
         contains = "Kernel",
         prototype = prototype(model = ~1, X = matrix(), kparam = NULL))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{LinearKernel}
#'
#' @name LinearKernel-methods
#'
#' @keywords internal
NULL

#' @rdname LinearKernel-methods
setMethod(f = ".kernelNumPars",
          signature = c(object = "LinearKernel"),
          definition = function(object, ...) {
              return( ncol(x = object@X) + 1L )
            })

#' @rdname LinearKernel-methods
setMethod(f = ".kernel",
          signature = c(object = "LinearKernel",
                        x1 = "matrix",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) {
                         if (ncol(x = x1) != ncol(x = x2)) stop("dim error")
                         Kern <- x1 %*% t(x = x2)
                         if (!is.matrix(x = Kern)) {
                           Kern <- matrix(data = Kern, nrow = nrow(x = x1))
                         }
                         if (nrow(x = Kern) != nrow(x = x1)) Kern <- t(x = Kern)
                         return( Kern )
                       })

#' @rdname LinearKernel-methods
setMethod(f = "print",
          signature = c(x = "LinearKernel"),
          definition = function(x, ...) {
              cat("kernel = linear\n")
              callNextMethod()
            })

#' @rdname LinearKernel-methods
setMethod(f = "show",
          signature = c(object = "LinearKernel"),
          definition = function(object) {
              cat("kernel = linear\n")
              callNextMethod()
            })

#' @rdname LinearKernel-methods
setMethod(f = "summary",
          signature = c(object = "LinearKernel"),
          definition = function(object, ...) {
              res1 <- list("kernel" = "linear")
              res2 <- callNextMethod()
              return( c(res1, res2) )
            })
