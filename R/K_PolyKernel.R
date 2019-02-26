# October 26, 2018

#' Class \code{PolyKernel}
#'
#' Class \code{PolyKernel} holds information regarding decision function 
#'   when kernel is polynomial
#'
#' @name PolyKernel-class
#'
#' @keywords internal
#'
#' @include K_Kernel.R
setClass(Class = "PolyKernel",
         slots = c(kparam = "numeric"),
         contains = "Kernel",
         prototype = prototype(model = ~1, X = matrix(), kparam = numeric()))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{PolyKernel}
#'
#' @name PolyKernel-methods
#'
#' @keywords internal
NULL

#' @rdname PolyKernel-methods
setMethod(f = ".kernel",
          signature = c(object = "PolyKernel",
                        x1 = "matrix",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) {
              if (ncol(x = x1) != ncol(x = x2)) stop("dim error")
              Kern <- (1.0 + x1 %*% t(x2))^object@kparam
              if (!is.matrix(x = Kern) ) Kern <- matrix(data = Kern, 
                                                        nrow = nrow(x = x1))
              if (nrow(x = Kern) != nrow(x = x1)) Kern <- t(x = Kern)
              if (nrow(x = Kern) != nrow(x = x1)) stop("dim error")
              return( Kern )
            })

#' @rdname PolyKernel-methods
setMethod(f = "print",
          signature = c(x = "PolyKernel"),
          definition = function(x, ...) {
              cat("kernel = polynomial; kparam =", x@kparam, "\n")
              callNextMethod()
            })

#' @rdname PolyKernel-methods
setMethod(f = "show",
          signature = c(object = "PolyKernel"),
          definition = function(object) {
              cat("kernel = polynomial; kparam =", object@kparam, "\n")
              callNextMethod()
            })

#' @rdname PolyKernel-methods
setMethod(f = "summary",
          signature = c(object = "PolyKernel"),
          definition = function(object, ...) {
              res1 <-  list("kernel" = "polynomial",
                            "kparam" = object@kparam)
              res2 <- callNextMethod()
              return( c(res1, res2))
            })
