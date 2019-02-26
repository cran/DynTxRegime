# October 26, 2018

#' Class \code{RadialKernel}
#'
#' Class \code{RadialKernel} holds information regarding decision function 
#'   when kernel is radial
#'
#' @name RadialKernel-class
#'
#' @keywords internal
#'
#' @include K_Kernel.R
setClass(Class = "RadialKernel",
         slots = c(kparam = "numeric"),
         contains = "Kernel",
         prototype = prototype(model = ~1, X = matrix(), kparam = numeric()))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{RadialKernel}
#'
#' @name RadialKernel-methods
#'
#' @keywords internal
NULL

#' @rdname RadialKernel-methods
setMethod(f = ".kernel",
          signature = c(object = "RadialKernel",
                        x1 = "matrix",
                        x2 = "matrix"),
          definition = function(object, x1, x2, ...) {
              if (ncol(x = x1) != ncol(x = x2)) stop("dim error")
              y <- t(x = x2)
              param <- 1.0 / (2.0 * object@kparam * object@kparam)

              Kern <- apply(X = x1,
                            MARGIN = 1L,
                            FUN = function(x) {
                                tm <- colSums(x = (x-y)^2)
                                exp(x = - param * tm)
                              })
              Kern <- t(x = Kern)
              if (!is.matrix(x = Kern)) Kern <- matrix(data = Kern, 
                                                       nrow = nrow(x = x1))
              if (nrow(x = Kern) != nrow(x = x1)) Kern <- t(x = Kern)
              if (nrow(x = Kern) != nrow(x = x1)) stop("dim error")
              return( Kern )
            })

#' @rdname RadialKernel-methods
setMethod(f = "print",
          signature = c(x = "RadialKernel"),
          definition = function(x, ...) {
              cat("kernel = radial; kparam =", x@kparam, "\n")
              callNextMethod()
            })

#' @rdname RadialKernel-methods
setMethod(f = "show",
          signature = c(object = "RadialKernel"),
          definition = function(object) {
              cat("kernel = radial; kparam =", object@kparam, "\n")
              callNextMethod()
            })

#' @rdname RadialKernel-methods
setMethod(f = "summary",
          signature = c(object = "RadialKernel"),
          definition = function(object, ...) {
              res1 <- list("kernel" = "radial",
                           "kparam" = object@kparam)
              res2 <- callNextMethod()
              return( c(res1, res2) )
            })
