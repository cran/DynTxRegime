# October 26, 2018

#' Class \code{OptimKernel}
#'
#' Class \code{OptimKernel} holds results of an optimization step when non-linear
#'   kernel is used for decision function.
#'
#' @name OptimKernel-class
#'
#' @keywords internal
#'
#' @include M_OptimBasic.R
setClass(Class = "OptimKernel",
         contains = c("OptimBasic"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OptimKernel}
#'
#' @name OptimKernel-methods
#'
#' @keywords internal
NULL

#' @rdname newOptim
setMethod(f = ".newOptim",
          signature = c(kernel = "Kernel"),
          definition = function(kernel,
                                lambda,
                                methodObject,
                                suppress, ...) {

              # redefine the covariate matrix to be the kernel matrix
              methodObject@x <- .kernel(object = kernel, 
                                        x1 = methodObject@x, 
                                        x2 = methodObject@x)

              # extract starting guesses for parameters
              par <- methodObject@pars

              # iterate optimization method until convergence or max iter
              conv <- 1L
              cnt <- 0L
              while (conv != 0L && cnt < 100L) {

                # .optim is defined for Surrogate
                test <- .optimFunc(methodObject = methodObject,
                                   par = par,
                                   lambda = lambda,
                                   suppress = suppress != 2L, ...)

                if (is.null(x = test)) {
                  # test is null if optimization failed
                  # reset parameters and try again
                  par <- rnorm(n = length(x = par))
                  cnt <- cnt + 1L
                  next
                } else if (is.numeric(x = test)) {
                  # test is the last parameter estimates if optimization
                  # hit max iterations
                  par <- test
                  cnt <- cnt + 1L
                  next
                }

                conv <- test$convergence
                par <- test$par
                cnt <- cnt + 1L
              }

              # return NULL if optimization did not converge
              if (is.null(x = test)) {
                cat("optimization did not converge.\n")
                return( NULL )
              }

              if (suppress == 2L) {
                cat("\nResults returned by optimization method\n")
                print(x = test)
              }

              # create OptimBasic object if optimization converged
              res <- new(Class = "OptimBasic",
                         "lambda" = lambda,
                         "optim" = test,
                         "surrogate" = methodObject@surrogate,
                         "kernel" = kernel)

              # return OptimKernel object
               return( new(Class = "OptimKernel", res) )
             })

#' @rdname OptimKernel-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimKernel",
                        newdata = "matrix"),
          definition = function(x, newdata) {

              # generate kernel
              kern <- .kernel(object = x@kernel, x1 = x@kernel@X, x2 = newdata)

              # retrieve estimated parameters
              pars <- regimeCoef(object = x)

              # ensure that kernel is appropriately dimensioned
              if (ncol(x = kern) != {length(x = pars)-1L} ) kern <- t(x = kern)
              if (ncol(x = kern) != {length(x = pars)-1L} ) stop('dim problem')

              # calculate decision function
              fx <- drop(x = unname(obj = kern %*% pars[-1L] + pars[1L]))

              # determine optimal tx
              optTx <- rep(x = -1.0, times = length(x=fx))
              optTx[fx > 0] <- 1.0

              return( list("optimalTx"    = optTx,
                           "decisionFunc" = fx) )
            })

#' @rdname OptimKernel-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimKernel",
                        newdata = "data.frame"),
          definition = function (x, newdata, ...) {
              return( .predictOptimalTx(x = x, 
                                        newdata = .getKernelX(data = newdata, 
                                                              object = x@kernel)) )
             })

#' @rdname OptimKernel-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimKernel",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = x, newdata = x@kernel@X) )
            })
