# October 26, 2018

#' Class \code{OptimBasic}
#'
#' Class \code{OptimBasic} holds results of an optimization step when linear
#'   kernel is used for decision function.
#'
#' @name OptimBasic-class
#'
#' @keywords internal
#'
#' @slot lambda A numeric, tuning parameter
#' @slot optim A list, value object returned by optimization method
#'     expected optimization methods are optim and hjk
#' @slot surrogate A Surrogate object specifying loss-function surrogate
#'
#' @include M_MethodObject.R
setClass(Class = "OptimBasic",
         slots = c(lambda = "numeric",
                   optim = "list",
                   surrogate = "Surrogate"),
         contains = c("KernelObj"))

##########
## GENERICS
##########

#' Complete an Optimization Step
#' 
#' Dispatches appropriate methods to optimize an object function.
#'
#' @rdname newOptim
#'
#' @keywords internal
setGeneric(name = ".newOptim",
           def = function(kernel, ...) { standardGeneric(f = ".newOptim") })

#' Extract Optimization Results
#'
#' Retrieves the value object returned by the optimization method for
#'   weighted learning methods.
#'
#' @param object A value object returned by a statistical method of DynTxRegime
#'   that uses optimization to estimate regime parameters.
#' @param ... Ignored.
#'
#' @name optimObj
#'
#' @usage
#' optimObj(object, ...)
#' 
#' ## S4 method for signature 'OWL'
#' optimObj(object, ...)
#' 
#' ## S4 method for signature 'RWL'
#' optimObj(object, ...)
#' 
#' ## S4 method for signature 'BOWL'
#' optimObj(object, ...)
#' 
#' ## S4 method for signature 'EARL'
#' optimObj(object, ...)
#' 
#' @exportMethod optimObj
setGeneric(name = "optimObj",
           def = function(object, ...) { standardGeneric(f = "optimObj") })


##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OptimBasic}
#'
#' @name OptimBasic-methods
#'
#' @keywords internal
NULL


#' @rdname newOptim
#' @importFrom stats rnorm
setMethod(f = ".newOptim",
          signature = c(kernel = "LinearKernel"),
          definition = function(kernel,
                                lambda,
                                methodObject,
                                suppress, ...) {

              # extract starting guesses for parameters
              par <- methodObject@pars

              # iterate optimization method until convergence or max iter
              conv <- 1L
              cnt <- 0L

              while (conv != 0L && cnt < 200L) {

                # .optim is defined for Surrogate
                test <- .optimFunc(methodObject = methodObject,
                                   par = par,
                                   lambda = lambda,
                                   suppress = suppress != 2L, ...)

                if (is.null(x = test)) {
                  # test is null if optimization failed
                  # reset parameters and try again
                  par <- stats::rnorm(n = length(x = par))
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

              if (!is.list(x = test)) {
                cat("optimization did not converge.\n")
                return( NULL )
              }

              if (suppress == 2L) {
                cat("\nResults returned by optimization method\n")
                print(x = test)
              }

              # return OptimBasic object if optimization converged
              return( new("OptimBasic",
                          "lambda" = lambda,
                          "optim" = test,
                          "surrogate" = methodObject@surrogate,
                          "kernel" = kernel) )
            })

#' @rdname OptimBasic-methods
setMethod(f = "optimObj",
          signature = c(object="OptimBasic"),
          definition = function(object, ...) {
              res1 <- object@optim
              res1[[ "lambda" ]]  <- object@lambda
              res1[[ "surrogate" ]] <- class(x = object@surrogate)[1L]
              res2 <- summary(object = as(object = object, Class = "KernelObj"))
              return( c(res1,res2) )
            })

#' @rdname OptimBasic-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimBasic",
                        newdata = "matrix"),
          definition = function(x, newdata) {

              # retrieve estimated parameters
              pars <- regimeCoef(object = x)

              # calculate decision function
              fx <- drop(x = unname(obj = newdata %*% pars[-1L] + pars[1L]))

              # determine optimal tx
              optTx <- rep(x = -1.0, times = length(x = fx))
              optTx[fx > 0] <- 1.0

              return( list("optimalTx"    = optTx,
                           "decisionFunc" = fx) )
            })

#' @rdname OptimBasic-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimBasic",
                        newdata = "data.frame"),
          definition = function (x, newdata, ...) {
              return( .predictOptimalTx(x = x, 
                                        newdata = .getKernelX(data = newdata, 
                                                              object = x@kernel)) )
             })

#' @rdname OptimBasic-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimBasic",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = x, newdata = x@kernel@X) )
            })

#' @rdname OptimBasic-methods
setMethod(f = "print",
          signature = c(x = "OptimBasic"),
          definition = function(x, ...) {
              callNextMethod()
              cat("lambda= ", x@lambda, "\n")
              cat("Surrogate:", class(x@surrogate)[1L], "\n")
              print(x = x@optim)
            })

#' @rdname OptimBasic-methods
setMethod(f = "regimeCoef",
          signature = c(object = "OptimBasic"),
          definition = function(object, ...) { return( object@optim$par ) })

#' @rdname OptimBasic-methods
setMethod(f = "show",
          signature = c(object = "OptimBasic"),
          definition = function(object) {
              callNextMethod()
              cat("\nlambda=  ", object@lambda, "\n")
              cat("Surrogate:", class(x = object@surrogate)[1L], "\n")
              show(object = object@optim)
            })

#' @rdname OptimBasic-methods
setMethod(f = "summary",
          signature = c(object = "OptimBasic"),
          definition = function(object, ...) {
              res1 <- object@optim
              res1[[ "lambda" ]]  <- object@lambda
              res1[[ "surrogate" ]] <- class(x = object@surrogate)[1L]
              res2 <- callNextMethod()
              return( c(res1, res2) )
            })
