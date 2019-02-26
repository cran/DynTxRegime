# October 26, 2018

#' Class \code{OptimStep}
#' Class \code{OptimStep} holds results of a combined cross-validation and final
#'   optimization step for weighted learning methods.
#'
#' @name OptimStep-class
#'
#' @keywords internal
#'
#' @include N_CVInfoObj.R
#'
setClass(Class = "OptimStep",
         contains = c("CVInfoObj",
                      "OptimObj",
                      "DynTxRegime"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OptimStep}
#'
#' @name OptimStep-methods
#'
#' @keywords internal
NULL

#' @rdname OptimStep-methods
setMethod(f = "Call",
          signature = c(name = "OptimStep"),
          definition = function(name, ...) {
              return( Call(name = as(object = name, 
                                     Class = "DynTxRegime"), ...) )
            })

#' @rdname OptimStep-methods
setMethod(f = "cvInfo",
          signature = c(object = "OptimStep"),
          definition = function(object) {
              return( cvInfo(object = as(object = object, Class = "CVInfoObj")) )
            })

#' @rdname OptimStep-methods
setMethod(f = "estimator",
          signature = c(x = "OptimStep"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x, 
                                       Class = "DynTxRegime"), ...) )
            })

#' @rdname OptimStep-methods
setMethod(f = "optimObj",
          signature = c(object = "OptimStep"),
          definition = function(object) {
              return( optimObj(object = as(object = object, Class = "OptimObj")) )
            })

#' @rdname OptimStep-methods
setMethod(f = "optTx",
          signature = c(x = "OptimStep",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x, Class = "DynTxRegime"), ...) )
            })

#' @rdname OptimStep-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimStep",
                        newdata = "matrix"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = as(object = x, Class = "OptimObj"),
                                        newdata = newdata) )
            })

#' @rdname OptimStep-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimStep",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = as(object = x, Class = "OptimObj"),
                                        newdata = newdata) )
            })

#' @rdname OptimStep-methods
setMethod(f = ".predictOptimalTx",
          signature = c(x = "OptimStep",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( .predictOptimalTx(x = as(object = x, Class = "OptimObj")) )
            })

#' @rdname OptimStep-methods
setMethod(f = "print",
          signature = c(x = "OptimStep"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "CVInfoObj"))
              print(x = as(object = x, Class = "OptimObj"))
              print(x = as(object = x, Class = "DynTxRegime"))
            })

#' @rdname OptimStep-methods
setMethod(f = "regimeCoef",
          signature = c(object = "OptimStep"),
          definition = function(object) {
              return( regimeCoef(object = as(object = object, Class = "OptimObj")) )
            })

#' @rdname OptimStep-methods
setMethod(f = "show",
          signature = c(object = "OptimStep"),
          definition = function(object) {
              show(object = as(object = object, Class = "CVInfoObj"))
              show(object = as(object = object, Class = "OptimObj"))
              show(object = as(object = object, Class = "DynTxRegime"))
            })

#' @rdname OptimStep-methods
setMethod(f = "summary",
          signature = c(object = "OptimStep"),
          definition = function(object, ...) {
              res1 <- summary(object = as(object = object, Class = "CVInfoObj"))
              res2 <- summary(object = as(object = object, Class = "OptimObj"))
              res3 <- summary(object = as(object = object, Class = "DynTxRegime"))
              return( c(res1, res2, res3) )
            })

#' Complete Cross-Validation Step and Final Optimization
#'
#' @name OptimStep
#'
#' @param methodObject Object parameters for weighted learning method
#' @param lambdas tuning parameter
#' @param cvFolds number of cross-validation folds
#' @param suppress integer indicating screen printing preferences
#'
#' @keywords internal
.OptimStep <- function(methodObject,
                       lambdas,
                       cvFolds,
                       txVec,
                       suppress, ...) {

  # if initial estimates for parameters not provided default to 0
  if (is.null(x = methodObject@pars)) {
    methodObject@pars <- rep(x = 0.0, 
                             times = .kernelNumPars(object = methodObject@kernel))
  }

  if (!is.null(x = cvFolds)) {

    # subset data for cross-validation
    cvObject <- new(Class = "CVBasic", 
                    cvFolds = cvFolds, 
                    txVec = txVec)

    # if cvFolds is not NULL, cross-validation was requested; perform CV
    cvResult <- .newCVInfoObj(lambdas = lambdas,
                              kernel = methodObject@kernel,
                              methodObj = methodObject,
                              cvObject = cvObject,
                              suppress = suppress, ...)

    if (is(object = cvResult@cvInfo, class2 = "CVInfo")) {
      # if returned object is CVInfo, extract optimal parameters
      optPars <- .getOptimal(object = cvResult)
      lambda <- optPars$lambda
      methodObject@kernel <- optPars$kernel
    } else {
      # if returned object is not CVInfo, STOP
      stop("cross-validation failed")
    }

  } else {
    # if cvFolds is NULL, cross-validation not requested
    cvResult <- new(Class = "CVInfoObj", "cvInfo" = NA)
    lambda <- lambdas[1L]
  }

  n <- length(x = txVec)

  # do final optimization with given or recommended parameters
  # .trainForValue is defined in CVBasic

  if (suppress != 0L) cat("\nFinal optimization step.\n")

  optimObj <- .trainForValue(methodObject = methodObject,
                             train_subset = 1L:n,
                             test_subset = 1L:n,
                             lambda = lambda,
                             suppress = suppress, ...)

  # if returned object is NULL, optimization was not successful
  if (is.null(x = optimObj)) {
    stop("optimization was not successful")
  }

  if (suppress == 1L) print(x = optimObj$optimObj)

  return( new(Class = "OptimStep", 
              optimObj$optimObj,  
              optimObj$dtrObj,  
              cvResult) )

}
