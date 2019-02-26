# October 26, 2018

#' Class \code{BOWL}
#'
#' Class \code{BOWL} contains results from a single step of BOWL algorithm.
#'
#' @name BOWL-class
#'
#' @slot step Integer indicating step of the algorithm
#' @slot analysis Contains a Learning or LearningMulti object.
#' @slot analysis@txInfo Feasible tx information.
#' @slot analysis@propen Propensity regression analysis.
#' @slot analysis@outcome Outcome regression analysis.
#' @slot analysis@cvInfo Cross-validation analysis if single regime.
#' @slot analysis@optim Optimization analysis if single regime.
#' @slot analysis@optimResult list of cross-validation and optimization results
#'   if multiple regimes. optimResult[[i]]@cvInfo and optimResult[[i]]@optim.
#' @slot analysis@optimal Estimated optimal Tx and value.
#' @slot analysis@call Unevaluated call to statistical method.
#' @slot prodPi Vector of the products of the propensity for the tx received 
#' @slot sumR Vector of the sum of the rewards
#' @slot index Vector indicating compliance with estimated optimal regime
#'
#' @keywords internal
#'
#' @include R_class_BOWLBasic.R
#'
setClass(Class = "BOWL",
         slots = c(step = "integer"),
         contains = c("BOWLBasic"))

##########
## GENERICS
##########

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".newBOWL",
           def = function(BOWLObj, ...) { standardGeneric(".newBOWL") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{BOWL}
#'
#' @name BOWL-methods
#'
#' @keywords internal
NULL

#' Create a BOWL Object for First Step of BOWL Algorithm
#'
#' @param BOWLObj NULL or an object returned from a previous step
#' @param moPropen modelObj or modelObjSubset for propensity modeling
#' @param fSet optional function defining subsets for modeling
#' @param data data.frame of covariates
#' @param response response
#' @param txName treatment variable column header in data
#' @param lambdas tuning parameter(s)
#' @param cvFolds number of cross-validation folds
#' @param kernel Kernel object
#' @param surrogate Surrogate object
#' @param suppress T/F indicating if prints to screen are to be executed
#' @param guess Starting values for optimization
#'
#' @name newBOWL
#'
#' @keywords internal
setMethod(f = ".newBOWL",
          signature = c(BOWLObj  = "NULL"),
          definition = function(BOWLObj,
                                moPropen,
                                fSet,
                                data,
                                response,
                                txName,
                                lambdas,
                                cvFolds,
                                kernel,
                                surrogate,
                                suppress, 
                                guess, ...) {

              index <- !logical(length = nrow(x = data))
              prodPi <- rep(x = 1.0, times = nrow(x = data))

              result <- .newBOWLStep(moPropen = moPropen,
                                     fSet = fSet,
                                     data = data,
                                     response = response,
                                     txName = txName,
                                     lambdas = lambdas,
                                     cvFolds = cvFolds,
                                     kernel = kernel,
                                     surrogate = surrogate,
                                     suppress = suppress,
                                     guess = guess,
                                     prodPi = prodPi,
                                     index = index, ...)

              return( new("BOWL", "step" = 1L, result) )

            })

#' @rdname newBOWL
setMethod(f = ".newBOWL",
          signature = c(BOWLObj  = "BOWL"),
          definition = function(BOWLObj,
                                moPropen,
                                fSet,
                                data,
                                response,
                                txName,
                                lambdas,
                                cvFolds,
                                kernel,
                                surrogate,
                                suppress, 
                                guess, ...) {

              if ({length(x = BOWLObj@sumR) != length(x = response)} ||
                  {length(x = response) != nrow(x = data)}) {
                stop("length of reward/response does not match previous steps")
              }

              response <- BOWLObj@sumR + response

              result <- .newBOWLStep(moPropen = moPropen,
                                     fSet = fSet,
                                     data = data,
                                     response = response,
                                     txName = txName,
                                     lambdas = lambdas,
                                     cvFolds = cvFolds,
                                     kernel = kernel,
                                     surrogate = surrogate,
                                     suppress = suppress,
                                     guess = guess,
                                     prodPi = BOWLObj@prodPi,
                                     index = BOWLObj@index, ...)

              return( new("BOWL", "step" = BOWLObj@step + 1L, result) )

            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "Call",
          signature = c(name = "BOWL"),
          definition = function(name, ...) {
              return( Call(name = as(object = name, 
                                     Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "cvInfo",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( cvInfo(object = as(object = object, 
                                         Class = "BOWLBasic"), ...)$cvInfo )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "coef",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( coef(object = as(object = object, 
                                       Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep", 
          signature = c(object = "BOWL"), 
          definition = function(object) {
              cat("Step", object@step, "of BOWL.\n")
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "estimator",
          signature = c(x = "BOWL"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x, 
                                       Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "fitObject",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object, 
                                            Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optimObj",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( optimObj(object = as(object = object, 
                                           Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "BOWL",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x, Class = "BOWLBasic"), 
                            newdata = newdata, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "BOWL",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x, Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "BOWL"),
          definition = function(object, ...) { return( NA) })

#' @rdname DynTxRegime-internal-api
setMethod(f = "plot",
          signature = c(x = "BOWL", y = "missing"),
          definition = function(x, y, suppress = FALSE, ...) {
              plot(x = as(object = x, Class = "BOWLBasic"), 
                   suppress = suppress, ...)
            })

#' @rdname BOWL-methods
setMethod(f = "print",
          signature = c(x = "BOWL"),
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = as(object = x, Class = "BOWLBasic"), ...)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "propen",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( propen(object = as(object = object, 
                                         Class = "BOWLBasic"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "regimeCoef",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( regimeCoef(object = as(object = object, 
                                             Class = "BOWLBasic"), ...) )
            })

#' @rdname BOWL-methods
setMethod(f = "show",
          signature = c(object = "BOWL"),
          definition = function(object) {
              DTRstep(object = object)
              show(object = as(object = object, Class = "BOWLBasic"))
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "summary",
          signature = c(object = "BOWL"),
          definition = function(object, ...) {
              return( summary(object = as(object = object, 
                                          Class = "BOWLBasic"), ...) )
            })
