# October 26, 2018

.validity_RWL <- function(object) {

  # analysis must be Learning or LearningMulti
  if (!is(object = object@analysis, class2 = "Learning") &&
      !is(object = object@analysis, class2 = "LearningMulti") &&
      !is.na(x = object@analysis)) {
    return( "incorrect object for @analysis" )
  }

  return( TRUE )
}

#' Class \code{RWL}
#'
#' Class \code{RWL} contains results for an RWL analysis.
#'
#' @name RWL-class
#'
#' @include Q_class_.rwl.R
#'
#' @slot responseType character indicating type of response
#' @slot residuals vector of outcome residuals
#' @slot beta vector of regime parameters
#' @slot analysis Contains a Learning or LearningMulti object
#' @slot analysis@txInfo Feasible tx information
#' @slot analysis@propen Propensity regression analysis
#' @slot analysis@outcome Outcome regression analysis
#' @slot analysis@cvInfo Cross-validation analysis if single regime
#' @slot analysis@optim Optimization analysis if single regime
#' @slot analysis@optimResult list of cross-validation and optimization results
#'   if multiple regimes. optimResult[[i]]@cvInfo and optimResult[[i]]@optim
#' @slot analysis@optimal Estimated optimal Tx and value
#' @slot analysis@Call Unevaluated Call
#'
#' @template outcomeOnly
#' @template propenOnly
#' @template regression
#' @template optimOnly
#' @template DynTxRegime_methods
setClass(Class = "RWL",
         slots = c(responseType = "character",
                   residuals = "numeric",
                   beta = "numeric",
                   analysis = "ANY"),
         prototype = list("responseType" = "",
                          "residuals" = numeric(),
                          "beta" = numeric(),
                          "analysis" = NA),
         validity = .validity_RWL)

##########
## GENERICS
##########

#' Complete a Residual Weighted Learning Analysis
#'
#' @name newRWL
#'
#' @param kernel A Kernel object
#'
#' @keywords internal
setGeneric(name = ".newRWL",
           def = function(kernel, ...) { standardGeneric(".newRWL") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{RWL}
#'
#' @name RWL-methods
#'
#' @keywords internal
NULL


#' Complete a Residual Weighted Learning Analysis
#'
#' @rdname newRWL-methods
#'
#' @param moPropen modelObj for propensity modeling
#' @param moMain modelObj for main effects
#' @param data data.frame of covariates
#' @param response vector of responses
#' @param txName treatment variable column header in data
#' @param lambdas tuning parameter(s)
#' @param cvFolds number of cross-validation folds
#' @param kernel Kernel object
#' @param fSet Function or NULL defining subsets
#' @param responseType Character indicating type of response
#' @param txVec treatment vector recast as +/- 1
#' @param surrogate Surrogate object
#' @param suppress T/F indicating if prints to screen are executed
#' @param guess optional numeric vector providing starting values for
#'   optimization methods
#' @param ...  Additional inputs for optimization
#'
#' @return An RWL object
#'
#' @keywords internal
setMethod(f = ".newRWL",
          signature = c(kernel = "Kernel"),
          definition = function(moPropen,
                                moMain,
                                responseType,
                                data,
                                response,
                                txName,
                                lambdas,
                                cvFolds,
                                surrogate,
                                guess,
                                kernel,
                                fSet,
                                suppress, ...) {

              if (suppress != 0L) cat("Residual Weighted Learning\n")

              if (responseType == 'count') func <- '.createrwlcount'
              if (responseType != 'count') func <- '.createrwl'

              analysis <- .newLearning(moPropen = moPropen,
                                       moMain = moMain,
                                       moCont = NULL,
                                       data = data,
                                       response = response,
                                       txName = txName,
                                       lambdas = lambdas,
                                       cvFolds = cvFolds,
                                       kernel = kernel,
                                       fSet = fSet,
                                       iter = NULL,
                                       surrogate = surrogate,
                                       suppress = suppress,
                                       guess = guess,
                                       createObj = func, 
                                       prodPi = rep(x = 1.0, times = nrow(x = data)),
                                       index = rep(x = TRUE, times = nrow(x = data)), ...)

              # recast tx as -1/1
              txVec <- .convertToBinary(txObj = analysis@txInfo, data = data)

              # get propensity for tx received
              prWgt <- .getPrWgt(propenObj = as(object = analysis, 
                                                Class = "PropensityObj"),
                                 txObj = analysis@txInfo,
                                 data = data)

              # get estimated outcome for each tx
              mu <- .getOutcome(outcomeObj = as(object = analysis, 
                                                Class = "OutcomeObj"),
                                txObj = analysis@txInfo,
                                data = data)

              if (is(object = kernel, class2 = "LinearKernel")) {
                kern <- kernel@X
              } else if (!is(object = kernel, class2 = "MultiRadialKernel")) {
                kern <- .kernel(object = kernel)
              } else {
                optPars <- .getOptimal(object = analysis@cvInfo)
                lambda <- optPars$lambda
                kernel <- new("RadialKernel", data = data,
                              model = kernel@model, 
                              kparam = optPars$kernel@kparam)
                kern <- .kernel(object = kernel)
              }

              if (responseType == "count") {
                rwlObj <- .createrwlcount(kernel = kernel, 
                                          txVec = txVec,  
                                          response = response,  
                                          prWgt = prWgt,  
                                          surrogate = surrogate,  
                                          guess = guess, 
                                          mu = mu)
              } else {
                rwlObj <- .createrwl(kernel = kernel, 
                                     txVec = txVec,  
                                     response = response,  
                                     prWgt = prWgt,  
                                     surrogate = surrogate,  
                                     guess = guess, 
                                     mu = mu)
              }

              rwlObj@x <- kern

              if (is(analysis@txInfo, "TxSubset")) {
                singles <- .getSingleton(object = analysis@txInfo)
                rwlObj <- .subsetObject(rwlObj, !singles)
                if (!is(object = kernel, class2 = "LinearKernel")) {
                  rwlObj@x <- rwlObj@x[,!singles,drop=FALSE]
                }
              }

              beta <- .calculateBeta(par = regimeCoef(object = analysis), 
                                     kern = rwlObj@x,  
                                     methodObject = rwlObj)

              return( new("RWL",
                          "responseType" = responseType,
                          "residuals" = rwlObj@residual,
                          "beta" = beta[abs(beta)>1e-8],
                          "analysis" = analysis) )
            })

#' @rdname newRWL
setMethod(f = ".newRWL",
          signature = c(kernel = "SubsetList"),
          definition = function(moPropen,
                                moMain,
                                responseType,
                                data,
                                response,
                                txName,
                                lambdas,
                                cvFolds,
                                surrogate,
                                guess,
                                kernel,
                                fSet,
                                suppress, ...) {

              if (suppress != 0L) cat("Residual Weighted Learning\n")

              if (responseType == 'count') func <- '.createrwlcount'
              if (responseType != 'count') func <- '.createrwl'

              analysis <- .newLearning(fSet = fSet,
                                       kernel = kernel,
                                       moPropen = moPropen,
                                       moMain = moMain,
                                       moCont = NULL,
                                       data = data,
                                       response = response,
                                       txName = txName,
                                       lambdas = lambdas,
                                       cvFolds = cvFolds,
                                       iter = NULL,
                                       surrogate = surrogate,
                                       suppress = suppress,
                                       guess = guess,
                                       createObj = func, 
                                       prodPi = rep(x = 1.0, times = nrow(x = data)),
                                       index = rep(x = TRUE, times = nrow(x = data)), ...)

              # recast tx as -1/1
              txVec <- .convertToBinary(txObj = analysis@txInfo, data = data)

              # extract optimal parameters
              par <- regimeCoef(object = analysis)

              # get propensity weights for tx received
              prWgt <- .getPrWgt(propenObj = as(object = analysis, 
                                                Class = "PropensityObj"),
                                 txObj = analysis@txInfo,
                                 data = data)

              # get estimated outcome for each tx
              mu <- .getOutcome(outcomeObj = as(object = analysis, 
                                                Class = "OutcomeObj"),
                                txObj = analysis@txInfo,
                                data = data)

              ptsSubset <- .getPtsSubset(object = analysis@txInfo)
              beta <- rep(x = NA, times = nrow(x = data))

              for (i in 1L:length(x = kernel)) {
                kName <- names(x = kernel)[i]
                kNames <- unlist(x = strsplit(x = kName, split = ","))

                usePts <- ptsSubset %in% kNames

                if (is(object = kernel[[ i ]], class2 = "LinearKernel")) {
                  kern <- kernel[[ i ]]@X
                } else if (!is(object = kernel[[ i ]], class2 = "MultiRadialKernel")) {
                  kern <- .kernel(object = kernel[[ i ]])
                } else {
                  optPars <- .getOptimal(object = analysis@optimResult[[ i ]]@cvInfo)
                  lambda <- optPars$lambda
                  kernel[[ i ]] <- new("RadialKernel", data = data,
                                       model = kernel[[ i ]]@model, 
                                       kparam = optPars$kernel@kparam)
                  kern <- .kernel(object = kernel[[ i ]])
                }

                if (responseType == "count") {
                  rwlObj <- .createrwlcount(kernel = kernel[[ i ]], 
                                            txVec = txVec,  
                                            response = response,  
                                            prWgt = prWgt,  
                                            surrogate = surrogate,  
                                            guess = guess, 
                                            mu = mu)
                } else {
                  rwlObj <- .createrwl(kernel = kernel[[ i ]], 
                                       txVec = txVec,  
                                       response = response,  
                                       prWgt = prWgt,  
                                       surrogate = surrogate,  
                                       guess = guess, 
                                       mu = mu)
                }

                rwlObj@x <- kern

                rwlObj <- .subsetObject(methodObject = rwlObj, subset = usePts)

                if (!is(object = kernel[[ i ]], class2 = "LinearKernel")) {
                  rwlObj@x <- rwlObj@x[,usePts,drop=FALSE]
                }

                beta[usePts] <- .calculateBeta(par = par[[ paste0('Subset=',kName) ]], 
                                               kern = rwlObj@x,  
                                               methodObject = rwlObj)
              }

              residual <- response - mu[,2L]
              residual[txVec < -0.5] <- response[txVec < -0.5] - 
                                        mu[txVec < -0.5,1L]

              return( new("RWL",
                          "responseType" = responseType,
                          "residuals" = residual,
                          "beta" = beta,
                          "analysis" = analysis) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "Call",
          signature = c(name = "RWL"),
          definition = function(name, ...) {
              return( Call(name = name@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "coef",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( coef(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "cvInfo",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( cvInfo(object = object@analysis, ...)$cvInfo )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep",
          signature = c(object = "RWL"),
          definition = function(object) {
              cat("Residual Weighted Learning\n")
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "estimator",
          signature = c(x = "RWL"),
          definition = function(x, ...) {
              return( estimator(x = x@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "fitObject",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( fitObject(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optimObj",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              res <- optimObj(object = object@analysis, ...)
              res[[ "beta" ]] <- object@beta
              return( res )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "RWL",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, newdata = newdata, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "RWL",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = x@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( outcome(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "plot",
          signature = c(x = "RWL", y = "missing"),
          definition = function(x, y, suppress = FALSE, ...) {
              plot(x = x@analysis, suppress = suppress, ...)
            })

#' @rdname RWL-methods
setMethod(f = "print",
          signature = c(x = "RWL"),
          definition = function(x, ...) {
              DTRstep(object = x)
              cat("Response of type", x@responseType, "\n")
              print(x = x@analysis, ...)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "propen",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( propen(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "regimeCoef",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@analysis, ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "residuals",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              return( object@residuals )
            })

#' @rdname RWL-methods
setMethod(f = "show",
          signature = c(object = "RWL"),
          definition = function(object) {
              DTRstep(object = object)
              cat("Response of type", object@responseType, "\n")
              show(object = object@analysis)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "summary",
          signature = c(object = "RWL"),
          definition = function(object, ...) {
              res1 <- summary(object = object@analysis, ...)
              res2 <- list("beta" = object@beta)
              return( c(res1, res2) )
            })
