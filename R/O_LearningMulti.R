# October 26, 2018

.validity_LearningMulti <- function(object) {

  # @optimResult must be NA or a SubsetList
  if (!is(object = object@optimResult, class2 = "SubsetList") &&
      !is.na(x = object@optimResult)) {
    return( "incorrect object for @optimResult" )
  }

  # if SubsetList elements must be OptimStep
  if (is(object = object@optimResult, class2 = "SubsetList")) {
    for (i in 1L:length(x = object@optimResult)) {
      if (!is(object = object@optimResult[[ i ]], class2 = "OptimStep")) {
        return( "incorrect object for @optimResult" )
      }
    }
  }

  return( TRUE )
}

#' Class \code{LearningMulti}
#'
#' Class \code{LearningMulti} contains results for a learning analysis 
#'   with multiple regimes.
#'
#' @name LearningMulti-class
#'
#' @slot optimResult ANY containing a list of OptimStep results
#'
#' @include O_Learning.R
#'
#' @keywords internal
setClass(Class = "LearningMulti",
         slot = c(optimResult = "ANY"),
         contains = c("TxObj", "PropensityObj", "OutcomeObj", "DynTxRegime"),
         prototype = list(optimResult = NA),
         validity = .validity_LearningMulti)

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{LearningMulti}
#'
#' @name LearningMulti-methods
#'
#' @keywords internal
NULL

#' @rdname newLearning
setMethod(f = ".newLearning",
          signature = c(fSet = "function",
                        kernel = "SubsetList"),
          definition = function(fSet, 
                                kernel,
                                moPropen,
                                moMain,
                                moCont,
                                data,
                                response,
                                txName,
                                lambdas,
                                cvFolds,
                                iter,
                                surrogate,
                                suppress,
                                guess, 
                                createObj, 
                                prodPi = 1.0,
                                index = NULL, ...) {

              # generate default index, which includes all data
              if (is.null(x = index)) {
                index <- rep(x = TRUE, times = nrow(x = data))
              }

              # process tx information
              txObj <- .newTxObj(txName = txName,
                                 data = data,
                                 fSet = fSet,
                                 suppress = suppress == 0L)

              # recast tx as -1/1
              txVec <- .convertToBinary(txObj = txObj, data = data)

              # complete propensity regression
              propenObj <- .newPropensityObj(moPropen = moPropen,
                                             txObj = txObj,
                                             data = data,
                                             suppress = suppress == 0L)

              # get propensity for tx received
              prWgt <- .getPrWgt(propenObj = propenObj, 
                                 txObj = txObj,  
                                 data = data)

              # include propensity weights
              prWgt <- prWgt * prodPi

              # complete outcome regression
              outcomeObj <- .newOutcomeObj(moMain = moMain,
                                           moCont = moCont,
                                           data = data,
                                           response = response,
                                           txObj = txObj,
                                           iter = iter,
                                           suppress = suppress == 0L)

              # get estimated outcome for each tx
              mu <- .getOutcome(outcomeObj = outcomeObj,
                                txObj = txObj,
                                data = data)

              # storage objects for optimal tx and decision function
              optVec <- txVec
              df <- rep(x = NA, times = nrow(x = data))

              res <- list()

              # extract patient subsets
              ptsSubset <- .getPtsSubset(object = txObj)

              # create argument list for createObj function
              argList <- list()
              argList[[ "txVec" ]] <- txVec
              argList[[ "response" ]] <- response
              argList[[ "prWgt" ]] <- prWgt
              argList[[ "surrogate" ]] <- surrogate
              argList[[ "guess" ]] <- guess
              argList[[ "mu" ]] <- mu

              for (i in 1L:length(x = kernel)) {

                # extract subset names for this kernel
                kName <- names(x = kernel)[i]
                kNames <- unlist(x = strsplit(x = kName, split = ","))

                # identify patients in the kernel subsets
                usePts <- ptsSubset %in% kNames

                # limit patient subsets to those with index=T
                usePts <- usePts & index

                # create method object for optimization/cross-validation methods
                argList[[ "kernel" ]] <- kernel[[ i ]]

                methodObj <- do.call(what = createObj, args = argList)

                # subset method object to those in this subset
                methodObj <- .subsetObject(methodObject = methodObj,
                                           subset = usePts)

                # perform cross-validation and optimization steps
                res[[ kName ]] <- .OptimStep(methodObject = methodObj,
                                             lambdas = lambdas,
                                             cvFolds = cvFolds,
                                             txVec = txVec[usePts],
                                             suppress = suppress, ...)

                # extract estimated optimal tx and decision function
                opt <- optTx(x = res[[ kName ]])

                # store optimal information
                optVec[usePts] <- opt$optimalTx
                df[usePts] <- opt$decisionFunc

              }

              # convert OptimStep results to a SubsetList
              res <- new("SubsetList", res)

              # create method object with full data
              methodObj <- do.call(what = createObj, args = argList)

              # re-calculate estimated value
              value <- .valueFunc(methodObject = methodObj, optTx = optVec)

              # optimal tx is returned as -1/1. convert to original coding
              optVec <- .convertFromBinary(txObj = txObj, 
                                           txVec = optVec)

              # create DynTxRegime object with full optimal estimates
              dtrObj <- new("DynTxRegime",
                            optimal = new("OptimalInfo",
                                          "optimalTx" = optVec,
                                          "decisionFunc" = df,
                                          "estimatedValue" = value),
                            "call" = NULL)

             if (suppress != 0L) print(x = dtrObj@optimal)

             return( new("LearningMulti", 
                          "optimResult" = res, 
                          dtrObj, txObj, outcomeObj, propenObj) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "Call",
          signature = c(name = "LearningMulti"),
          definition = function(name, ...) {
              return( Call(name = as(object = name, 
                                     Class = "DynTxRegime"), ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "cvInfo",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              return( .cycleList(object = object@optimResult, 
                                 func = "cvInfo", ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "coef",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              res1 <- coef(object = as(object = object, 
                                       Class = "PropensityObj"), ...)
              res2 <- coef(object = as(object = object, 
                                       Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "estimator",
          signature = c(x = "LearningMulti"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x, 
                                       Class = "DynTxRegime"), ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "fitObject",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              res1 <- fitObject(object = as(object = object, 
                                            Class = "PropensityObj"), ...)
              res2 <- fitObject(object = as(object = object, 
                                            Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "optimObj",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              return( .cycleList(object = object@optimResult, 
                                 func = "optimObj", ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "optTx",
          signature = c(x = "LearningMulti",
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {

              # process tx information for new data
              txObj <- .newTxObj(txName = .getTxName(x@txInfo),
                                 data = newdata,
                                 fSet = .getSubsetRule(x@txInfo),
                                 suppress = TRUE)

              # extract new patient subset assignments
              ptsSubset <- .getPtsSubset(object = txObj)

              # retrieve original analysis subsets
              subsets <- .getSubsets(object = x@txInfo)

              optVec <- rep(x = NA, times = nrow(x = newdata))
              df <- rep(x = NA, times = nrow(x = newdata))

              for (i in 1L:length(x = x@optimResult)) {

                # extract subsets included in OptimStep analysis
                nm <- names(x = x@optimResult)[i]
                nm <- unlist(x = strsplit(x = nm, split = ","))

                # identify patients assigned to this subset
                usePts <- ptsSubset %in% nm
                if (!any(usePts)) next

                # predict optimal tx and decision function based on 
                # learned regime
                opt <- .predictOptimalTx(x = x@optimResult[[ i ]], 
                                         newdata = newdata[usePts,])

                # store optimal results
                optVec[usePts] <- opt$optimalTx
                df[usePts] <- opt$decisionFunc
              } 

              # optimal tx returned as -1/+1; convert to original coding
              topt <- .convertFromBinary(txObj = txObj,
                                         txVec = optVec)

              return( list("optimalTx" = topt,
                           "decisionFunc" = df) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "optTx",
          signature = c(x = "LearningMulti",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x, Class = "DynTxRegime"), ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "outcome",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              return( outcome(object = as(object = object, 
                                          Class = "OutcomeObj"), ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "plot",
          signature = c(x = "LearningMulti"),
          definition = function(x, suppress = FALSE, ...) {
              plot(x = as(object = x, Class = "PropensityObj"), 
                   suppress = suppress, ...)
              plot(x = as(object = x, Class = "OutcomeObj"), 
                   suppress = suppress, ...)
            })

#' @rdname LearningMulti-methods
setMethod(f = "print",
          signature = c(x = "LearningMulti"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "PropensityObj"), ...)
              print(x = as(object = x, Class = "OutcomeObj"), ...)
              print(x = x@optimResult, ...)
            })

#' @rdname LearningMulti-methods
setMethod(f = "propen",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              return( propen(object = as(object = object, 
                                         Class = "PropensityObj"), ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "regimeCoef",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              return( .cycleList(object = object@optimResult, 
                                 func = "regimeCoef", ...) )
            })

#' @rdname LearningMulti-methods
setMethod(f = "show",
          signature = c(object = "LearningMulti"),
          definition = function(object) {
              show(object = as(object = object, Class = "PropensityObj"))
              show(object = as(object = object, Class = "OutcomeObj"))
              show(object = object@optimResult)
            })

#' @rdname LearningMulti-methods
setMethod(f = "summary",
          signature = c(object = "LearningMulti"),
          definition = function(object, ...) {
              res1 <- summary(object = as(object = object, 
                                          Class = "PropensityObj"), ...)
              res2 <- summary(object = as(object = object, 
                                          Class = "OutcomeObj"), ...)
              res3 <- .cycleList(object = object@optimResult, 
                                 func = "summary", ...)

              return( c(res1, res2, res3) )
            })
