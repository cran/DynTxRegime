#' Class \code{Learning}
#'
#' Class \code{Learning} contains results for a learning analysis with one 
#'   regime.
#'
#' @name Learning-class
#' @slot txInfo Feasible tx information
#' @slot propen Propensity regression analysis
#' @slot outcome Outcome regression analysis
#' @slot optim Optimization analysis
#'
#' @keywords internal
#'
#' @include O_LearningObject.R
setClass(Class = "Learning",
         contains = c("TxObj", "PropensityObj", "OutcomeObj", "OptimStep"))

##########
## GENERICS
##########

#' Complete a Learning Analysis
#'
#' Performs a weighted learning analysis.
#'
#' @rdname newLearning
#'
#' @param fSet NULL or function defining feasible tx
#' @param kernel a Kernel object
#'
#' @keywords internal
setGeneric(name = ".newLearning",
           def = function(fSet, kernel, ...) { 
               standardGeneric(f = ".newLearning") 
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{Learning}
#'
#' @name Learning-methods
#'
#' @keywords internal
NULL

.newLearningFunc <- function(fSet, 
                             kernel,
                             ..., 
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
                             index = NULL) {

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
  txVec <- .convertToBinary(txObj = txObj, 
                            txVec = data[,txName],
                            data = data)

  # complete propensity regression
  propenObj <- .newPropensityObj(moPropen = moPropen,
                                 txObj = txObj,
                                 data = data,
                                 suppress = {suppress == 0L})

  # get propensity for tx received
  prWgt <- .getPrWgt(propenObj = propenObj, txObj = txObj, data = data)

  # include propensity weights
  prWgt <- prWgt * prodPi

  # complete outcome regression
  outcomeObj <- .newOutcomeObj(moMain = moMain,
                               moCont = moCont,
                               data = data,
                               response = response,
                               txObj = txObj,
                               iter = iter,
                               suppress = {suppress  == 0L})

  # get estimated outcome for each tx
  mu <- .getOutcome(outcomeObj = outcomeObj, txObj = txObj, data = data)

  # create argument list for createObj function
  argList <- list()
  argList[[ "kernel" ]] <- kernel
  argList[[ "txVec" ]] <- txVec
  argList[[ "response" ]] <- response
  argList[[ "prWgt" ]] <- prWgt
  argList[[ "surrogate" ]] <- surrogate
  argList[[ "guess" ]] <- guess
  argList[[ "mu" ]] <- mu

  # create method object for specific learning method
  methodObj <- do.call(what = createObj, args = argList)

  # subset method object to those with >1 tx option and in index
  if (is(object = txObj, class2 = "TxInfoWithSubsets")) {
    isSingle <- .getSingleton(object = txObj)
  } else {
    isSingle <- FALSE
  }

  methodObj <- .subsetObject(methodObject = methodObj,
                             subset = !isSingle & index)

  # perform cross-validation and optimization steps
  optimStep <- .OptimStep(methodObject = methodObj,
                          lambdas = lambdas,
                          cvFolds = cvFolds,
                          txVec = txVec[!isSingle & index],
                          suppress = suppress, ...)

  # retrieve optimal tx and decision function
  opt <- optTx(x = optimStep)

  # extend to full size of data
  optVec <- rep(x = NA, times = nrow(x = data))
  optVec[!isSingle & index] <- opt$optimalTx

  df <- rep(x = NA, times = nrow(x = data))
  df[!isSingle & index] <- opt$decisionFunc


  # optimal tx is returned as -1/1. Singles not included in
  # fit of regime parameters are currently NA
  optOrg <- .convertFromBinary(txObj = txObj, 
                               txVec = optVec)

  # ensure that singletons are included appropriately in optVec
  optVec <- rep(x = NA, times = nrow(x = data))
  if (is(object = optOrg, class2 = "factor")) {
    optVec[!isSingle & index] <- levels(optOrg)[optOrg][!isSingle & index]
  } else {
    optVec[!isSingle & index] <- optOrg[!isSingle & index]
  }

  if (is(object = txObj, class2 = "TxInfoWithSubsets")) {
    subsets <- .getSubsets(object = txObj)
    ptsSubset <- .getPtsSubset(object = txObj)
    for (i in 1L:length(x = subsets)) {
      if (length(x = subsets[[ i ]]) != 1L) next
      usePts <- ptsSubset == names(subsets)[i]

      optVec[usePts & index] <- subsets[[ i ]]
    }
  }

  # value functions expect -1/1 notation convert full data back to binary
  txVec <- .convertToBinary(txObj = txObj, 
                            txVec = optVec,
                            data = data)
  txVec[is.na(x = optVec)] <- NA

  # create method object with full data
  methodObj <- do.call(what = createObj, args = argList)

  # re-calculate estimated value
  value <- .valueFunc(methodObject = methodObj, optTx = txVec)

  # replace optimal estimates
  optimStep@optimal@optimalTx <- optVec
  optimStep@optimal@decisionFunc <- df
  optimStep@optimal@estimatedValue <- value

  if (suppress != 0L) print(x = optimStep@optimal)

  return( new(Class = "Learning", 
              txObj,  
              propenObj,  
              outcomeObj,  
              optimStep) )
}

#' Complete a Learning Analysis for One Regime and No Subsets
#'
#' @rdname newLearning
#'
#' @param moPropen modelObj for propensity model
#' @param moMain modelObj for main effects of outcome model
#' @param moCont modelObj for contrasts of outcome model
#' @param data data.frame of covariates
#' @param response Vector of responses
#' @param txName Tx variable column header in data
#' @param lambdas Tuning parameter(s)
#' @param cvFolds Number of cross-validation folds
#' @param kernel Kernel object or SubsetList
#' @param fSet NULL or function defining subset rules
#' @param iter Maximum number of iterations for outcome regression
#' @param surrogate Surrogate object
#' @param suppress T/F indicating if prints to screen are executed
#' @param guess optional numeric vector providing starting values for
#'   optimization methods
#' @param createObj A function name defining the method object for a
#'   specific learning algorithm
#' @param prodPi A vector of propensity weights
#' @param index The subset of individuals to be included in learning
#' @param ...  Additional inputs for optimization
#'
#' @return A \code{Learning} object
setMethod(f = ".newLearning",
          signature = c(fSet = "NULL",
                        kernel = "Kernel"),
          definition = .newLearningFunc)

#' @rdname newLearning
setMethod(f = ".newLearning",
          signature = c(fSet = "function",
                        kernel = "Kernel"),
          definition = .newLearningFunc)

#' @rdname Learning-methods
setMethod(f = "Call",
          signature = c(name = "Learning"),
          definition = function(name, ...) {
              return( Call(name = as(object = name, 
                                     Class = "OptimStep"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "cvInfo",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              return( cvInfo(object = as(object = object, 
                                         Class = "OptimStep"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "coef",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              res1 <- coef(object = as(object = object, 
                                       Class = "PropensityObj"), ...)
              res2 <- coef(object = as(object = object, 
                                       Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname Learning-methods
setMethod(f = "estimator",
          signature = c(x = "Learning"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x, 
                                       Class = "OptimStep"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "fitObject",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              res1 <- fitObject(object = as(object = object, 
                                            Class = "PropensityObj"), ...)
              res2 <- fitObject(object = as(object = object, 
                                            Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname Learning-methods
setMethod(f = "optimObj",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              return( optimObj(object = as(object = object, 
                                           Class = "OptimStep"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "optTx",
          signature = c(x = "Learning",
                        newdata = "data.frame"),
          definition = function(x, newdata) {

              txName <- .getTxName(object = x@txInfo)
              if (!any(colnames(x = newdata) %in% txName)) {
                nms <- colnames(x = newdata)
                newdata <- cbind(newdata, .getSuperset(object = x@txInfo)[1L])
                colnames(x = newdata) <- c(nms, txName)
              } else {
                newdata[,txName] <- .getSuperset(object = x@txInfo)[1L]
              }

              # predict optimal tx and decision function based on learned regime
              opt <- .predictOptimalTx(x = as(object = x, Class = "OptimStep"), 
                                       newdata = newdata)

              # process tx information for new data
              txObj <- .newTxObj(txName = .getTxName(object = x@txInfo),
                                 data = newdata,
                                 fSet = .getSubsetRule(object = x@txInfo),
                                 suppress = TRUE, verify=FALSE) 

              txObj@txInfo@superset <- x@txInfo@superset

              # optimal tx returned as -1/+1; convert to original coding
              topt <- .convertFromBinary(txObj = txObj@txInfo,
                                         txVec = opt$optimalTx)

              return( list("optimalTx" = topt,
                           "decisionFunc" = opt$decisionFunc) )
            })

#' @rdname Learning-methods
setMethod(f = "optTx",
          signature = c(x = "Learning",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x, Class = "OptimStep"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "outcome",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              return( outcome(object = as(object = object, 
                                          Class = "OutcomeObj"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "plot",
          signature = c(x = "Learning"),
          definition = function(x, suppress = FALSE, ...) {
              plot(x = as(object = x, Class = "PropensityObj"), 
                   suppress = suppress, ...)
              plot(x = as(object = x, Class = "OutcomeObj"), 
                   suppress = suppress, ...)
            })

#' @rdname Learning-methods
setMethod(f = "print",
          signature = c(x = "Learning"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "PropensityObj"), ...)
              print(x = as(object = x, Class = "OutcomeObj"), ...)
              print(x = as(object = x, Class = "OptimStep"), ...)
            })

#' @rdname Learning-methods
setMethod(f = "propen",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              return( propen(object = as(object = object, 
                                         Class = "PropensityObj"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "regimeCoef",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              return( regimeCoef(object = as(object = object, 
                                             Class = "OptimStep"), ...) )
            })

#' @rdname Learning-methods
setMethod(f = "show",
          signature = c(object = "Learning"),
          definition = function(object) {
              show(object = as(object = object, Class = "PropensityObj"))
              show(object = as(object = object, Class = "OutcomeObj"))
              show(object = as(object = object, Class = "OptimStep"))
            })

#' @rdname Learning-methods
setMethod(f = "summary",
          signature = c(object = "Learning"),
          definition = function(object, ...) {
              res1 <- summary(object = as(object = object, 
                                          Class = "PropensityObj"))
              res2 <- summary(object = as(object = object, 
                                          Class = "OutcomeObj"))
              res3 <- summary(object = as(object = object, 
                                          Class = "OptimStep"))
              return( c(res1, res2, res3) )
            })
