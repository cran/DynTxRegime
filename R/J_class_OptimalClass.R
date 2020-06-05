# December 21, 2018

#' Class \code{OptimalClassObj}
#'
#' Class \code{OptimalClassObj} contains results for a single decision point 
#'   when estimates are obtained from the classification perspective.
#'   Objects of this class are returned by optimalClass().
#'
#' @slot class Results of the classification step.
#' @slot outcome Results of the outcome regression step.
#' @slot propen Results of the propensity step.
#' @slot optimal Estimated optimal tx and value
#' @slot Call Unevaluated call.
#'
#' @name OptimalClassObj-class
#'
#' @template outcomeOnly
#' @template propenOnly
#' @template classifOnly
#' @template regression
#' @template DynTxRegime_methods
setClass(Class = "OptimalClassObj",
         contains = c("ClassificationObj",
                      "OutcomeObj",
                      "PropensityObj",
                      "DynTxRegime"))

#' Class \code{OptimalClass}
#'
#' Class \code{OptimalClass} contains results for a single decision point 
#'   when estimates are obtained from the classification perspective.
#'   Objects of this class are returned by optimalClass().
#'
#' @slot step Step in the algorithm.
#' @slot analysis Analysis results.
#'
#' @name OptimalClass-class
#'
#' @template outcomeOnly
#' @template propenOnly
#' @template classifOnly
#' @template regression
#' @template DynTxRegime_methods
setClass(Class = "OptimalClass",
         slots = c("step" = "integer",
                   "analysis" = "ANY"))

##########
## GENERICS
##########

#' Estimate the Optimal Treatment and Value Using Classification
#'
#' Method dispatches the appropriate function to obtain estimates for the
#'   optimal treatment and value using classification.
#'
#' @name newOptimalClass
#'
#' @keywords internal
setGeneric(name = ".newOptimalClass",
           def = function(response, ...) {
               standardGeneric(f = ".newOptimalClass")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{OptimalClass}
#'
#' @name OptimalClass-methods
#'
#' @keywords internal
NULL


#' Perform Classification Step
#'
#' @name .optimalClass
#'
#' @param moPropen model object(s) for propensity regression
#' @param moMain model object(s) for main effects of outcome regression or NULL
#' @param moCont model object(s) for contrasts of outcome regression or NULL
#' @param moClass model object(s) for classification procedure
#' @param data data.frame of covariates and treatment history
#' @param response vector of responses
#' @param txName character of column header of data containing tx
#' @param iter maximum number of iterations for outcome regression or NULL
#' @param fSet function defining subsets or NULL
#' @param suppress T/F indicating screen printing preference
#' @param step integer indicating step of algorithm
#'
#' @return an object of class OptimalClass
#'
#' @keywords internal
.optimalClass <- function(moPropen,
                          moMain,
                          moCont,
                          moClass,
                          data,
                          response,
                          txName,
                          iter,
                          fSet,
                          suppress,
                          step) {

  if (!suppress) message("Classification Perspective.")

  # process tx information
  txObj <- .newTxObj(fSet = fSet,
                     txName = txName,
                     data = data,
                     suppress = suppress)

  # complete propensity regression
  propenObj <- .newPropensityObj(moPropen = moPropen,
                                 txObj = txObj,
                                 data = data,
                                 suppress = suppress)

  # complete outcome regression
  outcomeObj <- .newOutcomeObj(moMain = moMain,
                               moCont = moCont,
                               data = data,
                               response = response,
                               txObj = txObj,
                               iter = iter,
                               suppress = suppress)

  # calculate contrasts
  contrast <- .contrastFunc(txObj = txObj,
                            outcomeObj = outcomeObj,
                            propenObj = propenObj,
                            data = data,
                            response = response)

  # add the weights for the classification to data
  data$wgt <- abs(x = contrast)

  # add weights variable to regression input
  if (is(object = moClass, class2 = "ModelObj_SubsetList")) {
    nms <- names(x = moClass)
    moC <- list()
    for (i in 1L:length(x = moClass)) {
      cArgs <- modelObj::solverArgs(moClass[[ i ]])
      cArgs[[ "weights" ]] <- quote(expr = wgt)
      modelObj::solverArgs(moClass[[ i ]]) <- cArgs
      moC[[ nms[i]] ] <- moClass[[ i ]]
    }
    moClass <- new(Class = "ModelObj_SubsetList", moC)
  } else {
    cArgs <- modelObj::solverArgs(moClass)
    cArgs[[ "weights" ]] <- quote(expr = wgt)
    modelObj::solverArgs(moClass) <- cArgs
  }

  # convert contrast to 0/1 category
  ZinternalZ <- as.integer(x = {contrast > -1.5e-8})
  ZinternalZ <- factor(x = ZinternalZ, levels = c(0,1))

  # complete classification step
  classObj <- .newClassificationObj(moClass = moClass,
                                    data = data,
                                    response = ZinternalZ,
                                    txObj = txObj,
                                    suppress = suppress)

  # recommended tx in coding of prediction method
  opt <- predict(object = classObj, newdata = data)

  # predictions should be 0/1 or NA (for singletons)
  if (!all(opt %in% list(0,1,"0","1") | is.na(x = opt))) {
    stop("predict should return 0/1")
  }

  oHold <- opt

  if (any(is.na(x = opt))) {
    opt[is.na(x = opt)] <- data[is.na(x = opt), txName]
  }

  # get estimated outcome for each tx; matrix is in binary coding
  mu <- .getOutcome(outcomeObj = outcomeObj, 
                    txObj = txObj, 
                    data = data)

  tstNA <- is.na(x = mu)
  mu[tstNA] <- 0.0

  # get propensity for recommended tx
  prWgt <- .getPrWgt(propenObj = propenObj, 
                     txObj = txObj,  
                     data = data)

  # convert received tx to binary -1/1
  txVec <- .convertToBinary(txObj = txObj, 
                            txVec = data[,txName],
                            data = data)

  if (is(object = txObj@txInfo, class2 = "TxSubset")) {
    optTx <- NULL
    subsets <- .getSubsets(object = txObj)
    ptsSubset <- .getPtsSubset(object = txObj)
    for (i in 1L:length(x = subsets)) {
      usePts <- ptsSubset == names(x = subsets)[i]
      if (length(x = subsets[[ i ]]) == 2L) {
        base <- oHold %in% c("0",0)
        optTx[usePts & base] <- subsets[[ i ]][1L]
        optTx[usePts & !base] <- subsets[[ i ]][2L]
      } else {
        optTx[usePts] <- subsets[[ i ]][1L]
      }
    }
  } else {
    superset <- .getSuperset(object = txObj)
    optTx <- NULL
    base <- oHold %in% c("0",0)
    optTx[base] <- superset[1L]
    optTx[!base] <- superset[2L]
  }

  cTilde <- optTx == data[,txName]

  qTilde <- .predictMu(object = outcomeObj, data = data)
  qTilde <- qTilde[cbind(1L:nrow(x = data), 
                         match(x = optTx, table = colnames(x = qTilde)))]

  # contrast function
  value <- cTilde * response / prWgt - {cTilde - prWgt}/prWgt * qTilde

  if (is(object = txObj, class2 = "TxInfoWithSubsets")) {
    singles <- .getSingleton(object = txObj)
    singles <- singles & rowSums(abs(x = mu)) <= 1e-8 & cTilde
    value[singles] <- {response / prWgt}[singles]
  }

  if (is.null(x = moMain) && is.null(x = moCont)) value[!cTilde] <- NA

  optObj <- new(Class = "OptimalObj",
                optimal = new(Class = "OptimalInfo",
                              "decisionFunc"   = NA,
                              "estimatedValue" = value,
                              "optimalTx"      = optTx))

  if (!suppress) {
    print(x = optObj)
  }

  dtrObj <- new(Class = "DynTxRegime",
                optObj,
                "call" = NULL)

  return( new(Class = "OptimalClass",
              step = step,
              analysis = new(Class = "OptimalClassObj",
                             classObj,
                             propenObj,
                             outcomeObj,
                             dtrObj)) )

}

#' @rdname newOptimalClass
setMethod(f = ".newOptimalClass",
          signature = c(response = "vector"),
          definition = function(moPropen,
                                moMain,
                                moCont,
                                moClass,
                                data,
                                response,
                                txName,
                                iter,
                                fSet,
                                suppress, ...) {

              if (!suppress) message("First step of the Classification Algorithm.")

              analysis <- .optimalClass(moPropen = moPropen,
                                        moMain = moMain,
                                        moCont = moCont,
                                        moClass = moClass,
                                        data = data,
                                        response = response,
                                        txName = txName,
                                        iter = iter,
                                        fSet = fSet,
                                        suppress = suppress,
                                        step = 1L)
              return( analysis )

            })

#' @rdname newOptimalClass
setMethod(f = ".newOptimalClass",
          signature = c(response = "OptimalClass"),
          definition = function(moPropen,
                                moMain,
                                moCont,
                                moClass,
                                data,
                                response,
                                txName,
                                iter,
                                fSet,
                                suppress, ...) {

              step <- response@step + 1L

              if (!suppress) {
                message("Step ", step, " of the Classification Algorithm")
              }

              tst <- is.na(x = response@analysis@optimal@estimatedValue)
              if (sum(tst) > 0L) {
                message("removed ", sum(tst), 
                        " individuals that did not follow estimated optimal",
                        " tx in preceeding step(s)")
              }

              analysis <- .optimalClass(moPropen = moPropen,
                                        moMain = moMain,
                                        moCont = moCont,
                                        moClass = moClass,
                                        data = data[!tst,],
                                        response = response@analysis@optimal@estimatedValue[!tst],
                                        txName = txName,
                                        iter = iter,
                                        fSet = fSet,
                                        suppress = suppress,
                                        step = step)

              if (any(tst)) {
                estV <- rep(x = NA, times = nrow(x = data))
                estV[!tst] <- analysis@analysis@optimal@estimatedValue
                analysis@analysis@optimal@estimatedValue <- estV

                estTx <- rep(x = NA, times = nrow(x = data))
                estTx[!tst] <- analysis@analysis@optimal@optimalTx
                analysis@analysis@optimal@optimalTx <- estTx
              }

              return( analysis )

            })

.contrastFunc <- function(txObj,
                          outcomeObj,
                          propenObj,
                          data,
                          response) {

  # get propensity for tx received
  prWgt <- .getPrWgt(propenObj = propenObj, 
                     txObj = txObj,  
                     data = data)

  # get estimated outcome for each tx; matrix is in binary coding;
  # may contain NA if singletons present in data
  mu <- .getOutcome2(outcomeObj = outcomeObj, 
                     txObj = txObj, 
                     data = data)

  mu[is.na(x = mu)] <- 0.0


  # convert received tx to binary -1/1
  txVec <- .convertToBinary(txObj = txObj, 
                            txVec = data[,.getTxName(object = txObj)],
                            data = data)

  n <- nrow(x = data)

  # identify mu for tx received
  rTx <- rep(x = 2L, times = n)
  rTx[txVec < -0.5] <- 1L

  # identify mu for tx not received
  sTx <- rep(x = 1L, times = n)
  sTx[txVec < -0.5] <- 2L

  # contrast function
  ym <- txVec / prWgt * { response - 
        {1.0 - prWgt} * mu[cbind(1L:n,rTx)] - prWgt * mu[cbind(1L:n,sTx)]}

  if (is(object = txObj, class2 = "TxInfoWithSubsets")) {
    ym[.getSingleton(object = txObj)] <- 0.0
  }

  return( ym )
}

#' @rdname DynTxRegime-internal-api
setMethod(f = "Call",
          signature = c(name = "OptimalClass"),
          definition = function(name, ...) {
              return( Call(name = as(object = name@analysis, 
                                     Class = "DynTxRegime"), ...) )
            })

#' @rdname classif
setMethod(f = "classif",
          signature = c(object = "OptimalClass"),
          definition = function(object, ...) {
              return( classif(object = as(object = object@analysis, 
                                          Class = "ClassificationObj")) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "coef",
          signature = c(object = "OptimalClass"),
          definition = function(object, ...) {
              res1 <- coef(object = as(object = object@analysis, 
                                       Class = "PropensityObj"), ...)
              res2 <- coef(object = as(object = object@analysis, 
                                       Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep",
          signature = c(object = "OptimalClass"),
          definition = function(object) {
              cat("Classification Perspective - Step", object@step,"\n")
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "estimator",
          signature = c(x = "OptimalClass"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x@analysis, 
                                       Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "fitObject",
          signature = c(object = "OptimalClass"),
          definition = function(object, ...) {
              res1 <- fitObject(object = as(object = object@analysis, 
                                            Class = "PropensityObj"), ...)
              res2 <- fitObject(object = as(object = object@analysis, 
                                            Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "OptimalClass",
                        newdata = "data.frame"),
          definition = function (x, newdata, ...) {
              return( .predictAll(object = as(object = x@analysis, 
                                              Class = "ClassificationObj"),
                                  newdata = newdata) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "OptimalClass",
                        newdata = "missing"),
          definition = function (x, newdata, ...) {
              return( optTx(x = as(object = x@analysis, Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "OptimalClass"),
          definition = function(object, ...) {
              return( outcome(object = as(object = object@analysis, 
                                          Class = "OutcomeObj"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "plot",
          signature = c(x = "OptimalClass", y = "missing"),
          definition = function(x, suppress = FALSE, ...) {
              plot(x = as(object = x@analysis, Class = "PropensityObj"), 
                   suppress = suppress, ...)
              plot(x = as(object = x@analysis, Class = "OutcomeObj"), 
                   suppress = suppress, ...)
            })

#' @rdname OptimalClass-methods
setMethod(f = "print",
          signature = c(x = "OptimalClass"),
          definition = function(x, ...) {
              print(x = as(object = x@analysis, Class = "PropensityObj"), ...)
              print(x = as(object = x@analysis, Class = "OutcomeObj"), ...)
              print(x = as(object = x@analysis, Class = "ClassificationObj"), ...)
              print(x = as(object = x@analysis, Class = "DynTxRegime"), ...)
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "propen",
          signature = c(object = "OptimalClass"),
          definition = function(object, ...) {
              return( propen(object = as(object = object@analysis, 
                                         Class = "PropensityObj")) )
            })

#' @rdname OptimalClass-methods
setMethod(f = "show",
          signature = c(object = "OptimalClass"),
          definition = function(object) {
              show(object = as(object = object@analysis, Class = "PropensityObj"))
              show(object = as(object = object@analysis, Class = "OutcomeObj"))
              show(object = as(object = object@analysis, Class = "ClassificationObj"))
              show(object = as(object = object@analysis, Class = "DynTxRegime"))
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "summary",
          signature = c(object = "OptimalClass"),
          definition = function(object, ...) {
              res1 <- summary(object = as(object = object@analysis, 
                                          Class = "PropensityObj"), ...)
              res2 <- summary(object = as(object = object@analysis, 
                                          Class = "OutcomeObj"))
              res3 <- summary(object = as(object = object@analysis,  
                                          Class = "ClassificationObj"), ...)
              res4 <- summary(object = as(object = object@analysis,  
                                          Class = "DynTxRegime"), ...)
              return( c(res1, res2, res3, res4) )
            })

#' Retrieve Outcome for Both Tx Options When Tx is Binary
#'
#' @param outcomeObj a OutcomeObj
#' @param txObj a TxObj
#' @param data a data.frame
#'
#' @return matrix of outcome under binary tx.
#'
#' @name getOutcome
#'
#' @keywords internal
.getOutcome <- function(outcomeObj, txObj, data) {

  or <- .predictMu(object = outcomeObj, data = data)

  # if only 2 tx, return as is
  if (ncol(x = or) == 2L) return( or )

  if (!is(object = txObj, class2 = "TxSubset") &&
      !is(object = txObj, class2 = "TxInfoWithSubsets")) stop("wrong txObj")

  # extract subsets
  subsets <- .getSubsets(object = txObj)

  # extract patient subset
  ptsSubset <- .getPtsSubset(object = txObj)

  orWgt <- matrix(data = 0.0, nrow = nrow(x = data), ncol = 2L)

  for (i in 1L:length(x = subsets)) {

    # identify patients in the current subset
    usePts <- ptsSubset == names(x = subsets)[i]

    # match all tx values of the current subset to a column of or
    tst <- match(x = subsets[[ i ]], table = colnames(x = or))

    if (length(x = tst) > 2L) stop("dim problem")
    # if a tx is not matched, throw error
    if (any(is.na(x = tst))) {
      stop("unable to match tx to outcome")
    }
    if (length(x = tst) != length(x = subsets[[ i ]])) stop("dim problem")
    if (length(x = tst) > 2L) stop("dim problem")

    for (j in 1L:length(x = tst)) {
      orWgt[usePts, j] <- or[usePts, tst[j]]
    }
  }

  return( orWgt )
}

#' Retrieve Outcome for Both Tx Options When Tx is Binary
#'
#' @param outcomeObj a OutcomeObj
#' @param txObj a TxObj
#' @param data a data.frame
#'
#' @return matrix of outcome under binary tx.
#'
#' @name getOutcome
#'
#' @keywords internal
.getOutcome2 <- function(outcomeObj, txObj, data) {

  or <- .predictAll(object = outcomeObj, newdata = data)$decisionFunc

  # if only 2 tx, return as is
  if (ncol(x = or) == 2L) return( or )

  if (!is(object = txObj, class2 = "TxSubset") &&
      !is(object = txObj, class2 = "TxInfoWithSubsets")) stop("wrong txObj")

  # extract subsets
  subsets <- .getSubsets(object = txObj)

  # extract patient subset
  ptsSubset <- .getPtsSubset(object = txObj)

  orWgt <- matrix(data = 0.0, nrow = nrow(x = data), ncol = 2L)

  for (i in 1L:length(x = subsets)) {

    # identify patients in the current subset
    usePts <- ptsSubset == names(x = subsets)[i]

    # match all tx values of the current subset to a column of or
    tst <- match(x = subsets[[ i ]], table = colnames(x = or))

    if (length(x = tst) > 2L) stop("dim problem")
    # if a tx is not matched, throw error
    if (any(is.na(x = tst))) {
      stop("unable to match tx to outcome")
    }
    if (length(x = tst) != length(x = subsets[[ i ]])) stop("dim problem")
    if (length(x = tst) > 2L) stop("dim problem")

    for (j in 1L:length(x = tst)) {
      orWgt[usePts, j] <- or[usePts, tst[j]]
    }
  }

  return( orWgt )
}
