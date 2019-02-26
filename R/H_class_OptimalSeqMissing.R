# October 23, 2018

#' Class Contains Results for the Missing Data IPW/AIPW Method
#'
#' Methods for single decision point analyses. Class inherits directly from
#' \code{OptimalSeq} and all methods defined for objects of class \code{OptimaSeq}
#' are defined for this class.
#'
#' @name OptimalSeqMissing-class
#'
#' @include H_class_OptimalSeq.R
setClass("OptimalSeqMissing",
         slots = c(analysis = "OptimalSeq"))

##########
## METHODS
##########

#  @param moPropen A modelObj for propensity
#  @param moMain A modelObj for main effects of outcome
#  @param moCont A modelObj for contrasts of outcome
#  @param data A data.frame of covariates and treatment history
#  @param response A outcome of interest
#  @param txName A character name of treatment in data
#  @param regimes A regimes to be fit
#  @param fSet A subsetting function
#  @param iter A iteration max for outcome regression
#  @param suppress A T/F indicating if screen prints are suppressed
#  @param argList A arguments to be set in rgenoud
.OptimalSeqMissing <- function(moPropen,
                               moMain,
                               moCont,
                               data,
                               response,
                               txName,
                               regimesObj,
                               fSet,
                               iter,
                               suppress,
                               argsList, ...) {

  if (!suppress) cat("Value Search - Missing Data Perspective.\n")

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

  # complete genetic algorithm
  geneticObj <- .geneticStep(txObj = txObj, 
                             regimesObj = regimesObj, 
                             propenObj = propenObj,
                             outcomeObj = outcomeObj,
                             response = response,
                             data = data,
                             argsList = argsList,
                             suppress = suppress)

  # reset regime parameters to optimal values
  regimesObj <- .setPars(object = regimesObj, pars = geneticObj$par)

  # predict optimal treatment
  optTx <- .predictOptimalTx(x = regimesObj, newdata = data)
  optTx <- .convertTx(object = txObj, txVec = optTx)

  optObj <- new(Class = "OptimalObj",
                optimal = new(Class = "OptimalInfo",
                              "optimalTx" = optTx,
                              "estimatedValue" = geneticObj$value,
                              "decisionFunc" = NA))
  dtrObj <- new(Class = "DynTxRegime",
                optObj,
                "call" = NULL)

  if (!suppress) {
    cat("\n")
    print(x = optObj)
  }

  return( new(Class = "OptimalSeqMissing",
              "analysis" = new(Class = "OptimalSeq",
                               "genetic" = geneticObj,
                               propenObj,
                               outcomeObj,
                               regimesObj,
                               dtrObj)) )

}

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        fSet     = "NULL"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        fSet     = "NULL"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        fSet     = "NULL"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        fSet     = "NULL"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "ModelObj_SubsetList",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "NULL",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_SubsetList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "ModelObj_SubsetList",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "ModelObj_SubsetList",
                        moCont   = "NULL",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "ModelObj_SubsetList",
                        fSet     = "function"),
          definition = .OptimalSeqMissing)

#' Methods Available for Objects of Class \code{OptimalSeqMissing}
#'
#' @name OptimalSeqMissing-methods
#'
#' @keywords internal
NULL

#' \code{Call(name)}
#'   returns the unevaluated call to method
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "Call",
          signature = c(name = "OptimalSeqMissing"),
          definition = function(name, ...) {
              return( Call(name = name@analysis) )
            })

#' Retrieve coefficients of fits
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "coef",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) {
              return( coef(object = object@analysis, ...) )
            })

#' \code{DTRstep(x)}
#'   print statement indicating the coarsened data perspective
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "DTRstep",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object) {
              cat("Value Search - Missing Data Perspective\n")
            })

#' \code{estimator(x)}
#'   retrieves the estimated value.  Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "estimator",
          signature = c(x = "OptimalSeqMissing"),
          definition = function(x, ...) {
              return( estimator(x = x@analysis, ...) )
            })

#' \code{fitObject(object)}
#'   retrieves value objects of model functions. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "fitObject",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) {
              return( fitObject(object = object@analysis, ...) )
            })

#' \code{genetic(object)}
#'   retrieves genetic algorithm results. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "genetic",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) { 
              return( genetic(object = object@analysis, ...) )
            })

#' Predict Optimal Treatment and Decision Function Based on a 
#' Missing Data AIPW Analysis
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqMissing",
                        newdata = "data.frame"),
          definition = function (x, newdata, ...) {
              return( optTx(x = x@analysis, newdata = newdata, ...) )
            })

#' \code{optTx(x)}
#'   retrieves the optimal tx. Calls method defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqMissing",
                        newdata = "missing"),
          definition = function (x, newdata, ...) {
              return( optTx(x = x@analysis, ...) )
            })

#' \code{outcome(object)}
#'   retrieves value object returned by outcome model functions. Calls method 
#'   defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "outcome",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) {
              return( outcome(object = object@analysis) )
            })

#' \code{plot(x,suppress)}
#'   generates plot for model functions. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "plot",
          signature = c(x = "OptimalSeqMissing"),
          definition = function(x, suppress = FALSE, ...) {
              plot(x = x@analysis, 
                   suppress = suppress, ...)
            })

#' \code{print(x)}
#'   Extends method defined for \code{OptimalSeq} to include DTRStep()
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "print",
          signature = c(x = "OptimalSeqMissing"),
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = x@analysis, ...)
            })

#' \code{propen(object)}
#'   retrieves value object returned by propensity model functions. Calls method 
#'   defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "propen",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) {
              return( propen(object = object@analysis) )
            })

#' \code{regimeCoef(object)}
#'   retrieves estimated tx regime parameters. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "regimeCoef",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@analysis) )
            })

#' \code{show(object)}
#'   Extends method defined for \code{OptimalSeq} to include DTRStep()
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "show",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object) {
              DTRstep(object = object)
              show(object = object@analysis)
            })

#' \code{summary(object)}
#'   retrieves summary information. Calls method defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqMissing-methods
setMethod(f = "summary",
          signature = c(object = "OptimalSeqMissing"),
          definition = function(object, ...) {
              return( summary(object = object@analysis, ...) )
            })

#' Define the Single Decision Point Objective Function
#'
#' @rdname seqFunc
setMethod(f = ".seqFunc",
          signature = c("eta" = "numeric",
                        "txObj" = "TxObj"),
          definition = function(eta,
                                txObj,
                                regimesObj,
                                l.data,
                                outcomeObj, 
                                propenObj, 
                                response) {

              # reset regime parameters to current estimate
              regimesObj <- .setPars(object = regimesObj, pars = eta)

              nSamples <- nrow(x = l.data)

              # predict the optimal tx based on the new regime parameters
              optTx <- .predictOptimalTx(x = regimesObj, newdata = l.data)

              optTx <- .convertTx(object = txObj, txVec = optTx)

              .validTx(object = txObj, txVec = optTx)

              # ind[,i] = 1 if patient tx in accordance with regime
              #         = 0 if patient tx not in accordance with regime
              ind <- .compareTx(object = txObj, 
                                vec1 = l.data[,.getTxName(object = txObj)], 
                                vec2 = optTx)

              # set data tx to recommended tx
              l.data[,.getTxName(object = txObj)] <- optTx

              # get propensity for recommended tx
              prWgt <- .getPrWgt(propenObj = propenObj, 
                                 txObj = txObj,  
                                 data = l.data)

              # get estimated outcome for each tx
              mu <- .predictAll(object = outcomeObj, 
                                newdata = l.data)$decisionFunc

              # extract estimated outcome for recommended tx
              tst <- match(x = optTx, table = colnames(x = mu))
              tstNA <- is.na(x = tst)
              tst[tstNA] <- 1L

              mu <- mu[cbind(1:nSamples,tst)]

              # augmentation term of value estimate
              DR <- - {ind - prWgt} / prWgt * mu

              # value estimate
              mn <- sum(DR + ind / prWgt * response, na.rm = TRUE) / nSamples

              return( mn )
            })

#' Retrieve Propensity for Tx Received
#'
#' @param propenObj a PropensityObj
#' @param txObj a TxObj
#' @param data a data.frame
#'
#' @return vector of propensity for tx received.
#'
#' @name getPrWgt
#'
#' @keywords internal
.getPrWgt <- function(propenObj, txObj, data) {

  # calculate propensity for all tx
  pr <- .predictAll(object = propenObj, newdata = data)

  # match column names of pr matrix to tx received
  tst <- match(x = data[,.getTxName(object = txObj)], table = colnames(x = pr))
  if (any(is.na(x = tst))) stop("unable to match tx to propensity")
  if (length(x = tst) != nrow(x = data)) stop("dim problem")

  # take propensity for tx received
  prWgt <- pr[cbind(1L:nrow(x = data),tst)]

  if (any(is.infinite(x = 1.0/prWgt))) {
    stop("zero valued propensity encountered")
  }

  if (any(is.na(x = prWgt))) stop("NA valued propensity encountered")
  if (any(is.nan(x = prWgt))) stop("NaN valued propensity encountered")

  return( prWgt )
}
