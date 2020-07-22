# October 26, 2018

#' Class Contains Results for the Coarsened Data IPW/AIPW Method
#'
#' Methods for multiple decision point analyses. Class inherits directly from
#'  \code{OptimalSeq} and all methods defined for objects of class \code{OptimaSeq}
#'  are defined for this class.
#'
#' @name OptimalSeqCoarsened-class
#'
#' @include H_class_OptimalSeq.R
setClass("OptimalSeqCoarsened",
         slots = c(analysis = "OptimalSeq",
                   estimatedValue = "numeric"))

.OptimalSeqCoarsened <- function(moPropen,
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

  nDP <- length(x = txName)

  if (!suppress) {
    cat("Value Search - Coarsened Data Perspective", nDP, "Decision Points\n")
  }

  # process tx information
  txObj <- .newTxObj(fSet = fSet,
                     txName = as.list(x = txName),
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
  optimal <- list()

  for (i in 1L:nDP) {
    ot <- .predictOptimalTx(x = regimesObj@regime[[ i ]], newdata = data)
    optTx <- .convertTx(object = txObj@txInfo[[ i ]], txVec = ot)
    data[,.getTxName(object = txObj@txInfo[[ i ]])] <- optTx
    optimal[[ i ]] <- new(Class = "OptimalInfo",
                          estimatedValue = NA,
                          optimalTx = optTx,
                          decisionFunc = NA)
  }

  optimalObj <- new(Class = "DecisionPointList", optimal)

  dtrObj <- new(Class = "DynTxRegime",
                optimal = optimalObj,
                "call" = NULL)

  if (!suppress) {
    cat("\n")
    print(x = optimalObj)
    cat("\nEstimated Value:", geneticObj$value, "\n")
  }

  return( new(Class = "OptimalSeqCoarsened",
              "estimatedValue" = geneticObj$value,
              "analysis" = new(Class = "OptimalSeq",
                               "genetic" = geneticObj,
                               propenObj,
                               outcomeObj,
                               regimesObj,
                               dtrObj)) )

}

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "ModelObj_DecisionPointList",
                        fSet     = "list"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "NULL",
                        fSet     = "list"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_DecisionPointList",
                        fSet     = "list"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        fSet     = "list"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "ModelObj_DecisionPointList",
                        fSet     = "NULL"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "ModelObj_DecisionPointList",
                        moCont   = "NULL",
                        fSet     = "NULL"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "ModelObj_DecisionPointList",
                        fSet     = "NULL"),
          definition = .OptimalSeqCoarsened)

#' @rdname newOptimalSeq
setMethod(f = ".newOptimalSeq",
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        fSet     = "NULL"),
          definition = .OptimalSeqCoarsened)

#' Methods Available for Objects of Class \code{OptimalSeqCoarsened}
#'
#' @name OptimalSeqCoarsened-methods
#'
#' @keywords internal
NULL

#' \code{Call(name)}
#'   returns the unevaluated call to method
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "Call",
          signature = c(name = "OptimalSeqCoarsened"),
          definition = function(name, ...) {
              return( Call(name = name@analysis, ...) )
            })

#' \code{coef(object)}
#'   retrieves coefficients of model functions. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "coef",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              return( coef(object = object@analysis, ...) )
            })

#' \code{DTRstep(x)}
#'   print statement indicating the coarsened data perspective
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "DTRstep",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object) {
              cat("Value Search - Coarsened Data Perspective\n")
            })

#' \code{estimator(x)}
#'   retrieves the estimated value. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "estimator",
          signature = c(x = "OptimalSeqCoarsened"),
          definition = function(x, ...) {
              return( x@estimatedValue )
            })

#' \code{fitObject(object)}
#'   retrieves value objects of model functions. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "fitObject",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              return( fitObject(object = object@analysis, ...) )
            })

#' \code{genetic(object)}
#'   retrieves genetic algorithm results. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "genetic",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              return( genetic(object = object@analysis, ...) )
            })

#' \code{optTx(x,newdata)}
#'   estimates optimal tx. Calls method defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqCoarsened",
                        newdata = "data.frame"),
          definition = function (x, newdata, ..., dp=1) {
              tx <- .predictOptimalTx(x = as(object = x@analysis, Class = "RegimeObj"), 
                                      newdata = newdata, dp = dp,...)
              tx <- .convertTx(x@analysis@propen[[dp]]@txInfo, tx)
              return( list("optimalTx" = tx, "decisionFunc" = NA) )
            })

#' \code{optTx(x)}
#'   retrieves the optimal tx. Calls method defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqCoarsened",
                        newdata = "missing"),
          definition = function (x, newdata, ...) {
                return( .cycleList(object = x@analysis@optimal, 
                                   func = 'optTx',  
                                   trm = 'x', ...) )
            })

#' \code{outcome(object)}
#'   retrieves value object returned by outcome model functions. Calls method 
#'   defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "outcome",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              return( outcome(object = object@analysis) )
            })

#' \code{plot(x,suppress)}
#'   generates plot for model functions. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "plot",
          signature = c(x = "OptimalSeqCoarsened"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = x@analysis, suppress = suppress, ...)
            })

#' \code{print(x)}
#'   Extends method defined for \code{OptimalSeq} to include DTRStep()
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "print",
          signature = c(x = "OptimalSeqCoarsened"),
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = x@analysis, ...)
              cat("Estimated Value:", x@estimatedValue, "\n")
            })

#' \code{propen(object)}
#'   retrieves value object returned by propensity model functions. Calls method 
#'   defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "propen",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              return( propen(object = object@analysis) )
            })

#' \code{regimeCoef(object)}
#'   retrieves estimated tx regime parameters. Calls method defined for 
#'   \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "regimeCoef",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              return( regimeCoef(object = object@analysis) )
            })

#' \code{show(object)}
#'   Extends method defined for \code{OptimalSeq} to include DTRStep()
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "show",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object) {
              DTRstep(object = object)
              show(object = object@analysis)
              cat("Estimated Value:", object@estimatedValue, "\n")
            })

#' \code{summary(object)}
#'   retrieves summary information. Calls method defined for \code{OptimalSeq}.
#'
#' @rdname OptimalSeqCoarsened-methods
setMethod(f = "summary",
          signature = c(object = "OptimalSeqCoarsened"),
          definition = function(object, ...) {
              res <- summary(object = object@analysis, ...)
              res[[ "estimatedValue" ]] <- object@estimatedValue
              return( res )
            })


#' Define the Multiple Decision Point Objective Function
#'
#' @rdname seqFunc
setMethod(f = ".seqFunc",
          signature = c("eta" = "numeric",
                        "txObj" = "TxInfoList"),
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

              nDP <- length(x = regimesObj@regime)

              # ind : 0/1 patient tx is not/is in accordance with regime at dp i.
              ind <- matrix(data = 0L, nrow = nSamples, ncol = nDP)

              # note that data.frame reset txs to recommended optimal tx
              for (i in 1L:nDP) {

                # use current parameter estimates to determine recommended tx
                optTx <- .predictOptimalTx(x = regimesObj@regime[[ i ]], 
                                           newdata = l.data)

                optTx <- .convertTx(object = txObj@txInfo[[ i ]], txVec = optTx)

                txName <- .getTxName(object = txObj@txInfo[[ i ]])

                # does tx received match estimated optimal tx?
                ind[,i] <- .compareTx(object = txObj@txInfo[[ i ]], 
                                      vec1 = l.data[,txName],  
                                      vec2 = optTx)

                l.data[,txName] <- optTx
              }

              # lambda : probability that the tx does not follow regime.
              #          Pr(A_k != g_i)
              lambda <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

              # predict propensity to receive recommended tx
              for (i in 1L:nDP) {

                prWgt <- .getPrWgt(propenObj = propenObj@propen[[ i ]], 
                                   txObj = txObj@txInfo[[ i ]],  
                                   data = l.data)

                lambda[,i] <- 1.0 - prWgt

              }

              # qFunc : outcome regression at each dp at recommended tx
              qFunc <- matrix(data = 0.0, nrow = nSamples, ncol = nDP)

              if (!is(object = outcomeObj@outcome, class2 = "OutcomeNoFit")) {
                vtemp <- response

                for (i in nDP:1L) {

                  mu <- .predictAll(object = outcomeObj@outcome[[ i ]], 
                                    newdata = l.data)$decisionFunc

                  txName <- .getTxName(object = txObj@txInfo[[i]])
                  tst <- match(x = l.data[,txName], table = colnames(x = mu))
                  tstNA <- is.na(x = tst)
                  tst[tstNA] <- 1L

                  mu <- mu[cbind(1L:nSamples,tst)]
                  mu[tstNA] <- vtemp[tstNA]

                  vtemp <- mu

                  qFunc[,i] <- mu
                }
                qFunc[is.na(qFunc)] <- 0.0
              }

              # cumInd = 1 if patient followed tx regime up to the ith dp.
              #            I(C_{eta} >= i)
              #        = 0 if patient did not follow tx regime up to the ith dp.
              #            I(C_{eta} < i)
              temp <- apply(X = ind, MARGIN = 1L, FUN = cumprod)
              if (!all(dim(temp) == dim(ind))) temp <- t(temp)
              cumInd <- cbind(1L,temp)[,-{nDP+1L},drop=FALSE]

              # AC = 1 if all txs given to a patient follow the regime.
              #        I(C_{eta} = infinity)
              #    = 0 otherwise. I(C_{eta} <= K)
              AC <- apply(X = ind, MARGIN = 1L, FUN = prod)

              # C = 1 if patient treated in accordance with regime up to dp
              #       i, but did not follow tx regime at dp i I(C_{eta} = i)
              #   = 0 otherwise. I(C_{eta} != i)
              C <- cumInd * {1.0-ind}

              # pc = probability that coarsening occurs at a later dp.
              #   Pr(C_{et} > i) = prod_{k=1}^{i} (Pr(A_k=g_k))
              #                  = prod_{k=1}^{i} (1-Pr(A_k!=g_k))
              pc <- t(apply(X = {1.0-lambda}, MARGIN = 1L, FUN = cumprod))
              if (!all(dim(pc) == dim(lambda))) pc <- t(pc)

              #          I(C_{eta} = i) - Pr(A_i != g_i)*I(C_{eta} >= i)
              # DR = sum ----------------------------------------------- mu_i
              #       i               Pr(C_{eta} > i)
              DR <- rowSums(x = {C - lambda*cumInd} / pc * qFunc)


              #     (   I(C_{eta} = infinity)        )
              # mean|   --------------------- Y + DR |
              #     (      Pr(C_{eta} > K)           )

              mn <- sum(DR + AC / pc[,nDP] * response, na.rm=TRUE) / nSamples

              return( mn )

            })
