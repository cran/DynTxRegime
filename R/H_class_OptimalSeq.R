# October 26, 2018

#' Class \code{OptimalSeq}
#'
#' Class \code{OptimalSeq} contains the results for the estimated optimal tx
#'   and value when estimated from a coarsened or missing data perspective.
#'
#' @name OptimalSeq-class
#' @docType class
#'
#' @slot genetic A list containing the results from the genetic algorithm
#' @slot propen Results of the propensity regression step
#' @slot outcome Results of the outcome regression step
#' @slot regime Results for the regime.
#' @slot optimal Results for the estimated optimal tx and value
#' @slot Call The unevaluated call.
#'
#' @template outcomeOnly
#' @template propenOnly
#' @template regression
#' @template regimeOnly
#' @template DynTxRegime_methods
setClass("OptimalSeq",
         slots = c(genetic = "list"),
         contains = c("PropensityObj",
                      "OutcomeObj",
                      "RegimeObj",
                      "DynTxRegime"))

##########
## GENERICS
##########

#' Complete a the Coarsened/Missing Data Analysis
#'
#' Dispatches appropriate coarsened or missing data perspective method.
#'
#' @param moPropen model object(s) for propensity regression
#' @param moMain model object(s) for main effects of outcome regression
#' @param moCont model object(s) for contrasts of outcome regression
#' @param fSet function(s) defining feasible tx
#' @param ... additional inputs.
#'
#' @rdname newOptimalSeq
#'
#' @keywords internal
setGeneric(name = ".newOptimalSeq",
           def = function(moPropen, moMain, moCont, fSet, ...) {
               standardGeneric(f = ".newOptimalSeq")
             })

#' Retrieve the Genetic Algorithm Results
#'
#' Retrieve the value object returned by rgenoud() in optimalSeq().
#'
#' @param object Value object returned by optimalSeq()
#' @param ... Optional inputs. Ignored.
#'
#' @name genetic
#' @exportMethod genetic
setGeneric(name = "genetic",
           def = function(object, ...) { standardGeneric(f = "genetic") })

#' Define the Objective Function
#'
#' Method is defined by inheriting classes to define the objective function
#'   optmized by the genetic algorithm.
#'
#' @rdname seqFunc
#'
#' @keywords internal
setGeneric(name = ".seqFunc",
           def = function(eta, txObj, ...) { standardGeneric(f = ".seqFunc") })

##########
## METHODS
##########

#' Methods Available for Objects of Class \code{OptimalSeq}
#'
#' @name OptimalSeq-methods
#'
#' @keywords internal
NULL

#' @rdname DynTxRegime-internal-api
setMethod(f = "Call",
          signature = c(name = "OptimalSeq"),
          definition = function(name, ...) {
              return( Call(name = as(object = name, 
                                     Class = "DynTxRegime", ...)) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "coef",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) {
              res1 <- coef(object = as(object = object, 
                                       Class = "PropensityObj"), ...)
              res2 <- coef(object = as(object = object, 
                                       Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep",
          signature = c(object = "OptimalSeq"),
          definition = function(object) { stop("not allowed") })

#' @rdname DynTxRegime-internal-api
setMethod(f = "estimator",
          signature = c(x = "OptimalSeq"),
          definition = function(x, ...) {
              return( estimator(x = as(object = x, 
                                       Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "fitObject",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) {
              res1 <- fitObject(object = as(object = object, 
                                            Class = "PropensityObj"), ...)
              res2 <- fitObject(object = as(object = object, 
                                            Class = "OutcomeObj"), ...)
              return( c(res1, res2) )
            })

#' @rdname genetic
setMethod(f = "genetic",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) { return( object@genetic ) })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "OptimalSeq",
                        newdata = "data.frame"),
          definition = function (x, newdata, ...) {
              tx <- .predictOptimalTx(x = as(object = x, Class = "RegimeObj"), 
                                      newdata = newdata, ...)
              tx <- .convertTx(object = x@propen@txInfo, txVec = tx)
              return( list("optimalTx" = tx, "decisionFunc" = NA) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "optTx",
          signature = c(x = "OptimalSeq",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( optTx(x = as(object = x, Class = "DynTxRegime"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "outcome",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) {
              return( outcome(object = as(object = object, 
                                          Class = "OutcomeObj"), ...) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "plot",
          signature = c(x = "OptimalSeq", y = "missing"),
          definition = function(x, y, suppress=FALSE, ...) {
              plot(x = as(object = x, Class = "PropensityObj"), 
                   suppress = suppress, ...)
              plot(x = as(object = x, Class = "OutcomeObj"), 
                   suppress = suppress, ...)
            })

#' \code{print(x)}
#'   prints main results of a coarsened/missing data analysis
#'
#' @rdname OptimalSeq-methods
setMethod(f = "print",
          signature = c(x = "OptimalSeq"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "PropensityObj"))
              print(x = as(object = x, Class = "OutcomeObj"), ...)
              cat("Genetic\n")
              print(x = genetic(object = x))
              print(x = as(object = x, Class = "RegimeObj"))
              print(x = as(object = x, Class = "DynTxRegime"))
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "propen",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) {
              return( propen(object = as(object = object, Class = "PropensityObj")) )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "regimeCoef",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) {
              return( regimeCoef(object = as(object = object, Class = "RegimeObj")) )
            })

#' \code{show(object)}
#'   displays main results of a coarsened/missing data analysis
#'
#' @rdname OptimalSeq-methods
setMethod(f = "show",
          signature = c(object = "OptimalSeq"),
          definition = function(object) {
              show(object = as(object = object, Class = "PropensityObj"))
              show(object = as(object = object, Class = "OutcomeObj"))
              cat("Genetic\n")
              show(genetic(object = object))
              show(object = as(object = object, Class = "RegimeObj"))
              show(object = as(object = object, Class = "DynTxRegime"))
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "summary",
          signature = c(object = "OptimalSeq"),
          definition = function(object, ...) {
              res1 <- summary(object = as(object = object, Class = "PropensityObj"))
              res2 <- summary(object = as(object = object, 
                                          Class = "OutcomeObj"))
              res3 <- list("genetic" = object@genetic)
              res4 <- summary(object = as(object = object, Class = "RegimeObj"))
              res5 <- summary(object = as(object = object, Class = "DynTxRegime"))
              return( c(res1, res2, res3, res4, res5) )
            })

.geneticStep <- function(txObj, 
                         regimesObj, 
                         propenObj,
                         outcomeObj,
                         response,
                         data,
                         argsList,
                         suppress){

  argList <- argsList
  argList[[ 'regimesObj' ]] <- regimesObj
  argList[[ 'txObj' ]] <- txObj
  argList[[ 'l.data' ]] <- quote(expr = data)
  argList[[ 'propenObj' ]] <- propenObj
  argList[[ 'outcomeObj' ]] <- outcomeObj
  argList[[ 'response' ]] <- response

  argList[[ 'fn' ]] <- .seqFunc
  argList[[ 'nvars' ]] <- .getNumPars(object = regimesObj)
  argList[[ 'print.level' ]] <- !suppress
  argList[[ 'max' ]] <- TRUE
  argList[[ 'gradient.check' ]] <- FALSE
  argList[[ 'BFGS' ]] <- FALSE
  argList[[ 'P9' ]] <- 0L
  argList[[ 'optim.method' ]] <- "Nelder-Mead"

  gaEst <- do.call(what = rgenoud::genoud, args = argList)

  if (!suppress) {
    cat("Genetic Algorithm\n")
    print(x = gaEst)
  }

  return( gaEst )
}
