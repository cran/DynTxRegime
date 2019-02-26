# October 24, 2018

#' Class \code{IQLearnSS}
#'
#' Class \code{IQLearnSS} contains the results for the second stage
#'   of the interactive Q-Learning algorithm. Objects of this class are
#'   returned by iqLearnSS().
#'
#' @name IQLearnSS-class
#'
#' @slot yContHat : A numeric. Estimated contrast component
#' @slot yMainHat : A numeric. Estimated main effects component
#' @slot delta : A numeric. Indicator of compliance * response used for value calc
#' @slot step : Not used in this context.
#' @slot outcome : The outcome regression analysis
#' @slot txInfo : The feasible tx information
#' @slot optimal : The estimated optimal tx, decision function, and value
#'
#' @template outcomeOnly
#' @template regression
#' @template DynTxRegime_methods
#' @section Methods For Post-Processing of Regression Analysis:
#' \describe{
#'    \item{fittCont}{:Retrieve the contrasts component of the regression.}
#'    \item{fittMain}{:Retrieve the main effects component of the regression.}
#'}
#'
#' @include E_class_QLearn.R
setClass(Class = "IQLearnSS", 
         slots = c(yContHat = "numeric",
                   yMainHat = "numeric",
                   delta    = "numeric"),
         contains = c("QLearn"))

#' Complete Second Stage Analysis of Interactive Q-Learning Algorithm
#'
#' Performs the regression of the outcome.
#'
#' @name newIQLearnSS
#'
#' @keywords internal
setGeneric(name = ".newIQLearnSS", 
           def = function(moMain, moCont, ...) {
                   standardGeneric(f = ".newIQLearnSS")
                 })

#' Retrieve the Fitted Contrast Component from Second Stage IQ-Learning
#'
#' Extracts the contrasts component of the fitted outcome regression
#'  the second-stage analysis of the interactive Q-Learning algorithm.
#'
#' @param object An object of class IQLearnSS
#' @param ... Ignored.
#'
#' @name fittedCont
setGeneric(name = "fittedCont", 
           def = function(object, ...) { standardGeneric(f = "fittedCont") })

#' Retrieve the Fitted Main Effects Component from Second Stage IQ-Learning
#'
#' Extracts the main effects component of the fitted outcome regression for
#'  the second-stage analysis of the interactive Q-Learning algorithm.
#'
#' @param object An object of class IQLearnSS
#' @param ... Ignored.
#'
#' @name fittedMain
setGeneric(name = "fittedMain", 
           def = function(object, ...) { standardGeneric(f = "fittedMain") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{IQLearnSS}
#'
#' @name IQLearnSS-methods
#'
#' @keywords internal
NULL

# @param moMain An object of class modelObj or NULL
# @param moCont An object of class modelObj or NULL
#            note that at least 1 of moMain and moCont must be defined #
# @param data A data.frame of covariates
# @param response The response vector
# @param txName A string; column header of treatment variable in data
# @param iter The maximum number of iterations if iterative algorithm used
# @param suppress T/F indicating if prints to screen are executed
#
# @returns an object of class IQLearnSS.
.iqLearnSS <- function(moMain,
                       moCont,
                       data,
                       response,
                       txName,
                       iter, 
                       suppress) {

  if (!suppress) {
    cat("Second-Stage analysis of IQ-Learning Algorithm\n")
  }

  qLearnObj <- .qLearn(moMain = moMain, 
                       moCont = moCont,  
                       data = data,  
                       response = response,
                       txName = txName,  
                       fSet = NULL,  
                       iter = iter,  
                       step = 1L,  
                       suppress = suppress)

  opt <- optTx(x = qLearnObj)

  fC <- drop(0.5*{opt$decisionFunc[,2L] - opt$decisionFunc[,1L]})
  fM <- drop(0.5*{opt$decisionFunc[,2L] + opt$decisionFunc[,1L]})

  result <- new(Class = "IQLearnSS",
                "yContHat" = fC,
                "yMainHat" = fM,
                "delta"    = {opt$optimalTx == data[,txName]}*response,
                qLearnObj)

  return( result )

}

#' @rdname newIQLearnSS
setMethod(f = ".newIQLearnSS",
          signature = c(moMain = "modelObj",
                        moCont = "modelObj"), 
          definition = .iqLearnSS)

#' @rdname newIQLearnSS
setMethod(f = ".newIQLearnSS",
          signature = c(moMain = "modelObj",
                        moCont = "NULL"), 
          definition = .iqLearnSS)

#' @rdname newIQLearnSS
setMethod(f = ".newIQLearnSS",
          signature = c(moMain = "NULL",
                        moCont = "modelObj"), 
          definition = .iqLearnSS)

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnSS"), 
          definition = function(object) { 
              cat("IQ-Learning: Second Stage\n") 
            })

#' Retrieve the Fitted Contrast Component from Second Stage IQ-Learning
#'
#' @rdname fittedCont
setMethod(f = "fittedCont",    
          signature = c(object = "IQLearnSS"), 
          definition = function(object, ...) { return( object@yContHat ) })

#' Retrieve the Fitted Main Effects Component from Second Stage IQ-Learning
#'
#' @rdname fittedMain
setMethod(f = "fittedMain",    
          signature = c(object = "IQLearnSS"), 
          definition = function(object, ...) { return( object@yMainHat ) })

#' @rdname IQLearnSS-methods
setMethod(f = "print",    
          signature = c(x = "IQLearnSS"), 
          definition = function(x, ...) {
              DTRstep(object = x)
              print(x = as(object = x@analysis, Class = "OutcomeObj"))
              print(x = as(object = x@analysis, Class = "DynTxRegime"))
            })

#' @rdname IQLearnSS-methods
setMethod(f = "show",    
          signature = c(object = "IQLearnSS"), 
          definition = function(object) {
              DTRstep(object = object)
              show(object = as(object = object@analysis, Class = "OutcomeObj"))
              show(object = as(object = object@analysis, Class = "DynTxRegime"))
            })
