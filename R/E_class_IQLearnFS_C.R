# October 25, 2018

#' Class \code{IQLearnFS_C}
#'
#' Class \code{IQLearnFS_C} contains the results for the first stage
#'  contrasts component of the interactive Q-Learning algorithm.
#'  Objects of this class are returned by iqLearnFSC().
#'
#' @name IQLearnFS_C-class
#'
#' @slot txVec : A numeric. treatment vector from training data
#' @slot residuals : A numeric. residuals of the fit
#' @slot step : Not used in this context.
#' @slot outcome : The outcome regression analysis
#' @slot txInfo : The feasible tx information
#' @slot optimal : The estimated optimal tx, decision function, and value
#'
#' @template outcomeOnly
#' @template regression
#' @template DynTxRegime_methods
#' @section Methods For Accessing Main Results:
#' \describe{
#'    \item{residuals}{:Retrieve the residuals of the regression.}
#'    \item{sd}{:Retrieve the standard deviation of the residuals.}
#'}
#'
#' @include E_class_IQLearnSS.R E_class_IQLearnFS.R
setClass(Class = "IQLearnFS_C", 
         slots = c(txVec     = "numeric",
                   residuals = "numeric"),
         contains = c("IQLearnFS"))

##########
## GENERICS
##########

#' Complete First Stage Analysis of Contrasts for Interactive Q-Learning Algorithm
#'
#' Performs regression on the fitted contrasts of the second stage regression.
#'
#' @rdname newIQLearnFS_C
#'
#' @keywords internal
setGeneric(name = ".newIQLearnFS_C", 
           def = function(moMain, moCont, response, ...) {
               standardGeneric(f = ".newIQLearnFS_C")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{IQLearnFS_C}
#'
#' @name IQLearnFS_C-methods
#'
#' @keywords internal
NULL


# @param moMain An object of class modelObj or NULL
# @param moCont An object of class modelObj or NULL
#    note that at least 1 of moMain and moCont must be defined
# @param response A object of class IQLearnSS
# @param data A data.frame of covariates
# @param txName A string; column header of treatment variable in data
# @param iter The maximum number of iterations if iterative algorithm used
# @param suppress T/F indicating if prints to screen are to be executed
.iqLearnFS_C <- function(moMain,
                         moCont,
                         response,
                         data,
                         txName,
                         iter,
                         suppress) {

  if (!suppress) {
    cat("IQ-Learning Algorithm\n")
    cat("Regression of estimated contrasts component of outcome.\n")
  }

  response <- fittedCont(object = response)

  qLearnObj <- .qLearn(moMain = moMain, 
                       moCont = moCont,  
                       data = data,  
                       response = response,
                       txName = txName,  
                       fSet = NULL,  
                       iter = iter,  
                       step = 1L,  
                       suppress = suppress)

  residual <- drop(x = response - predict(object = as(object = qLearnObj@analysis, 
                                                      Class = "OutcomeObj"), 
                                          newdata = data))

  qLearnObj@analysis@optimal@optimalTx <- NA
  qLearnObj@analysis@optimal@estimatedValue <- NA

  return( new(Class = "IQLearnFS_C",
              "txVec"     = data[,txName],
              "residuals" = residual,
              qLearnObj) )

}

#' @rdname newIQLearnFS_C
setMethod(f = ".newIQLearnFS_C",
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_C)

#' @rdname newIQLearnFS_C
setMethod(f = ".newIQLearnFS_C",
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_C)

#' @rdname newIQLearnFS_C
setMethod(f = ".newIQLearnFS_C",
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_C)

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnFS_C"), 
          definition = function(object) { 
              cat("IQ-Learning: Regression of estimated outcome contrasts\n")
            })

#' @rdname IQLearnFS_C-methods
setMethod(f = "print",    
          signature = c(x = "IQLearnFS_C"), 
          definition = function(x, ...) {
              DTRstep(object = x)
              callNextMethod()
            })

#' @rdname residuals
#' @exportMethod residuals
setMethod(f = "residuals",
          signature = c(object = "IQLearnFS_C"),
          definition = function(object, ...) { return( object@residuals ) })

#' Standard Deviation
#'
#' Retrieve the standard deviation of the residuals for the first-stage contrasts
#'   regression in the interactive Q-Learning algorithm.
#'
#' @usage
#'   sd(x, na.rm=FALSE)
#'
#'   ## S4 method for IQLearnFS_C
#'   sd(x, na.rm=FALSE)
#'
#' @param x An object of class \code{IQLearnFS_C}
#' @param na.rm logical. Should missing values be removed?
#'
#' @name sd
#' @aliases sd,IQLearnFS_C-method
#' @importFrom stats sd
#' @exportMethod sd
setMethod(f = "sd", 
          signature = c(x = "IQLearnFS_C"), 
          definition = function(x, na.rm = FALSE) { 
              resid <- residuals(object = x)
              stdDev <- sd(x = resid)
              return( stdDev ) 
            })

#' @rdname IQLearnFS_C-methods
setMethod(f = "show",    
          signature = c(object = "IQLearnFS_C"), 
          definition = function(object) {
              DTRstep(object = object)
              callNextMethod()
            })
