# October 25, 2018

#' Class \code{IQLearnFS_ME}
#'
#' Class \code{IQLearnFS_ME} contains the results for the first stage
#'  main effects component of the interactive Q-Learning algorithm.
#'  Objects of this class are returned by iqLearnFSM().
#'
#' @name IQLearnFS_ME-class
#'
#' @slot step : Not used in this context.
#' @slot outcome : The outcome regression analysis
#' @slot txInfo : The feasible tx information
#' @slot optimal : The estimated optimal tx, decision function, and value
#'
#' @template outcomeOnly
#' @template regression
#' @template DynTxRegime_methods
#'
#' @include E_class_IQLearnSS.R E_class_IQLearnFS.R
setClass(Class = "IQLearnFS_ME", 
         contains = c("IQLearnFS"))

#' Complete First Stage Analysis of Main Effects for Interactive Q-Learning Algorithm
#'
#' Performs regression on the fitted main effects the second stage regression.
#'
#' @name newIQLearnFS_ME
#'
#' @keywords internal
setGeneric(name = ".newIQLearnFS_ME", 
           def = function(moMain, moCont, response, ...) {
               standardGeneric(f = ".newIQLearnFS_ME")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{IQLearnFS_ME}
#'
#' @name IQLearnFS_ME-methods
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
.iqLearnFS_ME <- function(moMain,
                          moCont,
                          data,
                          response,
                          txName,
                          iter,
                          suppress) {

  if (!suppress) {
    cat("IQ-Learning Algorithm\n")
    cat("Regression of estimated main effects component of outcome.\n")
  }

  response <- fittedMain(object = response)

  qLearnObj <- .qLearn(moMain = moMain, 
                       moCont = moCont,  
                       data = data,  
                       response = response,
                       txName = txName,  
                       fSet = NULL,  
                       iter = iter,  
                       step = 1L,  
                       suppress = suppress)

  qLearnObj@analysis@optimal@optimalTx <- NA
  qLearnObj@analysis@optimal@estimatedValue <- NA

  qLearnObj <- new(Class = "IQLearnFS", qLearnObj)

  return( new(Class = "IQLearnFS_ME", qLearnObj) )

}

#' @rdname newIQLearnFS_ME
setMethod(f = ".newIQLearnFS_ME",
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_ME)

#' @rdname newIQLearnFS_ME
setMethod(f = ".newIQLearnFS_ME",
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_ME)

#' @rdname newIQLearnFS_ME
setMethod(f = ".newIQLearnFS_ME",
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_ME)

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnFS_ME"), 
          definition = function(object) { 
              cat("IQ-Learning: Regression of estimated outcome main effects\n")
            })

#' @rdname IQLearnFS_ME-methods
setMethod(f = "print",    
          signature = c(x = "IQLearnFS_ME"), 
          definition = function(x, ...) {
              DTRstep(object = x)
              callNextMethod()
            })

#' @rdname IQLearnFS_ME-methods
setMethod(f = "show",    
          signature = c(object = "IQLearnFS_ME"), 
          definition = function(object) {
              DTRstep(object = object)
              callNextMethod()
            })
