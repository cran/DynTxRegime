#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                              Class BOWL                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of a call to BOWL                                            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# shift      : numeric, value by which the reward was shifted to make  #
#              all rewards positive                                    #
# propensity : PropensityRegression object                             #
# step       : integer indicating step of the algorithm                #
# ind        : T/F if did/did not follow optimal treatment in          #
#              this and all preceding steps.                           #
# sumR       : numeric total of the rewards for this and all preceding #
#              steps                                                   #
# prodPr     : numeric product of propensity weights for this and all  #
#              preceding steps.                                        #
# txInfo     : TxInfo object                                           #
# optim      : BOWLWithOneOrMoreRegimes object                         #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
.checkBOWL <- function(object) {

  errors <- character()

  if( !is(object@propen, "SingleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( is(object@txInfo, "TxInfoWithSubsets") ) {
    if( !is(object@optim, "BOWLWithSubsetRegimes") &&
        !is(object@propen, "SubsetsModeled") ) {
      msg <- "txInfo & propen/regime do not match"
      errors <- c(errors, msg)
    }
  }

  if( is(object@txInfo, "TxInfoNoSubsets") ) {
    if( is(object@optim, "BOWLWithSubsetRegimes") ||
        is(object@propen, "SubsetsModeled") ) {
      msg <- "txInfo & propen/regime do not match"
      errors <- c(errors, msg)
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}
setClass(Class = "BOWL",
         slots = c( shift = "numeric",
                     step = "integer",
                      ind = "logical",
                     sumR = "numeric",
                   prodPr = "numeric",
                   txInfo = "TxInfoBasic",
                    optim = "BOWLObj"),
         contains = c("DynTxRegime", "PropensityOnly"),
         validity = .checkBOWL)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                            BOWL METHODS                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setMethod(f = "cvInfo",   
          signature = c(object = "BOWL"), 
          definition = function(object) {
                         return(cvInfo(object@optim))
                       })
#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWL                                     #
#   returns                                                            #
# String indicating BOWL estimator                                     #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "BOWL"), 
          definition = function(object){
                         return(paste("Step", object@step, "of BOWL."))
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWL                                     #
#   returns                                                            #
# a list object containing key information from each optimization      #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="BOWL"),
          definition = function(object, ...){ 
                         return( optimObj(object@optim) ) } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWL                                          #
# newdata : a data.frame                                               #
#   returns                                                            #
# A integer or character vector of optimal treatments coded as given   #
# in original data                                                     #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "BOWL", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){
                         optimal <- .predictOptimalTx(x = x@optim, 
                                                      newdata = newdata)
                         return(optimal)
                       } )

setMethod(f = "optTx",
          signature = c(x = "BOWL", 
                        newdata = "missing"),
          definition = function (x, newdata,...){
                         optimal <- .predictOptimalTx(x = x@optim)
                         return(optimal)
                       } )

#----------------------------------------------------------------------#
# Print key results.                                                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWL                                          #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "BOWL"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         cat("Reward shifted by ", x@shift, "\n")
                         cat("\nOptimization\n")
                         print(x@optim, ...)
                         print(x = as(x,"PropensityOnly"))
                         cat("\nEstimated Value:", estimator(x),"\n\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the decision function           # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWL                                     #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "BOWL"), 
          definition = function(object, ...){
                         return( regimeCoef(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWL                                     #
#   returns                                                            #
# Nothing returned                                                     #
# Calls show methods of OWLOptim and PropensityFit                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "BOWL"), 
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         cat("Reward shifted by ", object@shift, "\n")
                         cat("\nOptimization\n")
                         show(object@optim)
                         show(object = as(object,"PropensityOnly"))
                         cat("\nEstimated Value:", estimator(object),"\n\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the propensity regression             #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWL                                     #
#   returns                                                            #
# a list with a three elements, 'propensity', holding the summary      #
# of each fit object returned by the propensity regression method.     #
# 'optim' holds the key results of the optimization as defined in      #
# OWLOptim. 'value' is the estimated value                             #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "BOWL"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityOnly"))
                         result[[ "optim" ]] <- summary(object@optim)
                         result[[ "value" ]] <- estimator(object)
                         return( result )
                       } )
