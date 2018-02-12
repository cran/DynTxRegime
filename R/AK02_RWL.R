 #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                              Class RWL                               #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of the Residual Weighted Learning algorithm                  #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# outcome         : outcome regression object                          #
# regime          : formula object for kernel covariates               #
# propensity      : PropensityFit object                               #
# crossValidation : cross-validation results                           #
# optim           : RWLOptim object                                    #
# txInfo          : TxInfo object                                      #
# responseType    : character indicating type of response              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
.checkRWL <- function(object) {

  errors <- character()

  if( !is(object@propen, "SingleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@outcome, "SingleDecisionPoint") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}
setClass(Class = "RWL",
         slots = c(         regime = "formula",
                   crossValidation = "CVInfoOrNULL",
                             optim = "RWLOptim",
                            txInfo = "TxInfoNoSubsets",
                      responseType = "character",
                      decisionFunc = "numeric"),
         contains = c("DynTxRegime", "PropensityAndOutcome"),
         validity = .checkRWL)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                             RWL GENERICS                             #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setGeneric(name = ".newRWL", 
           def = function(moPropen, moMain, ...){
                   standardGeneric(".newRWL")
                 })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                             RWL METHODS                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setMethod(f = "cvInfo",   
          signature = c(object = "RWL"), 
          definition = function(object) {
                         if( is(object@crossValidation,"NULL") ) {
                           return(NULL)
                         }
                         return(cvInfo(object@crossValidation))
                       })
#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWL                                      #
#   returns                                                            #
# String indicating RWL estimator                                      #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "RWL"), 
          definition = function(object){
                         return("Residual Weighted Learning")
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWL                                      #
#   returns                                                            #
# a list object containing key information from each optimization      #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",    
          signature = c(object = "RWL"), 
          definition = function(object, ...){
                         return( optimObj(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class RWL                                           #
# newdata : a data.frame                                               #
#   returns                                                            #
# A integer or character vector of optimal treatments coded as given   #
# in original data                                                     #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "RWL", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){

                         #-------------------------------------------#
                         # Obtain model matrix for decision rule     #
                         #-------------------------------------------#
                         x2 <- try(model.matrix(x@regime, newdata), 
                                   silent = TRUE)

                         if( is(x2,"try-error") ) {
                           UserError("input",
                                     paste("Unable to identify needed",
                                           "variables in newdata."))
                         }

                         #-------------------------------------------#
                         # Determine if there is an intercept, remove#
                         #-------------------------------------------#
                         if( attr(terms(x@regime), "intercept") == 1L ) {
                           x2 <- x2[,-1L,drop=FALSE]
                         }

                         #-------------------------------------------#
                         # Predict optimal treatment for RWLOptim    #
                         #-------------------------------------------#
                         opt <- .predictOptimalTx(x = x@optim,
                                                  newdata = x2)

                         #-------------------------------------------#
                         # Recast optimal tx into original coding    #
                         #-------------------------------------------#
                         fSetSuperSet <- .getSuperSet(x@txInfo)
                         temp <- NULL
                         temp[opt$optimalTx < -0.5] <- fSetSuperSet[1L]
                         temp[opt$optimalTx >  0.5] <- fSetSuperSet[2L]
                         opt$optimalTx <- temp

                         return( opt )
                       } )

setMethod(f = "optTx",
          signature = c(x = "RWL", 
                        newdata = "missing"),
          definition = function(x, newdata,....){
                         return(list("optimalTx" = x@optimalTx,
                                     "decisionFunc" = x@decisionFunc))
                       })
#----------------------------------------------------------------------#
# Print key results.                                                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class RWL                                           #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "RWL"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         cat("Response of type", x@responseType, "\n")
                         cat("Regime\n")
                         print(x@regime)
                         cat("\nOptimization\n")
                         print(x@optim, ...)
                         print(x = as(x,"PropensityAndOutcome"))
                         cat("\nEstimated Value:", estimator(x),"\n\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWL                                      #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "RWL"), 
          definition = function(object, ...){
                         return( regimeCoef(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Retrieve the residuals of the outcome regression                     #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWL                                      #
#   returns                                                            #
# the residuals of the outcome regression step used in optim.          #
#----------------------------------------------------------------------#
setMethod(f = "residuals",    
          signature = c(object = "RWL"), 
          definition = function(object, ...){
                         return( residuals(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWL                                      #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "RWL"), 
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         cat("Response of type", object@responseType, "\n")
                         cat("\nOptimization\n")
                         show(object@optim)
                         show(object = as(object,"PropensityAndOutcome"))
                         cat("\nEstimated Value:", estimator(object),"\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the propensity and outcome regressions#
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWL                                      #
#   returns                                                            #
# a list with a three elements, 'propensity', holding the summary      #
# of the fit object returned by the propensity regression method.      #
# 'optim' holds the key results of the optimization as defined in      #
# RWLOptim. 'outcome' hold the summary object defined for the          #
# OutcomeRegression object; 'value' is the estimated value             #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "RWL"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityAndOutcome"))
                         result[[ "optim" ]] <- summary(object@optim)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )

#----------------------------------------------------------------------#
# moPropen : object of class modelObj                                  #
# moMain   : object of class modelObj                                  #
# data     : data.frame of covariates and treatment                    #
# response : vector of response                                        #
# txName   : column header of data for treatment variable              #
# regime   : formula description of kernel covariates                  #
# lambdas  : tuning parameters to be considered                        #
# cvFolds  : number of cross validation folds to be used               #
# kernel   : character description of kernel function                  #
# kparam   : numeric parameter value for kernel function               #
# responseType : character indicating of continuous or count data      #
# txVec    : treatment variable defined as +/- 1.0                     #
# guess    : initial parameter estimates for optim                     #
# suppress : T/F indicating if screen prints are to be generated       #
#----------------------------------------------------------------------#
.RWL <- function(moPropen, moMain, data, 
                 response, txName, regime, lambdas,
                 cvFolds, kernel, kparam, responseType,
                 txVec, guess, suppress) { 

  if( !suppress ) cat("Residual Weighted Learning\n")

  #------------------------------------------------------------------#
  # Obtain model matrix for decision function                        #
  #------------------------------------------------------------------#
  x <- try(model.matrix(regime,data), silent = TRUE)

  if( is(x,"try-error") ) {
    UserError("input",
              "unable to generate model matrix")
  }

  #------------------------------------------------------------------#
  # Determine if decision function includes an intercept             #
  #------------------------------------------------------------------#
  hasIntercept <- attr(terms(regime),"intercept") == 1L
  if( hasIntercept ) x <- x[,-1L]

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(txName = txName, 
                       data = data, 
                       fSet = NULL,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve fitted propensity for treatment                         #
  #------------------------------------------------------------------#
  pr <- predict(object = propen, newdata = data)

  prWgt <- pr[,2L]
  prWgt[txVec < -0.5] <- pr[txVec < -0.5,1L]

  #------------------------------------------------------------------#
  # Fit outcome regression model                                     #
  #------------------------------------------------------------------#
  outcome <- .newOutcomeRegression(moMain = moMain, 
                                   moCont = NULL,  
                                   data = data,  
                                   response = response,  
                                   txInfo = txInfo,  
                                   iter = 0L,
                                   suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve residual of main effects model fit                      #
  #------------------------------------------------------------------#
  residual <- residuals(fitObject(outcome)[["moMain"]])

  #------------------------------------------------------------------#
  # If count data, change sign of residual                           #
  #------------------------------------------------------------------#
  if( responseType == "count" ) {
    residual <- -residual
    if( !suppress ) {
      cat("response type identified as count; residual -> -residual\n")
    }
  }

  #------------------------------------------------------------------#
  # Determine the best parameter value(s)                            #
  #------------------------------------------------------------------#
  if( cvFolds > 0L ) {
    cvResult <- .crossValidation2Par(optimFunc = ".newRWLOptim",
                                     valueFunc = ".valueFuncOWL",
                                     response = response,
                                     prWgt = prWgt,
                                     x = x,
                                     txVec = txVec,
                                     cvFolds = cvFolds,
                                     lambdas = lambdas,
                                     kernel = kernel,
                                     kparam = kparam,
                                     suppress = suppress,
                                     guess = guess,
                                     residual = residual)
  } else {
    cvResult <- list()
    cvResult$cv <- NULL
    cvResult$kparam <- kparam
    cvResult$lambda <- lambdas
  }

  #------------------------------------------------------------------#
  # Obtain training results at the lambda and kernel parameter.      #
  #------------------------------------------------------------------#
  trainResult <- .newRWLOptim(subset = 1L:nrow(x),
                              x = x,
                              kernel = kernel,
                              kparam = cvResult$kparam,
                              lambda = cvResult$lambda, 
                              txVec = txVec,
                              pr = prWgt,
                              response = response,
                              residual = residual,
                              guess = guess,
                              suppress = suppress)

  if( is(trainResult, "NULL") ) return(NULL)

  #------------------------------------------------------------------#
  # Determine optimal treatment                                      #
  #------------------------------------------------------------------#
  optVec <- .predictOptimalTx(x = trainResult, newdata = x)

  value <- .valueFuncOWL(subset = 1L:nrow(x), 
                         optTx = optVec$optimalTx, 
                         txVec = txVec,
                         prWgt = prWgt,
                         response = response)

  if( !suppress ) {
    cat("Estimated value:", value, "\n")
  }

  fSetSuperSet <- .getSuperSet(txInfo)
  topt <- NULL
  topt[optVec$optimalTx < -0.5] <- fSetSuperSet[1L]
  topt[optVec$optimalTx >  0.5] <- fSetSuperSet[2L]

  result <- new("RWL",
                "outcome"         = outcome,
                "regime"          = regime,
                "propen"          = propen,
                "crossValidation" = cvResult$cv,
                "optim"           = trainResult,
                "txInfo"          = txInfo,
                "responseType"    = responseType,
                "decisionFunc"    = optVec$decisionFunc,
                "estimatedValue"  = value,
                "optimalTx"       = topt,
                "call"            = NULL)

  return( result )
}
setMethod(f = ".newRWL",    
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj"), 
          definition = .RWL)

