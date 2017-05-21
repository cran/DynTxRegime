#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                              Class OWL                               #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of a call to OWL                                             #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# shift           : numeric, value by which the reward was shifted to  #
#                   make all rewards positive                          #
# modelFormula    : formula, model to be used for decision function    #
# propen          : PropensityFit object                               #
# crossValidation : matrix of crossValidation results if performed     #
# optim           : OWLOptim object                                    #
# txInfo          : TxInfo object                                      #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
.checkValidity_OWL <- function(object) {

  errors <- character()

  if( !is(object@propen, "SingleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@propen, "SubsetsNotModeled") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}
setClass(Class = "OWL",
         slots = c(          shift = "numeric",
                      modelFormula = "formula",
                   crossValidation = "CVInfoOrNULL",
                             optim = "OWLOptim",
                            txInfo = "TxInfoNoSubsets",
                      decisionFunc = "numeric"),
         contains = c("PropensityOnly", "DynTxRegime"),
         validity = .checkValidity_OWL)

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                             OWL GENERICS                             #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setGeneric(name = ".newOWL",
           def = function(moPropen, ...){
                   standardGeneric(".newOWL")
                 })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                             OWL METHODS                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setMethod(f = "cvInfo",
          signature = c(object = "OWL"),
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
# object : an object of class OWL                                      #
#   returns                                                            #
# String indicating OWL estimator                                      #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep",
          signature = c(object = "OWL"),
          definition = function(object){
                         return("Outcome Weighted Learning")
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OWL                                      #
#   returns                                                            #
# a list object containing key information from optimization routine   #
# Defined by OWLOptim                                                  #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object = "OWL"),
          definition = function(object,...){
                         return( optimObj(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OWL                                           #
#   returns                                                            #
# A numeric vector of +/- 1.0                                          #
#----------------------------------------------------------------------#
.predictOptTxOWL <- function(x, newdata) {

  #------------------------------------------------------------------#
  # Obtain model matrix for decision function                        #
  #------------------------------------------------------------------#
  x2 <- try(model.matrix(object = x@modelFormula, data = newdata),
            silent = FALSE)

  if( is(x2,"try-error") ) {
    UserError("input",
              "Unable to identify needed variables in newdata.")
  }

  #------------------------------------------------------------------#
  # If there is an intercept in model, remove it.                    #
  #------------------------------------------------------------------#
  if( attr(terms(x@modelFormula),"intercept") == 1L ) {
    x2 <- x2[,-1L,drop=FALSE]
  }

  #------------------------------------------------------------------#
  # Estimate optimal treatment                                       #
  #------------------------------------------------------------------#
  opt <- .predictOptimalTx(x = x@optim, newdata = x2)

  #------------------------------------------------------------------#
  # Retrieve super set of treatments                                 #
  #------------------------------------------------------------------#
  fSetSuperSet <- .getSuperSet(x@txInfo)
  topt <- NULL

  #------------------------------------------------------------------#
  # If sign is negative, set optimal treatment to first tx option    #
  #------------------------------------------------------------------#
  topt[opt$optimalTx < -0.5] <- fSetSuperSet[1L]

  #------------------------------------------------------------------#
  # If sign is positive, set optimal treatment to second tx option   #
  #------------------------------------------------------------------#
  topt[opt$optimalTx >  0.5] <- fSetSuperSet[2L]

  #------------------------------------------------------------------#
  # Return f(x) and the optimal treatment                            #
  #------------------------------------------------------------------#
  return( list("optimalTx" = topt,
               "decisionFunc" = opt$decisionFunc) )
}

setMethod(f = "optTx",
          signature = c(x = "OWL",
                        newdata = "data.frame"),
          definition = .predictOptTxOWL)

setMethod(f = "optTx",
          signature = c(x = "OWL",
                        newdata = "missing"),
          definition = function(x, newdata,....){
                         return(list("optimalTx" = x@optimalTx,
                                     "decisionFunc" = x@decisionFunc))
                       })

#----------------------------------------------------------------------#
# Print key results.                                                   #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OWL                                           #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",
          signature = c(x = "OWL"),
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         cat("Reward shifted by ", x@shift, "\n")
                         cat("\nOptimization\n")
                         print(x@optim, ...)
                         print(x = as(x,"PropensityOnly"))
                         cat("\nEstimated Value:", estimator(x),"\n\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OWL                                      #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",
          signature = c(object = "OWL"),
          definition = function(object, ...){
                         return( regimeCoef(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OWL                                      #
#   returns                                                            #
# Nothing returned                                                     #
# Calls show methods of OWLOptim and PropensityFit                     #
#----------------------------------------------------------------------#
setMethod(f = "show",
          signature = c(object = "OWL"),
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         cat("Reward shifted by ", object@shift, "\n")
                         cat("\nOptimization\n")
                         show(object@optim)
                         show(object = as(object,"PropensityOnly"))
                         cat("\nEstimated Value:", estimator(object),"\n\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of propensity regression, optimization,  #
# and estimated value                                                  #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OWL                                      #
#   returns                                                            #
# a list with a three elements, 'propensity', holding the summary      #
# of the fit object returned by the propensity regression method.      #
# 'optim' holds the key results of the optimization as defined in      #
# OWLOptim. 'value' is the estimated value                             #
#----------------------------------------------------------------------#
setMethod(f = "summary",
          signature = c(object = "OWL"),
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityOnly"))
                         result[[ "optim" ]] <- summary(object@optim)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )

#----------------------------------------------------------------------#
# Outcome Weighted Learning algorithm                                  #
#----------------------------------------------------------------------#
#   params                                                             #
# moPropen : modelObj for propensity modeling                          #
# data     : data.frame of covariates                                  #
# reward   : reward                                                    #
# txName   : treatment variable column header in data                  #
# regime   : formula description of decision function                  #
# lambdas  : tuning parameter(s)                                       #
# cvFolds  : number of cross-validation folds                          #
# kernel   : character description of kernel function                  #
# kparam   : numeric parameter for kernel function                     #
# txVec    : treatment vector recast as +/- 1                          #
# suppress : T/F indicating if prints to screen are executed           #
#   returns                                                            #
# an OWL object                                                        #
#----------------------------------------------------------------------#
.OWL <- function(moPropen,
                 data,
                 reward,
                 txName,
                 regime,
                 lambdas,
                 cvFolds,
                 kernel,
                 kparam,
                 txVec,
                 suppress) {

  if( !suppress ) cat("Outcome Weighted Learning\n")

  #------------------------------------------------------------------#
  # Obtain model matrix for decision function.                       #
  #------------------------------------------------------------------#
  x <- try(model.matrix(object = regime, data = data), silent = FALSE)

  if( is(x,"try-error") ) {
    UserError("input",
              paste("at least one element of the",
                    "covariates in regime was not",
                    "found in data"))
  }

  #------------------------------------------------------------------#
  # Remove intercept if specified in model.                          #
  #------------------------------------------------------------------#
  hasIntercept <- attr(terms(regime),"intercept") == 1L
  if( hasIntercept ) x <- x[,-1L]

  #------------------------------------------------------------------#
  # Shift reward to be all positive                                  #
  #------------------------------------------------------------------#
  shift <- min(reward)
  if( shift <= 0.0 ) {
    shift <- shift - 0.001
    reward <- reward - shift
    shift <- - shift
    if( !suppress ) {
      cat("Reward shifted by ", shift, "\n")
    }
  } else {
    shift <- 0.0
  }

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
  # Predict propensity for treatment                                 #
  #------------------------------------------------------------------#
  pr <- predict(object = propen, newdata = data)

  #------------------------------------------------------------------#
  # Propensity vector for weighting                                  #
  #------------------------------------------------------------------#
  prWgt <- pr[,2L]
  prWgt[txVec < -0.5] <- 1.0 - prWgt[txVec < -0.5]

  #------------------------------------------------------------------#
  # Determine the best parameter value(s)                            #
  #------------------------------------------------------------------#
  if( cvFolds > 0L ) {
    cvResult <- .newCVInfo(optimFunc = ".newOWLOptim",
                           valueFunc = ".valueFuncOWL",
                           response = reward,
                           prWgt = prWgt,
                           x = x,
                           txVec = txVec,
                           cvFolds = cvFolds,
                           lambdas = lambdas,
                           kernel = kernel,
                           kparam = kparam,
                           suppress = suppress)

  } else {
    cvResult <- list()
    cvResult$cv <- NULL
    cvResult$kparam <- kparam
    cvResult$lambda <- lambdas[1L]
  }

  #------------------------------------------------------------------#
  # Obtain parameter estimates at final lambda & kparam values.      #
  #------------------------------------------------------------------#
  trainResult <- .newOWLOptim(x = x,
                              subset = 1L:nrow(x),
                              lambda = cvResult$lambda,
                              txVec = txVec,
                              prWgt = prWgt,
                              response = reward,
                              suppress = suppress,
                              kernel = kernel,
                              kparam = cvResult$kparam)

  #------------------------------------------------------------------#
  # Determine optimal treatment                                      #
  #------------------------------------------------------------------#
  optVec <- .predictOptimalTx(x = trainResult, newdata = x)

  #------------------------------------------------------------------#
  # Estimate value                                                   #
  #------------------------------------------------------------------#
  value <- .valueFuncOWL(subset = 1L:nrow(x),
                         optTx = optVec$optimalTx,
                         txVec = txVec,
                         prWgt = prWgt,
                         response = reward)

  #------------------------------------------------------------------#
  # Recast optimal treatment in terms of original treatment values   #
  #------------------------------------------------------------------#
  fSetSuperSet <- .getSuperSet(txInfo)
  topt <- NULL
  topt[optVec$optimalTx < -0.5] <- fSetSuperSet[1L]
  topt[optVec$optimalTx >  0.5] <- fSetSuperSet[2L]

  if( !suppress ) {
    cat("Estimated Value:", value, "\n")
  }

  df <- drop(optVec$decisionFunc)
  names(df) <- NULL

  result <- new("OWL",
                "shift"           = shift,
                "modelFormula"    = regime,
                "crossValidation" = cvResult$cv,
                "propen"          = propen,
                "optim"           = trainResult,
                "txInfo"          = txInfo,
                "estimatedValue"  = value,
                "optimalTx"       = topt,
                "decisionFunc"    = df,
                "call"            = NULL)

  return( result )
}

setMethod(f = ".newOWL",
          signature = c(moPropen = "modelObj"),
          definition = .OWL)

#----------------------------------------------------------------------#
# Value function                                                       #
#----------------------------------------------------------------------#
#   params                                                             #
# subset : indices of those to include in estimating value             #
# optTx  : estimated optimal treatment                                 #
# txVec  : treatment received coded as 1/-1                            #
# prWgt  : propensity of treatment received                            #
# response : vector of response                                        #
#----------------------------------------------------------------------#
.valueFuncOWL <- function(subset,
                          optTx,
                          txVec,
                          prWgt,
                          response, ...){

  #------------------------------------------------------------------#
  # Limit value calculation to specified subset.                     #
  #------------------------------------------------------------------#
  optTx <- optTx[subset]
  txVec <- txVec[subset]
  prWgt <- prWgt[subset]
  response <- response[subset]

  #------------------------------------------------------------------#
  # Identify those with positive optimal treatment                   #
  #------------------------------------------------------------------#
  posOpt <- optTx > 0.5

  #------------------------------------------------------------------#
  # Identify those that received positive treatment                  #
  #------------------------------------------------------------------#
  posTx <- txVec > 0.5

  #------------------------------------------------------------------#
  # ind=1 for those that received the optimal treatment              #
  #------------------------------------------------------------------#
  ind <- {!posTx & !posOpt} | {posTx & posOpt}

  #------------------------------------------------------------------#
  # V = sum Y I(A=g) / pi(g)                                         #
  #------------------------------------------------------------------#
  val <- response * ind / prWgt

  badV <- is.infinite(val) | is.na(val) | is.nan(val)

  val[ badV ] <- 0.0

  value <- mean(val)

  return( value )
}


