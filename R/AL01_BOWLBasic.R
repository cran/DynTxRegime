setClass(Class = "BOWLObj",
         contains = "VIRTUAL")

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                           Class BOWLBasic                            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of a call to BOWL with Binary treatment options              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# optTx           : numeric vector of optimal treatments               #
# estVal          : estimated value of treatment                       #
# regime          : formula, model to be used for optimization         #
# crossValidation : matrix of crossValidation results if performed     #
# optim           : OWLOptim object                                    #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
.checkBOWLBasic <- function(object){

  errors <- character()

  if( is.na(object@estVal) || 
      is.nan(object@estVal) || 
      is.null(object@estVal) ) {
    msg <- "estimated value is not a real value."
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }


}

setClass(Class = "BOWLBasic",
         slots = c(          optTx = "numeric",
                            estVal = "numeric",
                            regime = "formula",
                   crossValidation = "CVInfoOrNULL",
                      decisionFunc = "numeric"),
         contains = c("OWLOptim", "BOWLObj"),
         validity = .checkBOWLBasic)

setGeneric(name = ".newBOWLBasic", 
           def = function(regime, txInfo, ...){
                   standardGeneric(".newBOWLBasic")
                 } )
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                          BOWLBasic METHODS                           #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setMethod(f = "cvInfo",   
          signature = c(object = "BOWLBasic"), 
          definition = function(object) {
                         if( is(object@crossValidation,"NULL") ) {
                           return(NULL)
                         }
                         return(cvInfo(object@crossValidation))
                       })

#----------------------------------------------------------------------#
# Retrieve the estimated optimal treatment                             #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLBasic                                     #
#   returns                                                            #
# a numeric vector +/- 1                                               #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",  
          signature = c(x = "BOWLBasic",
                        newdata = "missing"), 
          definition = function(x, newdata, ...){ 
                         return( list("optimalTx" = x@optTx,
                                      "decisionFunc" = x@decisionFunc) )
                       } )

#----------------------------------------------------------------------#
# Predict the optimal treatment                                        #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLBasic                                     #
#   returns                                                            #
# a numeric vector +/- 1                                               #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",
          signature = c(x = "BOWLBasic", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){

                         x2 <- try(model.matrix(object = x@regime,
                                               data = newdata), 
                                   silent = TRUE)

                         if( is(x2,"try-error") ) {
                           UserError("input",
                                     paste("unable to identify needed",
                                           "variables in newdata"))
                         }

                         hasIntercept <- attr(terms(x@regime),
                                              "intercept") == 1L
                         if( hasIntercept ) x2 <- x2[,-1L]

                         opt <- .predictOptimalTx(x = as(x, "OWLOptim"), 
                                                  newdata = x2)

                         return( opt )
                       } )

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLBasic                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "BOWLBasic"), 
          definition = function(x, ...){
                         print(as(x,"OWLOptim"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the decision function           # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLBasic                                #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "BOWLBasic"), 
          definition = function(object, ...){
                         return( regimeCoef(as(object,"OWLOptim")) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLBasic                                #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "BOWLBasic"), 
          definition = function(object){
                         show(as(object,"OWLOptim"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary                                                 #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLBasic                                #
#   returns                                                            #
# a list                                                               #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "BOWLBasic"), 
          definition = function(object, ...){
                         return( summary(as(object,"OWLOptim")) )
                       } )

#----------------------------------------------------------------------#
# Create a new BOWLBasic object when single decision function and      #
# propensity does not use subset modelling                             #
#----------------------------------------------------------------------#
# regime  : formula description of decision rule                       #
# txInfo  : TxInfoNoSubsets object                                     #
# ind     : T/F indicating if optimal treatment has been followed      #
#           beyond this decision point.                                #
# prWgt   : product of probabilities for positive treatment beyond this#
#           decision point                                             #
# response: sum of reward including this decision point.               #
# txVec   : recast treatment vector as +/-1                            #
# data    : data.frame of covariates and treatment histories           #
# kernel  : character description of kernel function                   #
# kparam  : numeric parameter for kernel function                      #
# lambdas : tuning parameters                                          #
# cvFolds : number of cross-validation Folds                           #
# suppress: T/F indicating if print to screens should be executed      #
#----------------------------------------------------------------------#
.BOWLBasic <- function(regime, 
                       txInfo, 
                       ind, 
                       prWgt, 
                       response, 
                       txVec, 
                       data,
                       kernel, 
                       kparam, 
                       lambdas, 
                       cvFolds, 
                       suppress){

  if( !suppress ) {
    cat("BOWL Analysis for binary treatment.\n")
  }

  #------------------------------------------------------------------#
  # Number of patients in data                                       #
  #------------------------------------------------------------------#
  ns <- nrow(data)

  #------------------------------------------------------------------#
  # Limit analysis to those patients that followed the opt treatment #
  # regime to this stage                                             #
  #------------------------------------------------------------------#
  if( !any(ind) ) {
    UserError("input",
              "no data given for optimization step")
  }

  if( !suppress ) {
    cat("Analysis includes", sum(ind), "patients.\n")
  }

  #------------------------------------------------------------------#
  # Obtain the model matrix for the optimization step                #
  #------------------------------------------------------------------#
  x <- try(model.matrix(regime,data), silent = TRUE)

  if( is(x,"try-error") ) {
    UserError("input",
              paste("at least one element of the",
                    "covariates in regime was not",
                    "found in data"))
  }

  #------------------------------------------------------------------#
  # Determine if decision function model includes an intercept.      #
  #------------------------------------------------------------------#
  hasIntercept <- attr(terms(regime),"intercept") == 1L
  if( hasIntercept ) x <- x[,-1L]

  #------------------------------------------------------------------#
  # Perform cross-validation                                         #
  #------------------------------------------------------------------#
  if( cvFolds > 0L ) {
    cvResult <- .crossValidation2Par(optimFunc = ".newOWLOptim",
                                     valueFunc = ".valueFuncOWL",
                                     response = response[ind],
                                     prWgt = prWgt[ind],
                                     x = x[ind,],
                                     txVec = txVec[ind],
                                     cvFolds = cvFolds,
                                     lambdas = lambdas,
                                     kernel = kernel,
                                     kparam = kparam,
                                     suppress = suppress)
  } else {
    cvResult <- list()
    cvResult$cv <- NULL
    cvResult$kparam <- kparam[1L]
    cvResult$lambda <- lambdas[1L]
  }

  #------------------------------------------------------------------#
  # Obtain training results at the lambda and kernel parameter.      #
  #------------------------------------------------------------------#
  trainResult <- .newOWLOptim(x = x[ind,],
                              subset = 1L:sum(ind),
                              lambda = cvResult$lambda, 
                              txVec = txVec[ind],
                              prWgt = prWgt[ind],
                              response = response[ind],
                              suppress = suppress,
                              kernel = kernel,
                              kparam = cvResult$kparam)

  if( is(trainResult, "NULL") ) return(NULL)

  #------------------------------------------------------------------#
  # Determine optimal tx for all patients eligible for binary tx.    #
  # Note that any patient that did not receive optimal treatment     #
  # in all later decision points will have optVec = NA and df = NA   #
  #------------------------------------------------------------------#
  optVec <- rep(NA, ns)
  df <- rep(NA,ns)

  pred <- .predictOptimalTx(x = trainResult, 
                            newdata = x[ind,,drop=FALSE])
  optVec[ind] <- pred$optimalTx
  df[ind] <- pred$decisionFunc

  #------------------------------------------------------------------#
  # Estimate value                                                   #
  #------------------------------------------------------------------#
  value <- .valueFuncOWL(subset = 1L:ns, 
                         optTx = optVec, 
                         txVec = txVec,
                         prWgt = prWgt, 
                         response = response*ind)

  result <- new("BOWLBasic",
                "estVal"          = value,
                "decisionFunc"    = df,
                "optTx"           = optVec,
                "regime"          = regime,
                "crossValidation" = cvResult$cv,
                trainResult)

  return(result)
}

setMethod(f = ".newBOWLBasic",    
          signature = c(regime = "formula",
                        txInfo = "TxInfoNoSubsets"), 
          definition = .BOWLBasic)
