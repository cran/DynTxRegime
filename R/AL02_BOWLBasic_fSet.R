#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                           Class BOWLBasic_fSet                       #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of a call to BOWL with Binary treatment options              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# optTx           : numeric vector of optimal treatments               #
# estVal          : estimated value of treatment                       #
# regime          : formula, model to be used for optimization         #
# crossValidation : matrix of crossValidation results if performed     #
# optim           : OWLOptim object                                    #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
.checkBOWLBasic_fSet <- function(object){

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

setClass(Class = "BOWLBasic_fSet",
         slots = c(txInfo = "TxSubset"),
         contains = c("BOWLBasic"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                        BOWLBasic_fSet METHODS                        #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#----------------------------------------------------------------------#
# Predict the optimal treatment                                        #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLBasic_fSet                                #
#   returns                                                            #
# a numeric vector +/- 1                                               #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",
          signature = c(x = "BOWLBasic_fSet", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){

                         newTx <- .newTxInfo(fSet = .getSubsetRule(x@txInfo),
                                             txName = .getTxName(x@txInfo),
                                             data = newdata,
                                             suppress = TRUE,
                                             verify = FALSE)

                         singles <- .getSingleton(newTx)

                         x2 <- try(model.matrix(object = x@regime,
                                               data = newdata[!singles,,drop=FALSE]), 
                                   silent = TRUE)

                         if( is(x2,"try-error") ) {
                           UserError("input",
                                     paste("unable to identify needed",
                                           "variables in newdata"))
                         }

                         hasIntercept <- attr(terms(x@regime),
                                              "intercept") == 1L
                         if( hasIntercept ) x2 <- x2[,-1L]

                         opt <- .predictOptimalTx(x = as(x,"OWLOptim"), 
                                                  newdata = x2)

                         optimalTx <- length(singles)
                         df <- length(singles)

                         optimalTx[!singles] <- opt$optimalTx
                         df[!singles] <- opt$decisionFunc

                         optimalTx[singles] <- -1.0
                         df[singles] <- NA

                         return( list("optimalTx" = optimalTx, "decisionFunc" = df) )
                       } )

#----------------------------------------------------------------------#
# Create a new BOWLBasic_fSet object when single decision function and #
# propensity uses subset modelling.                                    #
#----------------------------------------------------------------------#
# regime  : formula description of decision rule                       #
# txInfo  : TxInfoWithSubsets object                                   #
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
.BOWLBasic_fSet <- function(regime, 
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
  # Determine which pts are associated with a binary treatment       #
  #------------------------------------------------------------------#
  usePts <- !.getSingleton(txInfo)

  if( sum(usePts) == 0L ) {
     UserError("input", 
               "no observations have binary feasible treatments")
  }

  #------------------------------------------------------------------#
  # Limit analysis to those patients that followed the opt treatment #
  # regime after this stage and are in the subgroup of pts with      #
  # binary tx.                                                       #
  #------------------------------------------------------------------#
  idx <- ind & usePts

  if( !any(idx) ) {
    UserError("input",
              "no data given for optimization step")
  }

  if( !suppress ) {
    cat("Analysis includes", sum(idx), "patients.\n")
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
                                     response = response[idx],
                                     prWgt = prWgt[idx],
                                     x = x[idx,],
                                     txVec = txVec[idx],
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
  trainResult <- .newOWLOptim(x = x[idx,],
                              subset = 1L:sum(idx),
                              lambda = cvResult$lambda, 
                              txVec = txVec[idx],
                              prWgt = prWgt[idx],
                              response = response[idx],
                              suppress = suppress,
                              kernel = kernel,
                              kparam = cvResult$kparam)

  if( is(trainResult, "NULL") ) return(NULL)

  #------------------------------------------------------------------#
  # Determine optimal tx for all patients eligible for binary tx.    #
  # Note that any patient that did not receive optimal treatment     #
  # in all later decision points will have optVec = NA and df = NA   #
  # Patients for whom only 1 tx is available will have optVec = -1   #
  # and df = NA                                                      #
  #------------------------------------------------------------------#
  optVec <- rep(-1.0, ns)
  df <- numeric(ns)
  df[] <- NA

  pred <- .predictOptimalTx(x = trainResult, 
                            newdata = x[idx,,drop=FALSE])
  optVec[idx] <- pred$optimalTx
  df[idx] <- pred$decisionFunc
  optVec[!ind] <- NA
  df[!ind] <- NA

  #------------------------------------------------------------------#
  # Estimate value                                                   #
  #------------------------------------------------------------------#
  value <- .valueFuncOWL(subset = 1L:ns, 
                         optTx = optVec, 
                         txVec = txVec,
                         prWgt = prWgt, 
                         response = response*idx)

  result <- new("BOWLBasic_fSet",
                "txInfo"          = txInfo,
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
                        txInfo = "TxInfoWithSubsets"), 
          definition = .BOWLBasic_fSet)
