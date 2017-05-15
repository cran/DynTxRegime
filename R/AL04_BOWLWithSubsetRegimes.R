#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                     Class BOWLWithSubsetRegimes                      #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of a call to BOWL when subset modeling is used for regimes   #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# obj : list of BOWLBinary objects                                     #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setClass(Class = "BOWLWithSubsetRegimes",
         slots = c(txInfo = "TxInfoWithSubsets"),
         contains = c("SubsetList", "BOWLObj"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                   BOWLWithSubsetRegimes METHODS                      #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setMethod(f = "cvInfo",   
          signature = c(object = "BOWLWithSubsetRegimes"), 
          definition = function(object) {
                         res <- list()
                         for( i in 1L:length(object) ) {
                           res[[i]] <- cvInfo(object[[i]])
                         }
                         if( length(res) == 0L ) res <- NULL
                         return(res)
                       })

#----------------------------------------------------------------------#
# Retrieve the results of the optimizations step                       #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLWithSubsetRegimes                    #
#   returns                                                            #
# a list object containing key information from optimization routine   #
# Defined by OWLOptim                                                  #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="BOWLWithSubsetRegimes"),
          definition = function(object, ...){ 
                         obj <- list()
                         for( i in 1L:length(object) ) {
                           nm <- names(object)[i]
                           obj[[ nm ]] <- optimObj(object[[i]])
                         }

                         return( obj )

                       } )

#----------------------------------------------------------------------#
# Retrieve the estimated optimal tx for training data                  #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLWithSubsetRegimes                         #
#   returns                                                            #
# A vector of the estimated optimal tx for training data coded as +/-1 #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",
          signature = c(x = "BOWLWithSubsetRegimes", 
                        newdata = "missing"),
          definition = function(x, newdata, ...) {

                         ptsSubsets <- .getPtsSubset(x@txInfo)

                         resDF <- numeric(length(ptsSubsets))
                         resDF[.getSingleton(x@txInfo)] <- NA

                         resOT <- numeric(length(ptsSubsets))
                         resOT[.getSingleton(x@txInfo)] <- -1.0

                         for( i in 1L:length(x) ) {
                           tst <- .predictOptimalTx(x = x[[i]])
                           regimeName <- names(x)[i]
                           regimeName <- unlist(strsplit(regimeName,","))
                           used <- ptsSubsets %in% regimeName
                           resDF[used] <- tst$decisionFunc[used]
                           resOT[used] <- tst$optimalTx[used]
                         }

                         return(list("optimalTx" = resOT, 
                                     "decisionFunc" = resDF))
                         
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLWithSubsetRegimes                         #
# newdata : a data.frame                                               #
#   returns                                                            #
# estimated optimal treatment -/+ 1                                    #
#----------------------------------------------------------------------#
.predictOT <- function(x, newdata) {

  #------------------------------------------------------------------#
  # Number of new records.                                           #
  #------------------------------------------------------------------#
  n <- nrow(newdata)

  #------------------------------------------------------------------#
  # Create storage variable for optimal tx and decision function.    #
  #------------------------------------------------------------------#
  optimal <- rep(NA, n)
  fx <- rep(NA, n)

  #------------------------------------------------------------------#
  # Determine treatment options for new patients                     #
  #------------------------------------------------------------------#
  newTxInfo <- .newTxInfo(fSet = .getSubsetRule(x@txInfo), 
                          txName = .getTxName(x@txInfo), 
                          data = newdata,
                          suppress = TRUE,
                          verify = FALSE)

  #------------------------------------------------------------------#
  # For Binary treatments, predict optimal treatment from class obj  #
  #------------------------------------------------------------------#
  for( i in 1L:length(x) ) {

    #--------------------------------------------------------------#
    # Retrieve name of subset(s) for this object.                  #
    #--------------------------------------------------------------#
    nameRegime <- names(x)[i]
    regimeSubset <- unlist(strsplit(nameRegime,","))

    #--------------------------------------------------------------#
    # Identify new patients that are eligible for this subset      #
    #--------------------------------------------------------------#
    usePts <- .getPtsSubset(newTxInfo) %in% regimeSubset

    if( !any(usePts) ) next

    #--------------------------------------------------------------#
    # Predict the optimal treatment                                #
    #--------------------------------------------------------------#
    tempOpt <- .predictOptimalTx(x = x[[i]], 
                                 newdata = newdata[usePts,,drop=FALSE])

    optimal[usePts] <- tempOpt$optimalTx
    fx[usePts] <- tempOpt$decisionFunc
  }

  return(list("optimalTx"    = optimal,
              "decisionFunc" = fx))

}

setMethod(f = ".predictOptimalTx",
          signature = c(x = "BOWLWithSubsetRegimes", 
                        newdata = "data.frame"),
          definition = .predictOT)

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLWithSubsetRegimes                    #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "BOWLWithSubsetRegimes"), 
          definition = function(object, ...){

                         rc <- list()
                         for( i in 1L:length(object) ) {
                           rc[[ names(object)[i] ]] <- regimeCoef(object[[i]])
                         }
                         return( rc )  

                       } )

#----------------------------------------------------------------------#
# Create a new BOWLWithSubsetRegimes object                            #
#----------------------------------------------------------------------#
#   params                                                             #
# regime   : list of formulas giving covariates of kernel.             #
# txInfo   : TxInfo object                                             #
# ind      : T/F indicating if optimal tx followed after this dp       #
# prWgt    : product of probabilities for positive treatment beyond    #
#            this decision point                                       #
# response : sum of reward at this decision point.                     #
# txVec    : recast treatment vector as +/-1                           #
# data     : data.frame of covariates and treatment histories          #
# kernel   : character description of kernel function                  #
# kparam   : numeric parameter for kernel function                     #
# lambdas  : tuning parameters                                         #
# cvFolds  : number of cross-validation Folds                          #
# suppress : T/F indicating if print to screen executed                #
#   returns                                                            #
# an BOWLWithSubsetRegimes object                                      #
#----------------------------------------------------------------------#
.BOWLWithSubsetRegimes <- function(regime, 
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
                                   suppress) {

  if( !suppress ) {
    cat("BOWL Analysis for binary treatment with subsets.\n")
  }

  ns <- nrow(data)

  #------------------------------------------------------------------#
  # Retrieve feasible treatment subsets for each patient             #
  #------------------------------------------------------------------#
  ptsSubset <- .getPtsSubset(txInfo)

  #------------------------------------------------------------------#
  # Retrieve treatment subsets                                       #
  #------------------------------------------------------------------#
  subsets <- .getSubsets(txInfo)

  result <- list()

  for( i in 1L:length(regime) ) {

    nameRegime <- names(regime)[i]
    regimeSubset <- unlist(strsplit(nameRegime,","))

    if( !suppress ) {
      cat("Optimization for regime:", nameRegime, "\n")
    }

    #--------------------------------------------------------------#
    # Determine which pts are associated with the model subsets    #
    #--------------------------------------------------------------#
    usePts <- ptsSubset %in% regimeSubset

    if( sum(usePts) == 0L ) {
       UserError("input", 
                 paste("No observations match optimization model subset:",
                       nameRegime))
    }

    if( any(.getSingleton(txInfo)[usePts]) ) {
      UserError("input", 
                paste("An optimization model is trying to use a subset",
                      "of patients that have only one treatment",
                      "option available.",
                      nameRegime))
    }

    tempTx <- .newTxInfo(fSet = .getSubsetRule(txInfo), 
                         txName = .getTxName(txInfo),
                         data = data[usePts,,drop=FALSE],
                         suppress = TRUE,
                         verify = FALSE)

    result[[nameRegime]] <- .newBOWLBasic(regime = regime[[i]], 
                                          txInfo = tempTx, 
                                          ind = ind[usePts], 
                                          prWgt = prWgt[usePts], 
                                          response = response[usePts], 
                                          txVec = txVec[usePts],
                                          data = data[usePts,,drop=FALSE],
                                          kernel = kernel,
                                          kparam = kparam,
                                          lambdas = lambdas,
                                          cvFolds = cvFolds,
                                          suppress = suppress)
  }

  result <- new("SubsetList", loo = result)

  result <- new("BOWLWithSubsetRegimes",
                txInfo = txInfo,
                result)

  return( result )

}

setMethod(f = ".newBOWLOptimization",    
          signature = c(regime   = "list"), 
          definition = .BOWLWithSubsetRegimes )


