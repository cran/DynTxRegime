setGeneric(name = ".newTxInfo", 
           def = function(fSet, txName, ...){
                   standardGeneric(".newTxInfo")
                 })
          
#----------------------------------------------------------------------#
# Create a new TxInfoNoSubsets object.                                 #
#----------------------------------------------------------------------#
# input arguments                                                      #
#  fSet     : NULL                                                     #
#  txName   : character object.                                        #
#             The column header of data corresponding to treatments.   #
#  data     : data.frame object                                        #
#             The covariate and treatment histories used for training. #
#  suppress : T/F indicating if print to screens should be executed.   #
#  verify   : T/F indicating if treatments should be verified          #
# returns                                                              #
#  an object of class TxInfoFactor or TxInfoInteger                    #
#----------------------------------------------------------------------#
.txInfoNoSubsets <- function(fSet, 
                             txName,  
                             data,  
                             suppress,  
                             verify = TRUE){

  #------------------------------------------------------------------#
  # Only one treatment name allowed                                  #
  #------------------------------------------------------------------#
  if( length(txName) != 1L ) {
    UserError("input",
              "txName must be of length 1")
  }

  #------------------------------------------------------------------#
  # Verify that txName is found in data                              #
  #------------------------------------------------------------------#
  txVec <- try(data[,txName], silent = TRUE)

  if( is(txVec,"try-error") ) {
    UserError("input",
              paste(txName, "not found in data"))
  }

  #------------------------------------------------------------------#
  # Verify that tx is an integer or a factor.                        #
  # If numeric, attempt to convert to integer                        #
  #------------------------------------------------------------------#
  if( !is(txVec,"integer") && !is(txVec,"factor") ) {
    if( is(txVec, "character") ) {
      txVec <- factor(txVec)
    } else {
      if( !isTRUE(all.equal(txVec, round(txVec,0L))) ) {
        UserError("input",
                  "treatment variable must be integer or factor")
      }
      txVec <- as.integer(round(txVec,0L))
    }
  }

  result <- .newTxInfoNoSubsets(txName = txName, 
                                txVec = txVec,
                                suppress = suppress)

  if( verify ) {
    if( length(.getSuperSet(result)) == 1L ) {
      stop("only one treatment identified")
    }
  }

  return( result )
}

setMethod(f = ".newTxInfo",   
          signature = c(fSet = "NULL",
                        txName = "character"), 
          definition = .txInfoNoSubsets)

.txInfoWithSubsets <- function(fSet, 
                               txName, 
                               data, 
                               suppress, 
                               verify = TRUE){

  if( length(txName) != 1L ) {
    UserError("input",
              "lengths of txName and fSet do not agree")
  }

  #------------------------------------------------------------------#
  # Verify that txName is found in data                              #
  #------------------------------------------------------------------#
  txVec <- try(data[,txName], silent = TRUE)

  if( is(txVec,"try-error") ) {
    UserError("input",
              paste(txName, "not found in data"))
  }

  #------------------------------------------------------------------#
  # Verify that tx is an integer or a factor.                        #
  # If numeric, attempt to convert to integer                        #
  #------------------------------------------------------------------#
  if( !is(txVec,"integer") && !is(txVec,"factor") ) {
    if( is(txVec, "character") ) {
      data[,txName] <- factor(txVec)
      txVec <- data[,txName]
    } else {
      if( !isTRUE(all.equal(txVec, round(txVec,0L))) ) {
        UserError("input",
                  "treatment variable must be integer or factor")
      }
      data[,txName] <- as.integer(round(data[,txName],0L))
      txVec <- data[,txName]
    }
  }

  #------------------------------------------------------------------#
  # Determine the treatment options available for this decision pt   #
  #------------------------------------------------------------------#
  if( is(txVec, "factor") ) {
    #--------------------------------------------------------------#
    # If txVec is factor, use levels to define super set.          #
    #--------------------------------------------------------------#
    txOptions <- levels(txVec)
  } else {
    #--------------------------------------------------------------#
    # If txVec is integer, get unique values to define super set   #
    #--------------------------------------------------------------#
    txOptions <- sort(unique(txVec))
  }

  #------------------------------------------------------------------#
  # Determine subset of treatments available to each patient         #
  #------------------------------------------------------------------#
  txSubsets <- .getFeasibleTx(superSet = txOptions, 
                              fSet = fSet, 
                              data = data,
                              suppress = suppress,
                              verify = verify)

  #------------------------------------------------------------------#
  # Create appropriate txInfoWithSubset object                       #
  #------------------------------------------------------------------#
  result <- .newTxInfoWithSubsets(txName = txName, 
                                  txVec = txVec, 
                                  fSet = fSet, 
                                  subsets = txSubsets$subsets, 
                                  ptsSubset = txSubsets$ptsSubset)

  return(result)
}

setMethod(f = ".newTxInfo",   
          signature = c(fSet = "function",
                        txName = "character"), 
          definition = .txInfoWithSubsets)

#----------------------------------------------------------------------#
# Create a new TxInfoList object.                                      #
#----------------------------------------------------------------------#
# fSet  : A list of functions.                                         #
#         This argument allows the user to specify the subset of tx    #
#         options available to a patient or the subset of patients     #
#         that will be modeled uniquely.                               #
#         The functions should accept as input either                  #
#           1) explicit covariate names as given in column headers of  #
#              data                                                    #
#           2) a vector of covariates (i.e. a row of a data.frame)     #
#         and must return a list. The first element of the list is a   #
#         character giving a nickname to the subset. The second element#
#         is a vector of the tx options available to the subset.       #
#                                                                      #
# txName: character vector object.                                     #
#         The column header of data correspond to treatments.          #
#                                                                      #
# data  : data.frame object                                            #
#         The covariate and treatment histories used for training.     #
#                                                                      #
# suppress : logical                                                   #
#         If TRUE, status information is printed to terminal           #
#----------------------------------------------------------------------#
setMethod(f = ".newTxInfo",   
          signature = c(fSet = "list", 
                        txName = "list"), 
          definition = function(fSet, txName, data, suppress, verify=TRUE){ 

                         nDP <- length(txName)

                         if( length(fSet) != nDP ) {
                             UserError("input",
                                       paste("lengths of txName and",
                                             "fSet do not agree"))
                         }

                         result <- list()

                         for( i in 1L:nDP ) {

                           if( !suppress && nDP > 1L) {
                             cat("Decision point", i, "\n")
                           }

                           result[[i]] <- .newTxInfo(fSet = fSet[[i]], 
                                                     txName = txName[[i]], 
                                                     data = data,
                                                     suppress = suppress,
                                                     verify = verify)
                         }

                         if( nDP > 1L ) {
                           result <- new("TxInfoList",
                                         "loo" = result)
                         } else {
                           result <- result[[1L]]
                         }

                         return(result)
                       } )

#----------------------------------------------------------------------#
# Method to create a new TxInfo object.                                #
#----------------------------------------------------------------------#
# SINGLE OR MULTIPLE DECISION POINTS W/O SUBSET MODELING.              #
#----------------------------------------------------------------------#
# fSet  : NULL indicating that no subset modeling is used              #
#                                                                      #
# txName: character object.                                            #
#         The column header of data correspond to treatments.          #
#                                                                      #
# data  : data.frame object                                            #
#         The covariate and treatment histories used for training.     #
#                                                                      #
# suppress : logical                                                   #
#         If TRUE, status information is printed to terminal           #
#----------------------------------------------------------------------#
setMethod(f = ".newTxInfo",   
          signature = c(fSet = "NULL",
                        txName = "list"), 
          definition = function(fSet, txName, data, suppress, 
                                verify = TRUE){ 

                         nDP <- length(txName)

                         result <- list()

                         for( i in 1L:nDP ) {

                           if( !suppress && nDP > 1L) {
                             cat("Decision point", i, "\n")
                           }

                           result[[i]] <- .newTxInfo(fSet = NULL, 
                                                     txName = txName[[i]], 
                                                     data = data,
                                                     suppress = suppress,
                                                     verify = verify)
                         }

                         if( length(result) == 1L ) {
                           result <- result[[1L]]
                         } else {
                           result <- new("TxInfoList",
                                         "loo" = result)
                         }

                         return(result)

                       } )
