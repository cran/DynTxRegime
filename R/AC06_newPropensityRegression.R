setGeneric(name = ".newPropensityRegression", 
           def = function(moPropen, txInfo, ...){
                   standardGeneric(".newPropensityRegression")
                 } )


#----------------------------------------------------------------------#
# Fit propensity models.                                               #
#----------------------------------------------------------------------#
.propenFit <- function(moPropen, txInfo, data, suppress){

  #------------------------------------------------------------------#
  # Determine if user specified that a specific component of the     #
  # subset will be missing from predictions. Remove this from the    #
  # argument list to avoid errors in fit routines                    #
  #------------------------------------------------------------------#
  predArgs <- predictorArgs(moPropen)

  if( moPropen@predictor@propenMissing == "smallest" ) {
    small <- TRUE
  } else {
    small <- FALSE
  }

  #------------------------------------------------------------------#
  # Retrieve the column header of data that contains the tx variable #
  #------------------------------------------------------------------#
  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Retrieve the levels present in the data. Necessary because a     #
  # subset of data may have been passed in and levels() is not       #
  # correctly specified.                                             #
  #------------------------------------------------------------------#
  levs <- .getLevels(txInfo, data[,txName])

  if( !suppress ) {
    cat("Regression analysis of propensity for treatment.\n")
    cat("Treatment levels: {", levs, "}.\n")
    cat("Total number of patients included in analysis: ",
        nrow(data), ".\n")
    if( small ) {
      cat("Assumed that any missing predictions are for treatment",
          levs[1L], ".\n")
    } else {
      cat("Assumed that any missing predictions are for treatment",
          levs[length(levs)], ".\n")
    }
  }

  #------------------------------------------------------------------#
  # Fit model.                                                       #
  #------------------------------------------------------------------#
  it <- which( colnames(data) == txName )

  fitResult <- try(fit(object = moPropen, 
                       data = data[,-it,drop=FALSE], 
                       response = data[,txName]),
                   silent = TRUE)

  #------------------------------------------------------------------#
  # Stop if unable to obtain a fit                                   #
  #------------------------------------------------------------------#
  if( is(fitResult, "try-error") ) {

    fitResult <- try(fit(object = moPropen, 
                         data = data[,-it,drop=FALSE], 
                         response = as.factor(data[,txName])),
                     silent = FALSE)

    if( is(fitResult, "try-error") ) {
      UserError("input",
                "unable to obtain fit for propensity")
    }
  }

  if( !suppress ) print(fitResult)


  res <- new("PropensityFit",
             "small" = small,
             "levs"  = as.character(levs),
             fitResult)

  return( res )

}

setMethod(f = ".newPropensityRegression", 
          signature = c(moPropen = "modelObj",
                        txInfo   = "TxInfoNoSubsets"), 
          definition = .propenFit)

#----------------------------------------------------------------------#
# Fit propensity models.                                               #
#----------------------------------------------------------------------#
.propenFit_fSet <- function(moPropen, txInfo, data, suppress){

  #------------------------------------------------------------------#
  # Eliminate patients for whom only 1 treatment is available        #
  #------------------------------------------------------------------#
  fSetPtsSubset <- .getPtsSubset(txInfo)
  if( length(fSetPtsSubset) == nrow(data) ) {
    use <- !.getSingleton(txInfo)
    data <- data[use,]
  }

  fitResult <- .propenFit(moPropen = moPropen, 
                          txInfo = txInfo, 
                          data = data, 
                          suppress = suppress)

  res <- new("PropensityFit_fSet",
             "txInfo" = txInfo,
             "small" = fitResult@small,
             "levs" = fitResult@levs,
             "fitObj" = fitResult@fitObj,
             "model" = fitResult@model,
             "func" = fitResult@func)

  return( res )

}

setMethod(f = ".newPropensityRegression", 
          signature = c(moPropen = "modelObj",
                        txInfo   = "TxInfoWithSubsets"), 
          definition = .propenFit_fSet)

#----------------------------------------------------------------------#
# Fit propensity models.                                               #
#----------------------------------------------------------------------#
.propensitySubsetFit <- function(moPropen, txInfo, data, suppress) {

  #------------------------------------------------------------------#
  # Retrieve the feasible treatment subset for each patient          #
  #------------------------------------------------------------------#
  fSetPtsSubset <- .getPtsSubset(txInfo)

  fitObj <- list()

  for( i in 1L:length(moPropen) ){

    #--------------------------------------------------------------#
    # Retrieve the subset names for which this model is to be used #
    #--------------------------------------------------------------#
    nm <- names(moPropen@loo)[i]
    modelSubset <- unlist(strsplit(nm,","))

    if( !suppress ) {
      cat("Subset(s)", paste(modelSubset, collapse = " ,"), "\n")
    }

    #--------------------------------------------------------------#
    # Identify patients that will be used to obtain fit.           #
    #--------------------------------------------------------------#
    use4fit <- fSetPtsSubset %in% modelSubset

    if( !any(use4fit) ) {
      UserError("input", 
                paste("No observations match moPropen model subset:",
                      nm))
    }

    if( all(.getSingleton(txInfo)[use4fit]) ) {
      stop("modeling a subset of data that has only 1 tx option")
    }

    ntx <- .newTxInfo(fSet = NULL, 
                      txName = .getTxName(txInfo), 
                      data = data[use4fit,,drop=FALSE], 
                      suppress = TRUE,
                      verify = FALSE)

    fitObj[[nm]] <- .newPropensityRegression(moPropen = moPropen[[i]], 
                                             txInfo = ntx,
                                             data = data[use4fit,,drop=FALSE],
                                             suppress = suppress)

  }

  res <- new("PropensityFit_SubsetList",
             "loo" = fitObj,
             "txInfo" = txInfo)

  return( res )
}

setMethod(f = ".newPropensityRegression", 
          signature = c(moPropen = "ModelObj_SubsetList",
                        txInfo   = "TxInfoWithSubsets"), 
          definition = .propensitySubsetFit)

setMethod(f = ".newPropensityRegression", 
          signature = c(moPropen = "ModelObj_DecisionPointList",
                        txInfo   = "TxInfoList"), 
          definition = function(moPropen, txInfo, data, suppress){

                         nDP <- length(moPropen)

                         fitObj <- list()

                         for( j in 1L:nDP ){

                           if( !suppress && nDP > 1L ) {
                             cat("Decision point", j, "\n")
                           }

                           fitObj[[j]] <- .newPropensityRegression(moPropen = moPropen[[j]],
                                                                   txInfo = txInfo[[j]],
                                                                   data = data,
                                                                   suppress = suppress)
                         }

                         result <- new("PropensityFit_DecisionPointList",
                                       "loo" = fitObj)

                         return(result)

                       } )

setMethod(f = ".newPropensityRegression", 
          signature = c(moPropen = "ModelObj_SubsetList_DecisionPointList",
                        txInfo   = "TxInfoList"), 
          definition = function(moPropen, txInfo, data, suppress){

                         nDP <- length(moPropen)

                         fitObj <- list()

                         for( j in 1L:nDP ){

                           if( !suppress && nDP > 1L ) {
                             cat("Decision point", j, "\n")
                           }

                           fitObj[[j]] <- .newPropensityRegression(moPropen = moPropen[[j]],
                                                                  txInfo = txInfo[[j]],
                                                                  data = data,
                                                                  suppress = suppress)
                         }

                         result <- new("PropensityFit_SubsetList_DecisionPointList",
                                       "loo" = fitObj)

                         return(result)

                       } )
