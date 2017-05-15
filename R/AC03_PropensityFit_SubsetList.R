# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                  CLASS PropensityFit_SubsetList                  + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Propensity fit objects when subsets are modeled separately.          #
#----------------------------------------------------------------------#
# txInfo : TxInfoWithSubsets object                                    #
# extends SubsetList class                                             #
#----------------------------------------------------------------------#
.checkValidity_PropensityFit_SubsetList <- function(object){

  errors <- character()

  for( i in 1L:length(object) ) {

    if( !(is(object[[i]], "PropensityFit")) ) {
      msg <- paste("You are attempting to create a",
                   "PropensityFit_SubsetList with",
                   "an object that is not PropensityFit",
                   class(object[[i]]))
      errors <- c(errors, msg)
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "PropensityFit_SubsetList",
         slots = c(txInfo = "TxInfoWithSubsets"),
         contains = c("SubsetList", 
                      "PropensityRegression",
                      "SubsetsModeled"),
         validity = .checkValidity_PropensityFit_SubsetList)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                 PropensityFit_SubsetList METHODS                 + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Predict propensity for treatment                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityFit_SubsetList                 #
#   returns                                                            #
# matrix of propensity for each treatment                              #
#----------------------------------------------------------------------#
.predictPropensitySubset <- function(object, newdata, ...) {

  #------------------------------------------------------------------#
  # Retrieve superset of treatment options.                          #
  #------------------------------------------------------------------#
  levs <- .getSuperSet(object@txInfo)

  if( missing(newdata) ) {

    #--------------------------------------------------------------#
    # Retrieve treatment subsets available to each patient         #
    #--------------------------------------------------------------#
    fSetPtsSubset <- .getPtsSubset(object@txInfo)

    #--------------------------------------------------------------#
    # Storage matrix for all propensities.                         #
    #--------------------------------------------------------------#
    result <- matrix(data = NA, 
                     nrow = length(fSetPtsSubset), 
                     ncol = length(levs),
                     dimnames = list(NULL, levs))

    for( i in 1L:length(object) ) {
      #----------------------------------------------------------#
      # Retrieve subset names included in this fit               #
      #----------------------------------------------------------#
      nm <- names(object)[i]
      subsets <- unlist(strsplit(nm,","))

      #----------------------------------------------------------#
      # Call predict method of PropensityFit object              #
      #----------------------------------------------------------#
      mm <- predict(object[[i]])

      #----------------------------------------------------------#
      # Identify patients that are eligible for the subsets      #
      #----------------------------------------------------------#
      u4f <- fSetPtsSubset %in% subsets

      result[u4f, colnames(mm)] <- mm
    }

    subsets <- .getSubsets(object@txInfo)
    for( i in 1L:length(subsets) ) {
      if( length(subsets[[i]]) == 1L ) {
        reset <- fSetPtsSubset == names(subsets)[i]
        result[reset, as.character(subsets[[i]][1L])] <- 1.0
      }
    }

  } else {


    #--------------------------------------------------------------#
    # Determine treatment subsets available to each patient        #
    #--------------------------------------------------------------#
    newTxInfo <- .newTxInfo(fSet = .getSubsetRule(object@txInfo), 
                            txName = .getTxName(object@txInfo), 
                            data = newdata,
                            suppress = TRUE, verify = FALSE)

    fSetPtsSubset <- .getPtsSubset(newTxInfo)

    #--------------------------------------------------------------#
    # Storage matrix for all propensities.                         #
    #--------------------------------------------------------------#
    result <- matrix(data = NA, 
                     nrow = length(fSetPtsSubset), 
                     ncol = length(levs),
                     dimnames = list(NULL, levs))

    for( i in 1L:length(object) ) {
      #----------------------------------------------------------#
      # Retrieve subset names included in this fit               #
      #----------------------------------------------------------#
      nm <- names(object)[i]
      subsets <- unlist(strsplit(nm,","))

      #----------------------------------------------------------#
      # Identify patients that are eligible for the subsets      #
      #----------------------------------------------------------#
      use4pred <- fSetPtsSubset %in% subsets
      if( !any(use4pred) ) next

      #----------------------------------------------------------#
      # Call predict method of PropensityFit object              #
      #----------------------------------------------------------#
      mm <- predict(object = object[[i]], 
                    newdata[use4pred,,drop=FALSE])

      if( any(mm < -1.5e-8) ) {
        UserError("input", 
                  "Propensity model returns negative probabilities")
      }

      result[use4pred, colnames(mm)] <- mm

    }

    subsets <- .getSubsets(object@txInfo)
    for( i in 1L:length(subsets) ) {
      if( length(subsets[[i]]) == 1L ) {
        reset <- fSetPtsSubset == names(subsets)[i]
        result[reset, as.character(subsets[[i]][1L])] <- 1.0
      }
    }


  }

  

  return( result )

}

#----------------------------------------------------------------------#
# Predict propensity for treatment                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityFit_SubsetList                 #
# newdata: if given, data.frame of covariates and treatment history    #
#   returns                                                            #
# matrix of propensity for each treatment                              #
#----------------------------------------------------------------------#
setMethod(f = "predict", 
          signature = c(object = "PropensityFit_SubsetList"), 
          definition = .predictPropensitySubset )


