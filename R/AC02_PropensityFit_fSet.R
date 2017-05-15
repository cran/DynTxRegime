# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                     CLASS PropensityFit_fSet                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Propensity regression with subsets but no subset modeling.           #
# Appropriate only for Responder/Non-Responder type scenarios.         #
#----------------------------------------------------------------------#
#   txInfo : treatment information                                     #
#   extends PropensityFit                                              #
#----------------------------------------------------------------------#
setClass("PropensityFit_fSet",
         slots = c(txInfo = "TxInfoWithSubsets",
                   small = "logical",
                   levs  = "character"),
         contains = c("modelObjFit",
                      "PropensityRegression",
                      "SubsetsModeled", 
                      "SingleDecisionPoint"))


# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    PropensityFit_fSet METHODS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# Predict propensity for treatment                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityFit_fSet                       #
# newdata: if given, data.frame of covariates and treatment history    #
#   returns                                                            #
# matrix of propensity for each treatment                              #
#----------------------------------------------------------------------#
.predictNew_fSet <- function(object, newdata) {

  superSet <- .getSuperSet(object@txInfo)

  if( !missing(newdata) ) {
    #--------------------------------------------------------------#
    # Process treatment information for new data.                  #
    #--------------------------------------------------------------#
    object@txInfo <- .newTxInfo(fSet = .getSubsetRule(object@txInfo), 
                                txName = .getTxName(object@txInfo), 
                                data = newdata,
                                suppress = TRUE,
                                verify = FALSE)
  }

  #------------------------------------------------------------------#
  # Identify any patients for whom only 1 tx is available.           #
  #------------------------------------------------------------------#
  singles <- .getSingleton(object@txInfo)

  #------------------------------------------------------------------#
  # Setup result matrix                                              #
  #------------------------------------------------------------------#
  n <- length(singles)

  mm2 <- matrix(data = 0.0, 
                nrow = n, 
                ncol = length(superSet),
                dimnames = list(NULL, superSet))

  #------------------------------------------------------------------#
  # For patients with more than 1 tx option, predict propensity      #
  #------------------------------------------------------------------#
  if( !missing(newdata) ) {
    mm <- predict(object = as(object, "modelObjFit"),
                  newdata = newdata[!singles,,drop=FALSE])
  } else {
    mm <- predict(object = as(object, "modelObjFit"))
  }

  if( any(mm < -1.5e-8) ) {
    UserError("input", 
              "Propensity model returns negative probabilities")
  }

  #------------------------------------------------------------------#
  # If not all treatments are represented, add missing propensity    #
  #------------------------------------------------------------------#
  if( ncol(mm) != length(object@levs) ) {
    correction <- 1.0 - rowSums(mm)

    if( object@small ) {
      mm <- cbind(correction, mm)
    } else {
      mm <- cbind(mm, correction)
    }

  }

  colnames(mm) <- object@levs
  cols <- match(object@levs,superSet)

  mm2[!singles,cols] <- mm

  if( any(singles) ) {

    subsets <- .getSubsets(object@txInfo)
    ptsSubset <- .getPtsSubset(object@txInfo)

    for( i in 1L:length(subsets) ) {
      if( length(subsets[[i]]) == 1L ) {
         tst <- ptsSubset == names(subsets)[i]
         cols <- superSet %in% subsets[[i]]
         mm2[tst,cols] <- 1.0
      }
    }

  }

  return( mm2 )
}
  

setMethod(f = "predict", 
          signature = c(object = "PropensityFit_fSet"), 
          definition = .predictNew_fSet )
