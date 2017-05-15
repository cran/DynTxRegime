# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        SubsetListFit CLASS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of an outcome regression call obtained using subset models   #
#----------------------------------------------------------------------#
#  Inherits SubsetList where each element is class OutcomeRegression   #
#----------------------------------------------------------------------#
.checkValidity_SubsetListFit <- function(object){

  errors <- character()

  #------------------------------------------------------------------#
  # Each element must be an object of class OutcomeRegression        #
  #------------------------------------------------------------------#
  for( i in 1L:length(object) ) {
    if( !is(object[[i]], "OutcomeRegression") ) {
      msg <- "all elements of SubsetListFit must be OutcomeRegression"
      errors <- c(errors, msg)
      break
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "SubsetListFit",
         slots = c("txInfo" = "TxSubset"),
         contains = c("SubsetList", 
                      "OutcomeRegression",
                      "SubsetsModeled"),
         validity = .checkValidity_SubsetListFit)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      SubsetListFit METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Predict the outcome regression for all treatment options             #
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class IterateFitWithSubsets.                   #
# data : data.frame of covariates and treatment history                #
#   returns                                                            #
# A matrix. The estimated outcome.                                     #
#----------------------------------------------------------------------#
setMethod(f = ".predictAllTreatments", 
          signature = c(object = "SubsetListFit"), 
          definition = function(object, data, response){ 

                         subsets <- .getSubsets(object@txInfo)
                         ptsSubset <- .getPtsSubset(object@txInfo)
                         superSet <- .getSuperSet(object@txInfo)
                         txName <- .getTxName(object@txInfo)

                         res <- matrix(data = NA, 
                                       nrow = nrow(data), 
                                       ncol = length(superSet),
                                       dimnames = list(NULL,superSet))

                         nms <- NULL

                         for( i in 1L:length(object) ) {

                           objName <- unlist(strsplit(names(object)[i],","))
                           nms <- c(nms, objName)

                           tst <- ptsSubset %in% objName
                           df <- data[tst,,drop=FALSE]

                           tst2 <- which(names(subsets) %in% objName)

                           for( k in 1L:length(tst2) ) {

                             for( j in 1L:length(subsets[[tst2[k]]]) ) {

                               df[,txName] <- subsets[[tst2[k]]][j]
                               df[,txName] <- .convertTx(object@txInfo, df[,txName])

                               val <- predict(object = object[[i]], newdata = df)

                               tst3 <- superSet %in% subsets[[tst2[k]]][j]

                               res[tst,tst3] <- val
                             }
                           }
                         }

                         singletons <- .getSingleton(object@txInfo)

                         if( any(singletons) ) {
                           for( i in 1L:length(subsets) ) {
                             if( names(subsets)[i] %in% nms ) next
                             tst <- ptsSubset == names(subsets)[i] & singletons
                             if( any(tst) ) {
                               tst2 <- superSet %in% subsets[[i]]
                               res[tst, tst2] <- response[tst]
                             }
                           }
                         }

                         return(list(vals = res, ss = superSet))
                       } )


setMethod(f = "predict", 
          signature = c(object = "SubsetListFit"), 
          definition = function(object, newdata){ 

                         if( missing(newdata) ) {

                           ptsSubset <- .getPtsSubset(object@txInfo)
                           res <- numeric(length(ptsSubset))
                           for( i in 1L:length(object) ) {

                             objName <- unlist(strsplit(names(object)[i],","))
                             tst <- ptsSubset %in% objName

                             val <- predict(object = object[[i]])
 
                             res[tst] <- val
                           }

                         } else {
                           res <- numeric(nrow(newdata))
                           res[] <- NA

                           txInfo <- .newTxInfo(fSet = .getSubsetRule(object@txInfo),
                                                txName = .getTxName(object@txInfo),
                                                data = newdata,
                                                suppress = TRUE,
                                                verify = FALSE)

                           ptsSubset <- .getPtsSubset(txInfo)

                           for( i in 1L:length(object) ) {
                             objName <- unlist(strsplit(names(object)[i],","))
                             tst <- ptsSubset %in% objName
                             if( !any(tst) ) next
                             df <- newdata[tst,,drop=FALSE]
                             val <- predict(object = object[[i]], newdata = df)
                             res[tst] <- drop(val)
                           }
                         }

                         return(res)
                       } )
