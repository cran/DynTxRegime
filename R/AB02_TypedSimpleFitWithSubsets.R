# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                 TypedSimpleFitWithSubsets CLASS                  + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of an outcome regression call obtained with a single model   #
# Subsets are specified, but not modeled independently                 #
#----------------------------------------------------------------------#
# Extends TypedSimpleFitNoSubsets
#
# slots:
#
#   type : a string indicating 'Combined', 'moMain' or 'moCont'
#   txInfo : TxInfoWithSubsets
#   fitObj : ANY, value returned by fitting function
#
#----------------------------------------------------------------------#
setClass(Class = "TypedSimpleFitWithSubsets",
         slot = c(txInfo = "TxInfoWithSubsets"),
         contains = c("TypedSimpleFit"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                 TypedSimpleFitWithSubsets METHODS                + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Predict the outcome regression for all treatment options             #
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class TypedSimpleFitWithSubsets.               #
# data : data.frame of covariates and treatment history                #
#   returns                                                            #
# A matrix. The estimated outcome.                                     #
#----------------------------------------------------------------------#
setMethod(f = ".predictAllTreatments",
          signature = c(object = "TypedSimpleFitWithSubsets"),
          definition = function(object, data, response=NULL){

          	         txSet <- .getSuperSet(object@txInfo)
                         txName <- .getTxName(object@txInfo)

                         val <- .predictAllTreatments(object = as(object,"TypedSimpleFit"),
                                                      data = data,
                                                      response = response)
                         val <- val$vals

          	         txInfo <- .newTxInfo(fSet = .getSubsetRule(object@txInfo),
                                              txName = .getTxName(object@txInfo),
                                              data = data,
                                              suppress = TRUE,
          	              	              verify = FALSE)

                         ptsSubset <- .getPtsSubset(txInfo)
                         subsets <- .getSubsets(object@txInfo)
                         for( i in 1L:length(subsets) ) {
                           tst1 <- ptsSubset %in% names(subsets)[i]
                           tst2 <- txSet %in% subsets[[i]]
                           val[tst1,!tst2] <- NA
                         }

                         return(list(vals = val, ss = txSet))
                       } )

#----------------------------------------------------------------------#
# Predict the outcome regression for all treatment options             #
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class TypedSimpleFitWithSubsets.               #
# data : data.frame of covariates and treatment history                #
#   returns                                                            #
# A matrix. The estimated outcome.                                     #
#----------------------------------------------------------------------#
setMethod(f = "predict",
          signature = c(object = "TypedSimpleFitWithSubsets"),
          definition = function(object, newdata, ...){


                         if( !missing(newdata) ) {
                           vals <- predict(object = as(object,"TypedSimpleFit"), newdata = newdata)
                           txInfo <- .newTxInfo(fSet = .getSubsetRule(object@txInfo),
                                                txName = .getTxName(object@txInfo),
                                                data = newdata,
                                                suppress = TRUE,
                                                verify = FALSE)

                           ptsSubset <- .getPtsSubset(txInfo)
                           subsets <- .getSubsets(txInfo)
                           txName <- .getTxName(txInfo)

                           for( i in 1L:length(subsets) ) {
                             tst <- ptsSubset == names(subsets)[i] &
                                    !{newdata[,txName] %in% subsets[[i]]}
                             vals[tst] <- NA
                           }
                         } else {
                           vals <- predict(object = as(object,"TypedSimpleFit"))
                         }

                         return(vals)
                       } )
