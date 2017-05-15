#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                              Class EARL                              #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of the Efficient Augmentation and Relaxation learning using  #
# the Inverse Probability Weighted Estimator                           #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# propensity      : PropensityFit object                               #
# optim           : EARLOptim object                                   #
# crossValidation : matrix of cross-validation results                 #
# regime          : model formula for decision rule                    #
# txInfo          : TxInfo object                                      #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setClass(Class = "EARL",
         slots = c(optim           = "EARLOptim",
                   crossValidation = "CVInfoOrNULL",
                   regime          = "formula",
                   txInfo          = "TxInfoNoSubsets",
                   decisionFunc    = "numeric"),
         contains = c("DynTxRegime"))


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                             EARL METHODS                             #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setMethod(f = "cvInfo",   
          signature = c(object = "EARL"), 
          definition = function(object) {
                         if( is(object@crossValidation,"NULL") ) {
                           return(NULL)
                         }
                         return(cvInfo(object@crossValidation))
                       })
#----------------------------------------------------------------------#
# Retrieve the results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARL                                     #
#   returns                                                            #
# object defined by EARLOptim method                                   #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="EARL"),
          definition = function(object, ...) { 
                         return( optimObj(object@optim) )
                       } )


#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class EARL                                          #
# newdata : a data.frame                                               #
#   returns                                                            #
# A integer or character vector of optimal treatments coded as given   #
# in original data                                                     #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "EARL", 
                        newdata = "data.frame"),
          definition = function (x, newdata, ...){

                         #-------------------------------------------#
                         # Create model matrix from decision rule    #
                         #-------------------------------------------#
                         x2 <- try(model.matrix(x@regime, newdata),
                                   silent = TRUE)

                         if( is(x2, "try-error") ) {
                           msg <- "newdata does not contain needed covariates"
                           UserError("input", msg)
                         }

                         #-------------------------------------------#
                         # Determine if there is an intercept, remove#
                         #-------------------------------------------#
                         if( attr(terms(x@regime), "intercept") == 1L ) {
                           x2 <- x2[,-1L,drop=FALSE]
                         }

                         #-------------------------------------------#
                         # Obtain prediction from EARLOptim method   #
                         #-------------------------------------------#
                         opt <- .predictOptimalTx(x = x@optim, 
                                                  newdata = x2)

                         #-------------------------------------------#
                         # Recast treatment into original tx coding. #
                         #-------------------------------------------#
                         fSetSuperSet <- .getSuperSet(x@txInfo)

                         temp <- rep(NA, nrow(x2))

                         temp[opt$optimalTx < -0.5] <- fSetSuperSet[1L]
                         temp[opt$optimalTx >  0.5] <- fSetSuperSet[2L]

                         return( list("optimalTx" = temp,
                                      "decisionFunc" = opt$decisionFunc) )
                       } )

setMethod(f = "optTx",
          signature = c(x = "EARL", 
                        newdata = "missing"),
          definition = function(x, newdata,....){
                         return(list("optimalTx" = x@optimalTx,
                                     "decisionFunc" = x@decisionFunc))
                       })
#----------------------------------------------------------------------#
# Print key results.                                                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class EARL                                          #
#   returns                                                            #
# Calls print methods of EARLOptim and PropensityFit                   #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "EARL"), 
          definition = function(x, ...){
                         cat("\nRegime\n")
                         print(x@regime)

                         cat("\nOptimization\n")
                         print(x@optim, ...)
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARL                                     #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "EARL"), 
          definition = function(object, ...){
                         return( regimeCoef(object@optim) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARL                                     #
#   returns                                                            #
# Nothing returned                                                     #
# Calls show methods of OWLOptim, OutcomeRegression, and PropensityFit #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "EARL"), 
          definition = function(object){
                         cat("\nRegime\n")
                         show(object@regime)

                         cat("\nOptimization\n")
                         show(object@optim)
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the propensity regression             #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARL                                     #
#   returns                                                            #
# a list with three elements, 'propensity', holding the summary        #
# of the fit object returned by the propensity regression method.      #
# 'optim' holds the key results of the optimization as defined in      #
# EARLOptim. 'value' is the estimated value of the IPWE                #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "EARL"), 
          definition = function(object, ...){
                         result <- list()
                         result[[ "optim" ]] <- summary(object@optim, ...)
                         return(result)
                       } )
