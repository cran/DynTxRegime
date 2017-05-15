#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                            Class EARLIPWE                            #
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
setClass(Class = "EARLIPWE",
         contains = c("EARL", "PropensityOnly"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                           EARLIPWE METHODS                           #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARLIPWE                                 #
#   returns                                                            #
# String indicating EARLIPWE estimator                                 #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "EARLIPWE"), 
          definition = function(object){
                         return("Efficient Augmentation and Relaxation Learning IPWE")
                       } )

#----------------------------------------------------------------------#
# Print key results.                                                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class EARLAIPWE                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "EARLIPWE"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         print(x = as(x,"EARL"))
                         print(x = as(x,"PropensityOnly"))
                         cat("\nEstimated Value:", estimator(x),"\n\n")
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARLAIPWE                                #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "EARLIPWE"), 
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         show(object = as(object,"EARL"))
                         show(object = as(object,"PropensityOnly"))
                         cat("\nEstimated Value:", estimator(object),"\n\n")

                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the propensity and outcome regressions#
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARLIPWE                                 #
#   returns                                                            #
# a list with a three elements, 'propensity', holding the summary      #
# of the fit object returned by the propensity regression method.      #
# 'optim' holds the key results of the optimization as defined in      #
# RWLOptim. 'outcome' hold the summary object defined for the          #
# OutcomeRegression object; 'value' is the estimated value             #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "EARLIPWE"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"EARL"))
                         result <- c(result, summary(object = as(object,"PropensityOnly")))
                         return(result)
                       } )


