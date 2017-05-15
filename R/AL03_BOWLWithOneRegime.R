#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                       Class BOWLWithOneRegime                        #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of a call to BOWL without subset modeling regimes            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# obj : a BOWLBinary object                                            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setClass(Class = "BOWLWithOneRegime",
         contains = c("BOWLBasic"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                    BOWLWithOneRegime GENERICS                        #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setGeneric(name = ".newBOWLOptimization", 
           def = function(regime, ...){
                   standardGeneric(".newBOWLOptimization")
                 } )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                     BOWLWithOneRegime METHODS                        #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#----------------------------------------------------------------------#
# Create a new BOWLWithOneRegime object                                #
#----------------------------------------------------------------------#
#   params                                                             #
# regime   : formula                                                   #
# txInfo   : TxInfo object                                             #
# ind      : T/F indicating if optimal tx followed after this dp       #
# prWgt    : product of probabilities for positive treatment beyond    #
#            this decision point                                       #
# sumR     : sum of reward at this decision point.                     #
# txVec    : recast treatment vector as +/-1                           #
# data     : data.frame of covariates and treatment histories          #
# kernel   : character description of kernel function                  #
# kparam   : numeric parameter for kernel function                     #
# lambdas  : tuning parameters                                         #
# cvFolds  : number of cross-validation Folds                          #
# suppress : T/F indicating if print to screen executed                #
#   returns                                                            #
# an BOWLWithOneRegime object                                          #
#----------------------------------------------------------------------#
.BOWLWithOneRegime <- function(regime, 
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

  #------------------------------------------------------------------#
  # Number of records in dataset.                                    #
  #------------------------------------------------------------------#
  ns <- nrow(data)

  #------------------------------------------------------------------#
  # Perform weighted learning                                        #
  #------------------------------------------------------------------#
  result <- .newBOWLBasic(regime = regime, 
                          txInfo = txInfo, 
                          ind = ind, 
                          prWgt = prWgt, 
                          response = response, 
                          txVec = txVec,
                          data = data,
                          kernel = kernel,
                          kparam = kparam,
                          lambdas = lambdas,
                          cvFolds = cvFolds,
                          suppress = suppress)

  result <- new("BOWLWithOneRegime",
                result)

  return( result )

}

setMethod(f = ".newBOWLOptimization",    
          signature = c(regime   = "formula"), 
          definition = .BOWLWithOneRegime )


setMethod(f = "cvInfo",   
          signature = c(object = "BOWLWithOneRegime"), 
          definition = function(object) {
                         return( cvInfo(as(object,"BOWLBasic")) )
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
          signature = c(object="BOWLWithOneRegime"),
          definition = function(object, ...){ 
                         return( optimObj(as(object,"BOWLBasic")) )
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
          signature = c(x = "BOWLWithOneRegime", 
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
                         return( .predictOptimalTx(x = as(x,"BOWLBasic")) )
                       } )


setMethod(f = ".predictOptimalTx",
          signature = c(x = "BOWLWithOneRegime", 
                        newdata = "data.frame"),
          definition = function(x, newdata, ...) {
                         return( .predictOptimalTx(x = as(x,"BOWLBasic"), newdata = newdata) )
                       })

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class BOWLBasic                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "BOWLWithOneRegime"), 
          definition = function(x, ...){
                         print(as(x,"BOWLBasic"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLWithSubsetRegimes                    #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "BOWLWithOneRegime"), 
          definition = function(object, ...){
                         return( regimeCoef(as(object,"BOWLBasic")) )  
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLBasic                                #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "BOWLWithOneRegime"), 
          definition = function(object){
                         show(as(object,"BOWLBasic"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary                                                 #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class BOWLBasic                                #
#   returns                                                            #
# a list                                                               #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "BOWLWithOneRegime"), 
          definition = function(object, ...){
                         return( summary(as(object,"BOWLBasic")) )
                       } )

