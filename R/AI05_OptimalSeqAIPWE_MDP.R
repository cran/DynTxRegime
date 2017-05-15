# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    Class OptimalSeqAIPWE_MDP                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# Results of a call to optimalSeq for multiple decision pts AIPWE      #
#----------------------------------------------------------------------#
# outcome  : OutcomeRegressionAllTypes_DecisionPointList object        #
# extends OptimalSeqIPWE_MDP                                           #
#----------------------------------------------------------------------#
.checkValidity_OptimalSeqAIPWE_MDP <- function(object) {

  errors <- character()

  if( !is(object@propen, "MultipleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@outcome, "MultipleDecisionPoint") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}
setClass("OptimalSeqAIPWE_MDP",
         slots = c(genetic = "list",
                   regime  = "Regime_DecisionPointList"),
         contains = c("OptimalSeq", "PropensityAndOutcome"),
         validity = .checkValidity_OptimalSeqAIPWE_MDP)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                   OptimalSeqAIPWE_MDP METHODS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_MDP                      #
#   returns                                                            #
# String indicating optimalSeq IPWE estimator                          #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "OptimalSeqAIPWE_MDP"), 
          definition = function(object){ 
                         return("Coarsened Data Perspective AIPWE") 
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the genetic algorithm                        # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_MDP                      #
#   returns                                                            #
# a list object as defined by rgenoud                                  #
#----------------------------------------------------------------------#
setMethod(f = "genetic",    
          signature = c(object = "OptimalSeqAIPWE_MDP"), 
          definition = function(object, ...){  
                         return( object@genetic ) 
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqAIPWE_MDP                           #
# newdata : data.frame of covariate and treatment history              #
# dp : integer indicating decision point for which optimal tx estimated#
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqAIPWE_MDP", 
                        newdata = "data.frame"),
          definition = function (x, newdata, ..., dp=1){

                         if( dp > ncol(x@optimalTx) ) {
                           stop("dp is inappropriate value")
                         }

                         return( .predictOptimalTx(x = x@regime, 
                                                   newdata = newdata, 
                                                   dp = dp) )

                       } )

#----------------------------------------------------------------------#
# Retrieve estimated optimal treatment for training data               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqAIPWE_MDP                           #
# dp : integer indicating which decision point to retrieve             #
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqAIPWE_MDP", 
                        newdata = "missing"),
          definition = function (x, newdata, ..., dp=1){

                         if( dp > ncol(x@optimalTx) ) {
                           stop("dp is inappropriate value")
                         }

                         return( x@optimalTx[,dp] )

                       } )

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqAIPWE_MDP                           #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimalSeqAIPWE_MDP"), 
          definition = function(x, ...){
                         cat("\nGenetic\n")
                         print( genetic(x) )
                         cat("\nRegime Parameters:\n")
                         print( .getPars(x@regime) )
                         print(x = as(x,"PropensityAndOutcome"))
                         cat("Value: ", estimator(x), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_MDP                      #
#   returns                                                            #
# NULL, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "OptimalSeqAIPWE_MDP"), 
          definition = function(object, ...){
                         return( .getPars(object@regime) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_MDP                      #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimalSeqAIPWE_MDP"), 
          definition = function(object){
                         cat("\nGenetic\n")
                         show(genetic(object))
                         cat("\nRegime Parameters:\n")
                         show(.getPars(object@regime))
                         show(object = as(object,"PropensityAndOutcome"))
                         cat("Estimated Value: ", estimator(object), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the genetic algorithm and propensity  #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_MDP                      #
#   returns                                                            #
# a list, the elements of which are defined by solver.methods          #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OptimalSeqAIPWE_MDP"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityAndOutcome"))
                         result[[ "genetic" ]] <- object@genetic
                         result[[ "regime" ]] <- .getPars(object@regime)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )


