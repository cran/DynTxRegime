# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    Class OptimalSeqAIPWE_SDP                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of a call to optimalSeq for a single decision pt AIPWE       #
#----------------------------------------------------------------------#
# outcome : outcome regression object                                  #
# extends OptimalSeqIPWE_SDP                                           #
#----------------------------------------------------------------------#
.checkValidity_OptimalSeqAIPWE_SDP <- function(object) {

  errors <- character()

  if( !is(object@propen, "SingleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@outcome, "SingleDecisionPoint") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}
setClass("OptimalSeqAIPWE_SDP",
         slots = c(genetic = "list",
                   regime  = "Regime"),
         contains = c("OptimalSeq", "PropensityAndOutcome"),
         validity = .checkValidity_OptimalSeqAIPWE_SDP)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                   OptimalSeqAIPWE_SDP METHODS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_SDP                      #
#   returns                                                            #
# String indicating optimalSeq AIPWE estimator                         #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "OptimalSeqAIPWE_SDP"), 
          definition = function(object){ 
                         return("Missing Data Perspective AIPWE") 
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the genetic algorithm                        # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_SDP                      #
#   returns                                                            #
# a list object as defined by rgenoud                                  #
#----------------------------------------------------------------------#
setMethod(f = "genetic",    
          signature = c(object = "OptimalSeqAIPWE_SDP"), 
          definition = function(object, ...){  
                         return( object@genetic ) 
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqAIPWE_SDP                           #
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqAIPWE_SDP", 
                        newdata = "data.frame"),
          definition = function (x, newdata, ...){
                         return( .predictOptimalTx(x@regime, newdata) )
                       } )

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqAIPWE_SDP                           #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimalSeqAIPWE_SDP"), 
          definition = function(x, ...){
                         cat("\nGenetic\n")
                         print( genetic(x) )
                         cat("\nRegime Parameters:\n")
                         print( .getPars(x@regime) )
                         print(x=as(x,"PropensityAndOutcome"))
                         cat("Value: ", estimator(x), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_SDP                      #
#   returns                                                            #
# NULL, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "OptimalSeqAIPWE_SDP"), 
          definition = function(object, ...){
                         return( .getPars(object@regime) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_SDP                      #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimalSeqAIPWE_SDP"), 
          definition = function(object){
                         cat("\nGenetic\n")
                         show(genetic(object))
                         cat("\nRegime Parameters:\n")
                         show(.getPars(object@regime))
                         show(object = as(object,"PropensityAndOutcome"))
                         cat("Estimated Value: ", estimator(object), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the genetic algorithm, propensity, and#
# outcome analyses                                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqAIPWE_SDP                      #
#   returns                                                            #
# a list, the elements of which are defined by solver.methods          #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OptimalSeqAIPWE_SDP"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityAndOutcome"))
                         result[[ "genetic" ]] <- object@genetic
                         result[[ "regime" ]] <- .getPars(object@regime)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )



