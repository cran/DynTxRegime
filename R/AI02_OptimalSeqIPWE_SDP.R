# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                     Class OptimalSeqIPWE_SDP                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# Results of a call to optimalSeq for a single decision pt IPWE        #
#----------------------------------------------------------------------#
# genetic : results from a call to rgenoud (genetic algorithm)         #
# propen  : PropensityFit object                                       #
# regime  : Regime object                                              #
#----------------------------------------------------------------------#
.checkValidity_OptimalSeqIPWE_SDP <- function(object) {

  errors <- character()

  if( !is(object@propen, "SingleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}
setClass("OptimalSeqIPWE_SDP",
         slots = c(genetic = "list",
                   regime = "Regime"),
         contains = c("OptimalSeq", "PropensityOnly"),
         validity = .checkValidity_OptimalSeqIPWE_SDP)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    OptimalSeqIPWE_SDP METHODS                    + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqIPWE_SDP                       #
#   returns                                                            #
# String indicating optimalSeq IPWE estimator                          #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "OptimalSeqIPWE_SDP"), 
          definition = function(object){ 
                         return( "Missing Data Perspective IPWE" )
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the genetic algorithm                        # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqIPWE_SDP                       #
#   returns                                                            #
# a list object as defined by rgenoud                                  #
#----------------------------------------------------------------------#
setMethod(f = "genetic",    
          signature = c(object = "OptimalSeqIPWE_SDP"), 
          definition = function(object, ...){  
                         return( object@genetic ) 
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqIPWE_SDP                            #
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "OptimalSeqIPWE_SDP", 
                        newdata = "data.frame"),
          definition = function (x, newdata, ...){
                         return( .predictOptimalTx(x@regime, newdata) )
                       } )

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalSeqIPWE_SDP                            #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimalSeqIPWE_SDP"), 
          definition = function(x, ...){
                         cat("\nGenetic\n")
                         print( genetic(x) )
                         cat("\nRegime Parameters:\n")
                         print( .getPars(x@regime) )
                         print(x = as(x,"PropensityOnly"))
                         cat("Value: ", estimator(x), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqIPWE_SDP                       #
#   returns                                                            #
# NULL, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "OptimalSeqIPWE_SDP"), 
          definition = function(object, ...){
                         return( .getPars(object@regime) )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqIPWE_SDP                       #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimalSeqIPWE_SDP"), 
          definition = function(object){
                         cat("\nGenetic\n")
                         show(genetic(object))
                         cat("\nRegime Parameters:\n")
                         show(.getPars(object@regime))
                         show(object = as(object,"PropensityOnly"))
                         cat("Estimated Value: ", estimator(object), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the genetic algorithm and propensity  #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalSeqIPWE_SDP                       #
#   returns                                                            #
# a list, the elements of which are defined by solver.methods          #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OptimalSeqIPWE_SDP"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityOnly"))
                         result[[ "genetic" ]] <- object@genetic
                         result[[ "regime" ]] <- .getPars(object@regime)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )


