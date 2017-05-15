setClass(Class = "OutcomeOnly",
         slots = c(outcome = "OutcomeRegression"))

setGeneric(name = "outcome", 
           def = function(object, ...){standardGeneric("outcome")})

setGeneric(name = "propen", 
           def = function(object, ...){standardGeneric("propen")})

#----------------------------------------------------------------------#
# Retrieve the coefficient estimates for outcome regression step       #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OutcomeOnly                              #
#   returns                                                            #
# a list with a single element, 'outcome', holding the parameter       #
# estimates for the outcome regression.                                #
#----------------------------------------------------------------------#
setMethod(f = "coef",
          signature = c(object = "OutcomeOnly"),
          definition = function(object,...){
                         res <- list()
                         res[[ "outcome" ]] <- coef(object@outcome)
                         return(res)
                       } )

#----------------------------------------------------------------------#
# Retrieve the fit object returned by the outcome solver.method        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OutcomeOnly                              #
#   returns                                                            #
# a list with a single element, 'outcome', holding the fit object      #
# returned by the outcome regression method.                           #
#----------------------------------------------------------------------#
setMethod(f = "fitObject",    
          signature = c(object = "OutcomeOnly"), 
          definition = function(object, ...){
                         result <- list()
                         result[[ "outcome" ]] <- fitObject(object@outcome, ...)
                         return(result)
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the outcome regression step                  # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OutcomeOnly                              #
#   returns                                                            #
# the fit object returned by the solver.method used to obtain fit      #
#----------------------------------------------------------------------#
setMethod(f = "outcome",    
          signature = c(object = "OutcomeOnly"), 
          definition = function(object,...){ 
                         return( fitObject(object@outcome) ) 
                       } )

#----------------------------------------------------------------------#
# Plot the results of the outcome analysis                             # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OutcomeOnly                                   #
# suppress : T/F indicating if plot titles should be augmented by nms  #
#----------------------------------------------------------------------#
setMethod(f = "plot",
          signature = c(x = "OutcomeOnly"),
          definition = function(x, suppress=FALSE, ...){

                         argList <- list(...)

                         if( !suppress ) {
                           argList <- .titleIt(argList, "Outcome")
                         }

                         argList[[ "x" ]] <- x@outcome
                         argList[[ "suppress" ]] <- suppress

                         do.call(what = plot, args = argList)

                       } )

#----------------------------------------------------------------------#
# Print key results.                                                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OutcomeOnly                                   #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OutcomeOnly"), 
          definition = function(x, ...){
                         cat("\nOutcome\n")
                         print(x@outcome, ...)
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the propensity regression step               # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OutcomeOnly                              #
#   returns                                                            #
# the fit object returned by the solver.method used to obtain fit      #
#----------------------------------------------------------------------#
setMethod(f = "propen",    
          signature = c(object = "OutcomeOnly"), 
          definition = function(object,...){ 
                         return( NA ) 
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OutcomeOnly                              #
#   returns                                                            #
# Nothing returned                                                     #
# Calls show methods of Outcome Regression                             #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OutcomeOnly"), 
          definition = function(object){
                         cat("\nOutcome\n")
                         show(object@outcome)
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of outcome regression, optimization,     #
# and estimated value                                                  #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OutcomeOnly                              #
#   returns                                                            #
# a list 'outcome', holding the summary of the fit object              #
# returned by the outcome regression method.                           #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OutcomeOnly"), 
          definition = function(object, ...){
                         result <- list()
                         result[[ "outcome" ]] <- summary(object@outcome)
                         return(result)
                       } )
