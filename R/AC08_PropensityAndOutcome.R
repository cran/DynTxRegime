setClass(Class = "PropensityAndOutcome",
         slots = c(propen = "PropensityRegression",
                   outcome = "OutcomeRegression"))

#----------------------------------------------------------------------#
# Retrieve the coefficient estimates for propensity step               # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityAndOutcome                           #
#   returns                                                            #
# a list with a single element, 'propensity', holding the parameter    #
# estimates for the propensity regression.                             #
#----------------------------------------------------------------------#
setMethod(f = "coef",
          signature = c(object = "PropensityAndOutcome"),
          definition = function(object,...){
                         res <- list()
                         res[[ "propensity" ]] <- coef(object@propen)
                         res[[ "outcome" ]] <- coef(object@outcome)
                         return(res)
                       } )

#----------------------------------------------------------------------#
# Retrieve the fit object returned by the propensity solver.method     #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityAndOutcome                           #
#   returns                                                            #
# a list with a single element, 'propensity', holding the fit object   #
# returned by the propensity regression method.                        #
#----------------------------------------------------------------------#
setMethod(f = "fitObject",    
          signature = c(object = "PropensityAndOutcome"), 
          definition = function(object, ...){
                         result <- list()
                         result[[ "propensity" ]] <- fitObject(object@propen, ...)
                         result[[ "outcome" ]] <- fitObject(object@outcome, ...)
                         return(result)
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the outcome regression step                  # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityAndOutcome                     #
#   returns                                                            #
# the fit object returned by the solver.method used to obtain fit      #
#----------------------------------------------------------------------#
setMethod(f = "outcome",    
          signature = c(object = "PropensityAndOutcome"), 
          definition = function(object,...){ 
                         return( fitObject(object@outcome) ) 
                       } )

#----------------------------------------------------------------------#
# Plot the results of the propensity analysis                          # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class PropensityAndOutcome                                #
# suppress : T/F indicating if plot titles should be augmented by nms  #
#----------------------------------------------------------------------#
setMethod(f = "plot",
          signature = c(x = "PropensityAndOutcome"),
          definition = function(x, suppress=FALSE, ...){

                         argList <- list(...)

                         if( !suppress ) {
                           argList <- .titleIt(argList, "Propensity")
                         }

                         argList[[ "x" ]] <- x@propen
                         argList[[ "suppress" ]] <- suppress

                         do.call(what = plot, args = argList)

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
# x : an object of class PropensityAndOutcome                          #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "PropensityAndOutcome"), 
          definition = function(x, ...){
                         cat("\nPropensity\n")
                         print(x@propen, ...)
                         cat("\nOutcome\n")
                         print(x@outcome, ...)
                         return()
                       } )

#----------------------------------------------------------------------#
# Retrieve the results of the propensity regression step               # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityAndOutcome                     #
#   returns                                                            #
# the fit object returned by the solver.method used to obtain fit      #
#----------------------------------------------------------------------#
setMethod(f = "propen",    
          signature = c(object = "PropensityAndOutcome"), 
          definition = function(object,...){ 
                         return( fitObject(object@propen) ) 
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityAndOutcome                     #
#   returns                                                            #
# Nothing returned                                                     #
# Calls show methods of PropensityAndOutcomeOptim and PropensityFit    #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "PropensityAndOutcome"), 
          definition = function(object){
                         cat("\nPropensity\n")
                         show(object@propen)
                         cat("\nOutcome\n")
                         show(object@outcome)
                         return()
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of propensity regression, optimization,  #
# and estimated value                                                  #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityAndOutcome                           #
#   returns                                                            #
# a list 'propensity', holding the summary of the fit object           #
# returned by the propensity regression method.                        #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "PropensityAndOutcome"), 
          definition = function(object, ...){
                         result <- list()
                         result[[ "propensity" ]] <- summary(object@propen)
                         result[[ "outcome" ]] <- summary(object@outcome)
                         return(result)
                       } )
