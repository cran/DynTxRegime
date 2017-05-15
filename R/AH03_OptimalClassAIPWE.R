# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                     Class OptimalClassAIPWE                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of a call to optimalClass with outcome modeling              #
#----------------------------------------------------------------------#
# classif : an object of class modelObjFit                             #
# extends OptimalClass                                                 #
#----------------------------------------------------------------------#
.checkValidity_OptimalClassAIPWE <- function(object) {

  errors <- character()

  if( !is(object@outcome, "SingleDecisionPoint") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@outcome, "SubsetsNotModeled") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@propen, "SingleDecisionPoint") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( !is(object@propen, "SubsetsNotModeled") ) {
    msg <- "propen is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "OptimalClassAIPWE",
         slots = c(classif = "modelObjFit"),
         contains = c("OptimalClass", "PropensityAndOutcome"),
         validity = .checkValidity_OptimalClassAIPWE)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    OptimalClassAIPWE METHODS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the results of the classification step                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassAIPWE                         #
#   returns                                                            #
# an object as defined by solver.method                                #
#----------------------------------------------------------------------#
setMethod(f = "classif",
          signature = c(object="OptimalClassAIPWE"),
          definition = function(object, ...){ 
                         return( fitObject(object@classif) )  
                       } )

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassAIPWE                        #
#   returns                                                            #
# String indicating optimalClass AIPWE estimator                       #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "OptimalClassAIPWE"), 
          definition = function(object){
                         return( "Classification Perspective AIPWE" )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regression results                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassAIPWE                        #
#   returns                                                            #
# a list, the elements of which are defined by the solver.methods      #
#----------------------------------------------------------------------#
setMethod(f = "fitObject",    
          signature = c(object = "OptimalClassAIPWE"), 
          definition = function(object, ...){
                         result <- fitObject(object = as(object,"PropensityAndOutcome"))
                         result[[ "classif" ]] <- classif(object)
                         return(result)
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalClassAIPWE                             #
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "OptimalClassAIPWE", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){
                         opt <- predict(x@classif, newdata = newdata)
                         return( drop(opt) )
                       } )

#----------------------------------------------------------------------#
# Plot the results of the classification and propensity                # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalClassAIPWE                             #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "plot",
          signature = c(x = "OptimalClassAIPWE"),
          definition = function(x, suppress=FALSE, ...){

                         argList <- list(...)
                         if( !suppress ) {
                           argList <- .titleIt(argList, "Classification")
                         }
                         argList[[ "x" ]] <- x@classif
                         do.call(what = plot, args = argList)

                         plot(x = as(x,"PropensityAndOutcome"), suppress = suppress, ...)

                       } )
#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalClassAIPWE                             #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimalClassAIPWE"), 
          definition = function(x, ...){
                         cat("\nClassification\n")
                         print(classif(x),...)
                         print(x = as(x,"PropensityAndOutcome"))
                         cat("\nEstimated Value: ", estimator(x), "\n")
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassAIPWE                        #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimalClassAIPWE"), 
          definition = function(object){
                         cat("\nClassification\n")
                         show(classif(object))
                         show(object = as(object,"PropensityAndOutcome"))
                         cat("\nValue: ", estimator(object), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the classification and propensity     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassAIPWE                        #
#   returns                                                            #
# a list, the elements of which are defined by solver.methods          #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OptimalClassAIPWE"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityAndOutcome"))
                         result[[ "classif" ]] <- summary(classif(object), ...)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )


