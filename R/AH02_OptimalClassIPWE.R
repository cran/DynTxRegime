# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      Class OptimalClassIPWE                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of a call to optimalClass with IPWE                          #
#----------------------------------------------------------------------#
# classif : modelObjFit object for classification                      #
# propen  : PropensityFit object                                       #
#----------------------------------------------------------------------#
.checkValidity_OptimalClassIPWE <- function(object) {

  errors <- character()

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

setClass(Class = "OptimalClassIPWE",
         slots = c(classif = "modelObjFit"),
         contains = c("OptimalClass", "PropensityOnly"),
         validity = .checkValidity_OptimalClassIPWE)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                     OptimalClassIPWE METHODS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the results of the classification step                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassIPWE                         #
#   returns                                                            #
# an object as defined by solver.method                                #
#----------------------------------------------------------------------#
setMethod(f = "classif",
          signature = c(object="OptimalClassIPWE"),
          definition = function(object, ...){ 
                         return( fitObject(object@classif) )  
                       } )

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassIPWE                         #
#   returns                                                            #
# String indicating optimalClass IPWE estimator                        #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "OptimalClassIPWE"), 
          definition = function(object){
                         return( "Classification Perspective IPWE" )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regression and classification results                   #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassIPWE                         #
#   returns                                                            #
# a list, the elements of which are defined by the solver.methods      #
#----------------------------------------------------------------------#
setMethod(f = "fitObject",    
          signature = c(object = "OptimalClassIPWE"), 
          definition = function(object, ...){
                         result <- fitObject(object = as(object,"PropensityOnly"))
                         result[[ "classif" ]] <- classif(object)
                         return(result)
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalClassIPWE                              #
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "OptimalClassIPWE", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){
                         opt <- predict(x@classif, newdata = newdata)
                         return( drop(opt) )
                       } )

#----------------------------------------------------------------------#
# Plot the results of the classification and propensity                # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalClassIPWE                              #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "plot",
          signature = c(x = "OptimalClassIPWE"),
          definition = function(x, suppress=FALSE, ...){

                         argList <- list(...)
                         if( !suppress ) {
                           argList <- .titleIt(argList, "Classification")
                         }

                         argList[[ "x" ]] <- x@classif
                         do.call(what = plot, args = argList)

                         plot(x = as(x,"PropensityOnly"), suppress = suppress, ...)

                       } )

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimalClassIPWE                              #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimalClassIPWE"), 
          definition = function(x, ...){
                         cat("\nClassification\n")
                         print(classif(x),...)
                         print(x = as(x,"PropensityOnly"))
                         cat("\nEstimated Value: ", estimator(x), "\n")
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassIPWE                         #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimalClassIPWE"), 
          definition = function(object){
                         cat("\nClassification\n")
                         show(classif(object))
                         show(object = as(object,"PropensityOnly"))
                         cat("\nValue: ", estimator(object), "\n")
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the classification and propensity     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimalClassIPWE                         #
#   returns                                                            #
# a list, the elements of which are defined by solver.methods          #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OptimalClassIPWE"), 
          definition = function(object, ...){
                         result <- summary(object = as(object,"PropensityOnly"))
                         result[[ "classif" ]] <- summary(classif(object), ...)
                         result[[ "value" ]] <- estimator(object)
                         return(result)
                       } )


