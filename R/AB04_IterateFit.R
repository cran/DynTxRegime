# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         IterateFit CLASS                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of an outcome regression obtained iteratively.               #
#----------------------------------------------------------------------#
# Extends SingleDecisionPoint, OutcomeRegression, and 
# SubsetsNotModeled directly
#
#   fitObjME  : An object of class modelObjFit for main effects model
#   fitObjC   : An object of class modelObjFit for contrast model
#----------------------------------------------------------------------#
setClass(Class = "IterateFit", 
         slots = c(fitObjME  = "TypedSimpleFit",
                   fitObjC   = "TypedSimpleFit"),
         contains = c("SubsetsNotModeled",
                      "SingleDecisionPoint",
                      "OutcomeRegression"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        IterateFit METHODS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the coefficient estimates                                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IterateFit                               #
#   returns                                                            #
# a list; elements are defined in package modelObj.                    #
#----------------------------------------------------------------------#
setMethod(f = "coef",
          signature = c(object = "IterateFit"),
          definition = function(object, ...) {
                         res <- coef(object@fitObjME, ...)
                         res <- c(res, coef(object@fitObjC, ...))
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regression results                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IterateFit                               #
#   returns                                                            #
# a list; elements are defined in package modelObj.                    #
#----------------------------------------------------------------------#
setMethod(f = "fitObject", 
          signature = c(object = "IterateFit"), 
          definition = function(object, ...){
                         res <- fitObject(object@fitObjME, ...)
                         res <- c(res, fitObject(object@fitObjC, ...))
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Generate plots of the regression results                             # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IterateFit                                    #
#   returns                                                            #
# No object return.                                                    #
#----------------------------------------------------------------------#
setMethod(f = "plot", 
          signature = c(x = "IterateFit"), 
          definition = function(x, suppress=FALSE, ...){

                         plot(x@fitObjME, suppress = suppress, ...)
                         plot(x@fitObjC, suppress = suppress, ...)

                       } )

setMethod(f = "predict", 
          signature = c(object = "IterateFit"), 
          definition = function(object, ...){
                         fittedME <- predict(object = object@fitObjME, ...)
                         fittedC <- predict(object = object@fitObjC, ...)
                         return( drop(fittedME + fittedC) )
                       } )

#----------------------------------------------------------------------#
# Predict the outcome regression for all treatment options             #
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class IterateFit.                              #
# data : data.frame of covariates and treatment history                #
#   returns                                                            #
# A matrix. The estimated outcome.                                     #
#----------------------------------------------------------------------#
setMethod(f = ".predictAllTreatments", 
          signature = c(object = "IterateFit"), 
          definition = function(object, data, response){ 

                         pME <- .predictAllTreatments(object = object@fitObjME, 
                                                      data = data, response = response)
                         pC <- .predictAllTreatments(object = object@fitObjC, 
                                                     data = data, response = response)
                         return(list(vals = pME$vals + pC$vals, ss = pME$ss))
                       } )

#----------------------------------------------------------------------#
# Print key regression results                                         # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IterateFit                           #
#----------------------------------------------------------------------#
setMethod(f = "print", 
          signature = c(x = "IterateFit"), 
          definition = function(x, ...){
                         print(x@fitObjME)
                         print(x@fitObjC)
                       } )

#----------------------------------------------------------------------#
# Show key regression results                                          # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IterateFit                      #
#----------------------------------------------------------------------#
setMethod(f = "show", 
          signature = c(object = "IterateFit"), 
          definition = function(object){
                         show(object@fitObjME)
                         show(object@fitObjC)
                       } )

#----------------------------------------------------------------------#
# Retrieve summary information of regression results                   # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IterateFit                      #
#   returns                                                            #
# a list; elements are defined in the inherited class modelObjFit.     #
#----------------------------------------------------------------------#
setMethod(f = "summary",
          signature = c(object = "IterateFit"),
          definition = function(object, ...) {
                         res <- summary(object@fitObjME, ...)
                         res <- c(res, summary(object@fitObjC, ...))
                         return( res )
                       } )
