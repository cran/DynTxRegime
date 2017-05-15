# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       ModelObjSubset CLASS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# class for model objects specific to a subset of data                 #
#----------------------------------------------------------------------#
#
# slots:
#
#   decisionPoint : an object of class integer
#                   decision point for which modelObj is defined
#   subset        : an object of class character
#                   subset nickname for which modelObj is defined
#   modelObj      : an object of class modelObj
#                   model and methods for regression
#
#----------------------------------------------------------------------#
.checkValidity_ModelObjSubset <- function(object) {

  errors <- character()

  #------------------------------------------------------------------#
  # Decision point must be positive non-zero value.                  #
  #------------------------------------------------------------------#
  if( object@decisionPoint < 1L ) {
    msg <- "decision point must be a positive value"
    errors <- c(errors, msg)
  }

  #------------------------------------------------------------------#
  # Must provide a nickname for the subset.                          #
  #------------------------------------------------------------------#
  if( length(object@subset) == 0L ) {
    msg <- "subset must have a unique name"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "ModelObjSubset",
         slots = c(decisionPoint = "integer",
                   subset        = "character",
                   modelObj      = "modelObj"),
         validity = .checkValidity_ModelObjSubset)

setGeneric(name = ".getDecisionPoint", 
           def = function(object){standardGeneric(".getDecisionPoint")})

setGeneric(name = ".getSubset", 
           def = function(object){standardGeneric(".getSubset")})

setGeneric(name = ".getModelObj", 
           def = function(object){standardGeneric(".getModelObj")})

setMethod(f = ".getDecisionPoint",    
          signature = c(object = "ModelObjSubset"), 
          definition = function(object){ return( object@decisionPoint ) } )

setMethod(f = ".getSubset",    
          signature = c(object = "ModelObjSubset"), 
          definition = function(object){ return( object@subset ) } )

setMethod(f = ".getModelObj",    
          signature = c(object = "ModelObjSubset"), 
          definition = function(object){ return( object@modelObj ) } )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                        ModelObjSubset FUNCTION                       #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#----------------------------------------------------------------------#
# buildModelObjSubset extends functionality of modelObj                #
#----------------------------------------------------------------------#
# model          : A formula object. Object is symbolic model          #
#                  representation.                                     #
#                                                                      #
# dp             : decision point for which the model should be used   #
#                                                                      #
# subset         : character nickname for subset                       #
#                                                                      #
# solver.method  : A character giving the R function to be used to     #
#                  obtain parameter estimates. For example, `lm' `glm'.#
#                                                                      #
# solver.args    : Additional arguments to be sent to solver.method.   #
#                  This must be provided as a list, where the name of  #
#                  each element matches a formal argument of           #
#                  solver.method. For example,                         #
#                  if a logistic regression using glm is desired,      #
#                     solver.method = 'glm'                            #
#                     solver.args = list(family=binomial)              #
#                                                                      #
#                  It is assumed that solver.method takes formal       #
#                  arguments 'formula' and 'data' as input.            #
#                  Occasionally, R methods are developed that do not   #
#                  conform to this convention. A user can indicate if  #
#                  a different naming convention is used for these     #
#                  two input arguments. For example, if a method       #
#                  expects the formula object to be passed through     #
#                  input variable \code{x},                            #
#                    \code{solver.args} <- list("x"="formula")         #
#                                                                      #
# predict.method : A function name giving the R function to be used to #
#                  obtain predicted values. For example, `predict.lm'  #
#                  or `predict.glm'. If not explicitly given, the      #
#                  generic \code{predict} is assumed. Usually, this    #
#                  input does not need to be specified.                #
#                                                                      #
# predict.args   : Additional arguments to be sent to predict.method.  #
#                  This must be provided as a list, where the name of  #
#                  each element matches a formal argument of           #
#                  predict.method. For example, if a logistic          #
#                  regression using glm was used to fit the model      #
#                  formula object,                                     #
#                     solver.method = 'glm'                            #
#                     solver.args = list(family=binomial)              #
#                  then                                                #
#                     predict.method = 'predict.glm'                   #
#                     predict.args = list(type="response")             #
#                                                                      #
#                  It is assumed that predict.method takes formal      #
#                  arguments 'object' and 'newdata' as input.          #
#                  Occasionally, R methods are developed that do not   #
#                  conform to this convention. A user can indicate if  #
#                  a different naming convention is used for these     #
#                  two input arguments. For example, if a method       #
#                  expects the fit object to be passed through input   #
#                  variable \code{x},                                  #
#                    \code{predict.args} <- list("x"="object")         #
#                                                                      #
#----------------------------------------------------------------------#
buildModelObjSubset <-  function(...,
                                 model, 
                                 solver.method, 
                                 solver.args = NULL, 
                                 predict.method = NULL, 
                                 predict.args = NULL,
                                 dp = 1L, 
                                 subset = NA){

 if( !is(subset, "character") ) {
   UserError("input",
             "subset must be of class character")
 }

 dp <- as.integer(round(dp,0L))
 if( dp <= 0L ) {
   UserError("input",
             "dp must be a positive integer")
 }

 if( is(predict.method,"NULL") ) predict.method <- 'predict'

 myobjTemp <- buildModelObj(model = model, 
                            solver.method = solver.method, 
                            solver.args = solver.args,
                            predict.method = predict.method,
                            predict.args = predict.args)

 myobj <- new("ModelObjSubset", 
              "decisionPoint" = dp,
              "subset"        = subset,
              "modelObj"      = myobjTemp)

 return(myobj)

}

