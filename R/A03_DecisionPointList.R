# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      DecisionPointList CLASS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# VIRTUAL class used to store results for multiple decision points     #
#----------------------------------------------------------------------#
# extends List & MultipleDecisionPoint directly
#
# slots:
#
#  loo : an object of class "list" whose elements can be of any class
#
#----------------------------------------------------------------------#
.checkValidity_DecisionPointList <- function(object){

  errors <- character()

  #------------------------------------------------------------------#
  # Cannot create an empty DecitionPointList object.                 #
  #------------------------------------------------------------------#
  if( length(object) <= 0L ) {
    msg <- "cannot create a DecisionPointList of length 0"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "DecisionPointList",
         contains = c("MultipleDecisionPoint", "List"),
         validity = .checkValidity_DecisionPointList)


# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    DecisionPointList METHODS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Applies a specified method to each element of a DecisionPointList.   #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : An object of class DecisionPointList                      #
#   func   : A character object. The name of the function to be        #
#            called for each element of object.                        #
#   trm    : A character object. The formal argument name through      #
#            which each element of object is passed to func.           #
#   ...    : Additional arguments to be passed to func.                #
# returns                                                              #
#   a list object. Each element is named "dp=x" where x=1:ndp.         #
#   The object stored in each element is the value object returned     #
#   by the call to func.                                               #
#----------------------------------------------------------------------#
.CycleThruDP <- function(object, func, trm = "object", ...){

  #------------------------------------------------------------------#
  # Retrieve any additional arguments to be passed to func.          #
  #------------------------------------------------------------------#
  argList <- list(...)

  #------------------------------------------------------------------#
  # Storage list of results                                          #
  #------------------------------------------------------------------#
  res <- list()

  #------------------------------------------------------------------#
  # For each element of object, call func w/ argList.                #
  #------------------------------------------------------------------#
  for( i in 1L:length(object) ) {

    #--------------------------------------------------------------#
    # Add the object stored in the ith element to the argument list#
    #--------------------------------------------------------------#
    argList[[ trm ]] <- object[[i]]

    #--------------------------------------------------------------#
    # Name for element of res                                      #
    #--------------------------------------------------------------#
    nms <- paste("dp=", i, sep="")

    #--------------------------------------------------------------#
    # Call the function.                                           #
    #--------------------------------------------------------------#
    res[[ nms ]] <- do.call(what = func, args = argList)

  }

  return( res )

}

#----------------------------------------------------------------------#
# Retrieve the coefficient estimates for each decision point model     #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class DecisionPointList                      #
#   ...    : additional arguments to be passed to coef()               #
# returns                                                              #
#   a list object. Each element is named "dp=x" where x=1:ndp.         #
#   The object stored in each element is the value object returned     #
#   by the call to coef.                                               #
#----------------------------------------------------------------------#
setMethod(f = "coef", 
          signature = c(object = "DecisionPointList"), 
          definition = function(object, ...){
                         res <- .CycleThruDP(object = object, 
                                             func = "coef", 
                                             trm = "object", ...)
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regression results for each decision point              # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class DecisionPointList                      #
#   ...    : additional arguments to be passed to coef()               #
# returns                                                              #
#   a list object. Each element is named "dp=x" where x=1:ndp.         #
#   The object stored in each element is the value object returned     #
#   by the call to fitObject.                                          #
#----------------------------------------------------------------------#
setMethod(f = "fitObject", 
          signature = c(object = "DecisionPointList"), 
          definition = function(object, ...){
                         res <- .CycleThruDP(object = object, 
                                             func = "fitObject", 
                                             trm = "object", ...)
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Plot the results of each regression analysis                         # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   x : an object of class DecisionPointList                           #
#   ...    : additional arguments to be passed to plot()               #
# returns                                                              #
#   Calls plot method defined for regression object.                   #
#----------------------------------------------------------------------#
setMethod(f = "plot",
          signature = c(x = "DecisionPointList"),
          definition = function(x, suppress = FALSE, ...){

                         for( i in 1L:length(x) ) {

                           #-----------------------------------------#
                           # Retrieve additional arguments that may  #
                           # have been passed by user.               #
                           #-----------------------------------------#
                           argList <- list(...)

                           if( !suppress ) {
                             #---------------------------------------#
                             # If user requested that the plots be   #
                             # augmented by details of the dp, add   #
                             # the decision point to a title argument#
                             #---------------------------------------#
                             nms <- paste("dp=", i, sep="")
                             argList <- .titleIt(argList, nms)
                           }

                           #-----------------------------------------#
                           # Store the DecisionPointList element as  #
                           # formal arg for next plot call.          #
                           #-----------------------------------------#
                           argList[[ "x" ]] <- x[[i]]

                           #-----------------------------------------#
                           # Call next plot method.                  #
                           #-----------------------------------------#
                           do.call(what = plot, args = argList)
                         }
                       } )

#----------------------------------------------------------------------#
# Obtain predicted values for each decision point                      # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class DecisionPointList                      #
#   newdata: if given, data.frame of covariates and treatment history  #
#            with which predictions are to be made.                    #
# returns                                                              #
#   a list object. Each element is named "dp=x" where x=1:ndp.         #
#   The object stored in each element is the value object returned     #
#   by the call to predict().                                          #
#----------------------------------------------------------------------#
setMethod(f = "predict", 
          signature = c(object = "DecisionPointList"), 
          definition = function(object, newdata, ...){ 
                         if( missing(newdata) ) {
                           res <- .CycleThruDP(object = object, 
                                               func = "predict", 
                                               trm = "object", ...)
                         } else {
                           res <- .CycleThruDP(object = object, 
                                               func = "predict", 
                                               trm = "object", 
                                               newdata = newdata, ...)
                         }
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Print the key results of each regression analysis                    # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   x : an object of class DecisionPointList                           #
#----------------------------------------------------------------------#
setMethod(f = "print", 
          signature = c(x = "DecisionPointList"), 
          definition = function(x, ...){
                         for( i in 1L:length(x) ) {
                           cat("Decision Point:", i, "\n")
                           print(x = x[[i]], ...)
                         }
                       } )

#----------------------------------------------------------------------#
# Show the key results of regression steps                             # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class DecisionPointList                      #
#----------------------------------------------------------------------#
setMethod(f = "show", 
          signature = c(object = "DecisionPointList"), 
          definition = function(object){
                         for( i in 1L:length(object) ) {
                           cat("Decision Point:", i, "\n")
                           show(object = object[[i]])
                         }
                         return()
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of regression analyses                   # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class DecisionPointList                      #
#   ...    : additional arguments to be passed to summary()            #
# returns                                                              #
#   a list object. Each element is named "dp=x" where x=1:ndp.         #
#   The object stored in each element is the value object returned     #
#   by the call to summary().                                          #
#----------------------------------------------------------------------#
setMethod(f = "summary", 
          signature = c(object = "DecisionPointList"), 
          definition = function(object, ...){
                         res <- .CycleThruDP(object = object, 
                                             func = "summary", 
                                             trm = "object", ...)
                         return( res )
                       } )
