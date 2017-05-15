setClass(Class = "SingleDecisionPoint", contains = c("VIRTUAL"))

setClass(Class = "MultipleDecisionPoint", contains = c("VIRTUAL"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         SubsetList CLASS                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# VIRTUAL class to store for subsets                                   #
#----------------------------------------------------------------------#
# extends List & SingleDecisionPoint directly
#
# slots:
#
#  loo : an object of class "list" whose elements can be of any class
#
#----------------------------------------------------------------------#
.checkValidity_SubsetList <- function(object){

  errors <- character()

  #------------------------------------------------------------------#
  # Cannot create an empty SubsetList object.                        #
  #------------------------------------------------------------------#
  if( length(object) <= 0L ) {
    msg <- "a SubsetList cannot be empty"
    errors <- c(errors, msg)
  }

  #------------------------------------------------------------------#
  # SubsetList must have named elements. The names are expected to be#
  # the nicknames specified by user. If multiple subsets are         #
  # associated, they are collapsed into a single comma separated     #
  # string for the purposes of naming.                               #
  #------------------------------------------------------------------#
  namesLoo <- names(object)
  if( is.null(namesLoo) ) {
    msg <- "name for subset not provided to element of SubsetList"
    errors <- c(errors,msg)
  }

  for( i in namesLoo ) {
    if( is.null(i) || {i == ""} ) {
      msg <- "name for subset not provided to element of SubsetList"
      errors <- c(errors,msg)
      break
    }
  }

  #------------------------------------------------------------------#
  # Ensure no duplicate names                                        #
  #------------------------------------------------------------------#
  uniqueNames <- unique(namesLoo)
  if( length(uniqueNames) != length(namesLoo) ) {
    msg <- "at least two elements of SubsetList have the same name"
    errors <- c(errors,msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "SubsetList",
         contains = c("List", "SingleDecisionPoint"),
         validity = .checkValidity_SubsetList)

setGeneric(name = "print")

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        SubsetList METHODS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Applies the specified function to each element of a SubsetList.      #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : An object of class SubsetList                             #
#   func   : A character object. The name of the function to be        #
#            applied to each element of the SubsetList object.         #
#   trm    : A character object. The formal argument name through      #
#            which each element of the SubsetList is passed to func.   #
#   ...    : Additional arguments to be passed to func.                #
# returns                                                              #
#   a list object. Each element is named "Subset=x" where x is the     #
#   subset nickname. The object stored in each element is the value    #
#   object returned by the call to func.                               #
#----------------------------------------------------------------------#
.CycleThruSS <- function(object, func, trm = "object", ...){

  n <- length(object)

  if( n < 1L ) return()

  #------------------------------------------------------------------#
  # Retrieve any additional arguments to be passed to method.        #
  #------------------------------------------------------------------#
  argList <- list(...)

  #------------------------------------------------------------------#
  # Storage list of results                                          #
  #------------------------------------------------------------------#
  res <- list()

  #------------------------------------------------------------------#
  # For each element of the SubsetList, call the method.             #
  #------------------------------------------------------------------#
  for( i in 1L:n ) {

    #--------------------------------------------------------------#
    # Add the object stored in the ith element of SubsetList       #
    # object to the method argument list for function call         #
    #--------------------------------------------------------------#
    argList[[ trm ]] <- object[[i]]

    #--------------------------------------------------------------#
    # Name the element of the result list using subset nickname    #
    #--------------------------------------------------------------#
    nms <- paste("Subset=", names(object)[i], sep="")

    #--------------------------------------------------------------#
    # Call the function.                                           #
    #--------------------------------------------------------------#
    res[[ nms ]] <- do.call(what = func, args = argList)

  }

  return( res )

}

#----------------------------------------------------------------------#
# Retrieve the coefficient estimates for each subset model             # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class SubsetList                             #
#   ...    : additional arguments to be passed to coef()               #
# returns                                                              #
#   a list object. Each element is named "Subset=x" where x is the     #
#   subset nickname. The object stored in each element is the value    #
#   object returned by coef.                                           #
#----------------------------------------------------------------------#
setMethod(f = "coef", 
          signature = c(object = "SubsetList"), 
          definition = function(object, ...){
                         res <- .CycleThruSS(object = object, 
                                             func = "coef", 
                                             trm = "object", ...)
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regression results for each subset model                # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class SubsetList                             #
#   ...    : additional arguments to be passed to coef()               #
# returns                                                              #
#   a list object. Each element is named "Subset=x" where x is the     #
#   subset nickname. The object stored in each element is the value    #
#   object returned by regression method.                              #
#----------------------------------------------------------------------#
setMethod(f = "fitObject", 
          signature = c(object = "SubsetList"), 
          definition = function(object, ...){
                         res <- .CycleThruSS(object = object, 
                                             func = "fitObject", 
                                             trm = "object", ...)
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Plot the results of each regression analysis                         # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   x : an object of class SubsetList                                  #
#   ...    : additional arguments to be passed to plot()               #
# returns                                                              #
#   Calls plot method defined for regression object.                   #
#----------------------------------------------------------------------#
setMethod(f = "plot",
          signature = c(x = "SubsetList"),
          definition = function(x, suppress=FALSE, ...){

                         for( i in 1L:length(x) ) {

                           #-----------------------------------------#
                           # Retrieve additional arguments that may  #
                           # have been passed by user.               #
                           #-----------------------------------------#
                           argList <- list(...)

                           if( !suppress ) {
                             #---------------------------------------#
                             # If user requested that the plots be   #
                             # augmented by details of the SS, add   #
                             # the subset name to a title argument   #
                             #---------------------------------------#
                             nms <- paste("Subset=", 
                                          names(x)[i], sep="")

                             argList <- .titleIt(argList, nms)
                           }

                           #-----------------------------------------#
                           # Store the SubsetList element as formal  #
                           # arg for next plot call                  #
                           #-----------------------------------------#
                           argList[[ "x" ]] <- x[[i]]

                           #-----------------------------------------#
                           # Call next plot method                   #
                           #-----------------------------------------#
                           do.call(what = plot, 
                                   args = argList)
                         }
                       } )

#----------------------------------------------------------------------#
# Print the key results of each regression analysis                    # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   x : an object of class SubsetList                                  #
#----------------------------------------------------------------------#
setMethod(f = "print", 
          signature = c(x = "SubsetList"), 
          definition = function(x, ...){
                        for( i in 1L:length(x) ) {
                           cat("Subset:", names(x)[i], "\n")
                           print(x = x[[i]], ...)
                        }
                       } )

#----------------------------------------------------------------------#
# Show the key results of regression steps                             # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class SubsetList                             #
#----------------------------------------------------------------------#
setMethod(f = "show", 
          signature = c(object = "SubsetList"), 
          definition = function(object){
                         for( i in 1L:length(object) ) {
                           cat("Subset:", names(object)[i], "\n")
                           show(object = object[[i]])
                         }
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of regression analyses                   # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class SubsetList                             #
#   ...    : additional arguments to be passed to summary()            #
# returns                                                              #
#   a list object. Each element is named "Subset=x" where x is the     #
#   subset nickname. The object stored in each element is the value    #
#   object returned by smmary().                                       #
#----------------------------------------------------------------------#
setMethod(f = "summary", 
          signature = c(object = "SubsetList"), 
          definition = function(object, ...){
                         res <- .CycleThruSS(object = object, 
                                             func = "summary", 
                                             trm = "object", ...)
                         return( res )
                       } )
