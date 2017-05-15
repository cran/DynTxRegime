# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                  CLASS Regime_DecisionPointList                  + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
.checkValidity_Regime_DecisionPointList <- function(object){

  errors <- character()

  for( i in 1L:length(object@loo) ) {

    if( !is(object[[i]], "Regime") ) {
      msg <- paste("You are attempting to create a",
                   "Regime_DecisionPointList with",
                   "an object that is not Regime",
                   class(object[[i]]))
      errors <- c(errors, msg)
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "Regime_DecisionPointList",
         contains = c("RegimeObject",
                      "DecisionPointList"),
         validity = .checkValidity_Regime_DecisionPointList)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                 Regime_DecisionPointList METHODS                 + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the number of parameters to be estimated in regime functions#
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime_DecisionPointList                 #
#   returns                                                            #
# an integer                                                           #
#----------------------------------------------------------------------#
setMethod(f = ".getNumPars",   
          signature = c(object="Regime_DecisionPointList"), 
          definition = function(object){ 
                         tot <- 0L
                         for( i in 1L:length(object@loo) ) {
                           tot <- tot + .getNumPars(object@loo[[i]])
                         }
                         return( tot ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the names of the parameters estimated in regime function    #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime_DecisionPointList                 #
#   returns                                                            #
# a list                                                               #
#----------------------------------------------------------------------#
setMethod(f = ".getParNames",  
          signature = c(object = "Regime_DecisionPointList"), 
          definition = function(object){ 
                         tot <- list()
                         for( i in 1L:length(object@loo) ) {
                           tot[[i]] <- .getParNames(object@loo[[i]])
                         }
                         return( tot ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameters values                                       #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime_DecisionPointList                 #
#   returns                                                            #
# a list, each element a named numeric vector                          #
#----------------------------------------------------------------------#
setMethod(f = ".getPars",  
          signature = c(object = "Regime_DecisionPointList"), 
          definition = function(object){ 
                         tot <- list()
                         for( i in 1L:length(object@loo) ) {
                           tot[[i]] <- .getPars(object@loo[[i]])
                         }
                         return( tot ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the user specified function that defines the regime         #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime                                   #
#   returns                                                            #
# a function                                                           #
#----------------------------------------------------------------------#
setMethod(f = ".getRegimeFunction",   
          signature = c(object = "Regime_DecisionPointList"), 
          definition = function(object){ 
                         tot <- list()
                         for( i in 1L:length(object@loo) ) {
                           tot[[i]] <- .getRegimeFunction(object@loo[[i]])
                         }
                         return( tot ) 
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class Regime                                        #
#   returns                                                            #
# A vector                                                             #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",
          signature = c(x = "Regime_DecisionPointList", 
                        newdata = "data.frame"),
          definition = function (x, newdata, dp=1L, ...){
                         if( dp > length(x) ) stop("invalid dp")
                         tot <- .predictOptimalTx(x@loo[[dp]], 
                                                  newdata)
                         return( tot ) 
                       } )


#----------------------------------------------------------------------#
# Set the parameters values                                            #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime                                   #
# pars   : numeric vector                                              #
#   returns                                                            #
# the updated object of class Regime                                   #
#----------------------------------------------------------------------#
setMethod(f = ".setPars",  
          signature = c(object = "Regime_DecisionPointList",
                        pars = "numeric"), 
          definition = function(object, pars){ 
                         ni <- 1L
                         for( i in 1L:length(object@loo) ) {
                           nf <- ni + .getNumPars(object@loo[[i]]) - 1L
                           object@loo[[i]] <- .setPars(object@loo[[i]],
                                                       pars[ni:nf])
                           ni <- nf + 1L
                         }
                         return( object ) 
                       } )


#----------------------------------------------------------------------#
# Retrieve the parameter estimates                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime_DecisionPointList                 #
#   returns                                                            #
# a list, each element a named numeric vector                          #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "Regime_DecisionPointList"), 
          definition = function(object, ...){
                         return( .getPars(object) )
                       } )

