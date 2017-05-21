setClass("RegimeObject",
         contains = c("VIRTUAL") )   
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                           CLASS Regime                           + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Information regarding the class of regimes being considered          #
#----------------------------------------------------------------------#
# contains all relevant information for a regime function              #
#   nVars  : the number of parameters to be estimated in the regime.   #
#   vNames : the names of the parameters to be estimated in the regime #
#   func   : the user specified function that defines the regime       #
#   pars   : the estimated parameters                                  #
#----------------------------------------------------------------------#
setClass("Regime",
         slots = c( nVars = "integer",
                   vNames = "character",
                     func = "function",
                     pars = "numeric"),
         contains = c("RegimeObject", "SingleDecisionPoint") )   

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         Regime GENERICS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".getNumPars", 
           def = function(object){standardGeneric(".getNumPars")})

setGeneric(name = ".getParNames", 
           def = function(object){standardGeneric(".getParNames")})

setGeneric(name = ".getPars", 
           def = function(object){standardGeneric(".getPars")})

setGeneric(name = ".getRegimeFunction", 
           def = function(object){standardGeneric(".getRegimeFunction")})

setGeneric(name = ".predictOptimalTx",
           def = function(x, newdata, ...){
                   standardGeneric(".predictOptimalTx")
                 })

setGeneric(name = ".setPars", 
           def = function(object, pars){standardGeneric(".setPars")})

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                          Regime METHODS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the number of parameters to be estimated in regime function #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime                                   #
#   returns                                                            #
# an integer                                                           #
#----------------------------------------------------------------------#
setMethod(f = ".getNumPars", 
          signature = c(object = "Regime"), 
          definition = function(object){
                         return( object@nVars ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the names of the parameters estimated in regime function    #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime                                   #
#   returns                                                            #
# a character vector                                                   #
#----------------------------------------------------------------------#
setMethod(f = ".getParNames",  
          signature = c(object = "Regime"), 
          definition = function(object){ 
                         return( object@vNames ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameters values                                       #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime                                   #
#   returns                                                            #
# a named numeric vector                                               #
#----------------------------------------------------------------------#
setMethod(f = ".getPars",  
          signature = c(object = "Regime"), 
          definition = function(object){ 
                         res <- object@pars
                         names(res) <- object@vNames
                         return( res ) 
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
          signature = c(object = "Regime"), 
          definition = function(object){ 
                         return( object@func ) 
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
          signature = c(x = "Regime", 
                        newdata = "data.frame"),
          definition = function (x, newdata, ...){
                         #-------------------------------------------#
                         # Create argument list for call to function #
                         #-------------------------------------------#
                         argList <- list()
                         for( j in 1L:x@nVars ) {
                           argList[[ x@vNames[j] ]] <- x@pars[j]
                         }

                         argList[[ 'data' ]] <- newdata

                         #-------------------------------------------#
                         # Call regime function                      #
                         #-------------------------------------------#
                         reg.g <- do.call(what = x@func, args = argList)

                         return( drop(reg.g) )
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class Regime                                   #
#   returns                                                            #
# a named numeric vector                                               #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "Regime"), 
          definition = function(object, ...){
                         return( .getPars(object) )
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
          signature = c(object = "Regime",
                        pars = "numeric"), 
          definition = function(object, pars){ 
                         object@pars <- pars
                         return( object ) 
                       } )


