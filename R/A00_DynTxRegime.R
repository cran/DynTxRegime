setClassUnion("CallOrNull", 
              members = c("call","NULL"))

setClassUnion("LogicalOrNumeric", 
              members = c("logical","numeric"))

setClassUnion("MatrixOrVector", 
              members = c("matrix","vector"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         CLASS DynTxRegime                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# VIRTUAL class used for all objects returned by methods in DynTxRegime#
#----------------------------------------------------------------------#
# slots:
#
#   call           : An object of class 'CallOrNull'
#                    matched call to the DynTxRegime method
#   estimatedValue : An object of class 'LogicalOrNumeric'
#                    Estimated value as defined for the method called
#   optimalTx      : An object of class 'MatrixOrVector'
#                    Estimated optimal treatment for the training data
#
#----------------------------------------------------------------------#

setClass(Class = "DynTxRegime",
         slots = c(call           = "CallOrNull",
                   estimatedValue = "LogicalOrNumeric",
                   optimalTx      = "MatrixOrVector"   ),
         prototype = list(call           = NULL,
                          estimatedValue = numeric(),
                          optimalTx      = integer() ),
         contains = c("VIRTUAL"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       DynTxRegime GENERICS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
setGeneric(name = "classif", 
           def = function(object, ...){standardGeneric("classif")})

setGeneric(name = "cvInfo", 
           def = function(object){standardGeneric("cvInfo")})

setGeneric(name = "DTRstep", 
           def = function(object){standardGeneric("DTRstep")})

setGeneric(name = "estimator", 
           def = function(x, ...){standardGeneric("estimator")})

setGeneric(name = "fittedCont",  
           def = function(object, ...){standardGeneric("fittedCont")})

setGeneric(name = "fittedMain", 
           def = function(object, ...){standardGeneric("fittedMain")})

setGeneric(name = "genetic", 
           def = function(object, ...){standardGeneric("genetic")})

setGeneric(name = "optimObj", 
           def = function(object, ...){standardGeneric("optimObj")})

setGeneric(name = "optTx", 
           def = function(x, newdata, ...){standardGeneric("optTx")})

setGeneric(name = "regimeCoef", 
           def = function(object, ...){standardGeneric("regimeCoef")})

# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        DynTxRegime METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the results of the classification step                      # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "classif",
          signature = c(object="DynTxRegime"),
          definition = function(object, ...){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the results of the cross-validation step                    # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "cvInfo",   
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) })

#----------------------------------------------------------------------#
# Retrieve a string describing the method used to obtain the object    # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep",    
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the estimated value of the estimated optimal treatment      #
# regime                                                               # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   x : an object of class DynTxRegime                                 #
# returns                                                              #
#   value stored in slot estimatedValue                                #
#----------------------------------------------------------------------#
setMethod(f = "estimator",    
          signature = c(x = "DynTxRegime"), 
          definition = function(x){ return( x@estimatedValue ) } )

#----------------------------------------------------------------------#
# Retrieve the estimated value of the contrast components              # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "fittedCont",    
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the estimated value of the main effects components          # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "fittedMain",    
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the genetic algorithm object                                # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "genetic",    
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the optimization object                                     # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",    
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the estimated optimal treatment for the training data       #
#----------------------------------------------------------------------#
# required arguments                                                   #
#   x       : an object of class DynTxRegime                           #
#   newdata : missing                                                  #
# returns                                                              #
#   value stored in slot optimalTx                                     #
#----------------------------------------------------------------------#
setMethod(f = "optTx", 
          signature = c(x = "DynTxRegime",
                        newdata = "missing"), 
          definition = function(x, newdata){ 
                         return( x@optimalTx )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regime parameters                                       # 
#----------------------------------------------------------------------#
# required arguments                                                   #
#   object : an object of class DynTxRegime                            #
# returns                                                              #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "DynTxRegime"), 
          definition = function(object){ return( NA ) } )

#----------------------------------------------------------------------#
# Retrieve the residuals of the contrast after modeling                #
#----------------------------------------------------------------------#
#   params                                                             #
#   object : an object of class DynTxRegime                            #
#   returns                                                            #
#   NA, must be defined in the inheriting class.                       #
#----------------------------------------------------------------------#
setMethod(f = "residuals",
          signature = c(object="DynTxRegime"),
          definition = function(object, ...){ 
                         return( NA ) 
                       } )

setMethod(f = "sd", 
          signature = c(x = "DynTxRegime"), 
          definition = function(x, na.rm=FALSE){ 
                         return( NA ) 
                       } )

