# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         CLASS IQLearnSS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results obtained from a call to iqLearnSS method                     #
#----------------------------------------------------------------------#
# extends IQLearnBase directly
#
#  yContHat : Estimated contrast component
#  yMainHat : Estimated main effects component
#   delta   : indicator of compliance * response used for value calc
#  decisionFunc : matrix of outcome regression at each treatment opt
#----------------------------------------------------------------------#
setClass(Class = "IQLearnSS", 
         slots = c(yContHat     = "numeric",
                   yMainHat     = "numeric",
                   delta        = "numeric",
                   decisionFunc = "matrix"),
         contains = c("IQLearnBase", "DynTxRegime"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      IQLearnSS GENERICS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".newIQLearnSS", 
           def = function(moMain, moCont, ...){
                   standardGeneric(".newIQLearnSS")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        IQLearnSS METHODS                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method                              # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnSS                                #
#   returns                                                            #
# a string describing method                                           #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnSS"), 
          definition = function(object){ 
                         return( "IQ-Learning: Second Stage" ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the fitted contrasts of the outcome regression steps of     #
# a DynTxRegime method                                                 #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class DynTxRegime                              #
#   returns                                                            #
# NULL, must be defined in the inheriting class. IQ-Learning SS only   #
#----------------------------------------------------------------------#
setMethod(f = "fittedCont",    
          signature = c(object = "IQLearnSS"), 
          definition = function(object, ...){
                         return( object@yContHat )
                       } )

#----------------------------------------------------------------------#
# Retrieve the fitted main effects of the outcome regression steps of  #
# a DynTxRegime method                                                 #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class DynTxRegime                              #
#   returns                                                            #
# NULL, must be defined in the inheriting class. IQ-Learning SS only   #
#----------------------------------------------------------------------#
setMethod(f = "fittedMain",    
          signature = c(object = "IQLearnSS"), 
          definition = function(object, ...){
                         return( object@yMainHat )
                       } )

#----------------------------------------------------------------------#
# Estimate optimal treatment for new data                              #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnSS                                     #
# newdata : a data.frame of covariates and treatments for which the    #
# optimal treatment is to be estimated.                                #
#   returns                                                            #
# the estimated optimal treatment for data provided through newdata    #
# and the q-functions at each treatment.                               #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "IQLearnSS", 
                        newdata = "data.frame"),
          definition = function(x, newdata,...) {

                         qf <- .predictAllTreatments(object = x@outcome, 
                                                     data = newdata,
                                                     response = rep(NA,nrow(newdata)))

                         opt <- apply(X = qf$vals, MARGIN = 1L, which.max)
                         opt <- qf$ss[opt]

                         return( list("optimalTx"    = opt,
                                      "decisionFunc" = qf$vals) )
                       } )

#----------------------------------------------------------------------#
# Retrieve optimal treatment for training data                         #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnSS                                     #
# newdata : a data.frame of covariates and treatments for which the    #
# optimal treatment is to be estimated.                                #
#   returns                                                            #
# the estimated optimal treatment for training data                    #
# and the q-functions at each treatment.                               #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "IQLearnSS", 
                        newdata = "missing"),
          definition = function(x, newdata,...) {

                         return( list("optimalTx" = x@optimalTx,
                                      "decisionFunc" = x@decisionFunc) )
                       } )

#----------------------------------------------------------------------#
# Print the key results of method.                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnSS                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "IQLearnSS"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         print(as(x,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# Show the key results of method.                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnSS                                #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "IQLearnSS"), 
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         show(as(object,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# IQ-Learning Method for Second Stage                                  #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj or NULL                       #
# moCont   : an object of class modelObj or NULL                       #
#            note that at least 1 of moMain and moCont must be defined #
# data     : data.frame of covariates                                  #
# response : response vector                                           #
# txName   : string; column header of treatment variable in data       #
# iter     : maximum number of iterations if iterative algorithm used  #
# suppress : T/F indicating if prints to screen are executed           #
#   returns                                                            #
# an object of class IQLearnSS.                                        #
#----------------------------------------------------------------------#
.iqLearnSS <- function(moMain,
                       moCont,
                       data,
                       response,
                       txName,
                       iter, 
                       suppress) {

  if( !suppress ) {
    cat("Second-Stage analysis of IQ-Learning Algorithm\n")
  }

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  # Method does not allow for subset modeling                        #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = NULL, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Obtain fit of moMain and moCont models                           #
  #------------------------------------------------------------------#
  est <- .newOutcomeRegression(moMain = moMain, 
                               moCont = moCont,
                               data = data, 
                               response = response, 
                               txInfo = txInfo,
                               iter = iter, 
                               suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve Q-Functions                                             #
  #------------------------------------------------------------------#
  qf <- .predictAllTreatments(object = est, data = data, response = response)

  #------------------------------------------------------------------#
  # Determine optimal treatment.                                     #
  #------------------------------------------------------------------#
  opt <- apply(X = qf$vals, MARGIN = 1L, FUN = which.max)
  opt <- qf$ss[opt]

  result <- new("IQLearnSS",
                "outcome"        = est,
                "yContHat"       = 0.5*(qf$vals[,2L] - qf$vals[,1L]),
                "yMainHat"       = 0.5*(qf$vals[,2L] + qf$vals[,1L]),
                "delta"          = {opt == data[,txName]}*response,
                "call"           = NULL,
                "optimalTx"      = opt,
                "decisionFunc"   = qf$vals,
                "estimatedValue" = NA)

  return( result )

}

setMethod(f = ".newIQLearnSS",
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj"), 
          definition = .iqLearnSS)

setMethod(f = ".newIQLearnSS",
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL"), 
          definition = .iqLearnSS)

setMethod(f = ".newIQLearnSS",
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj"), 
          definition = .iqLearnSS)

