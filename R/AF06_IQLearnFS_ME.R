# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        CLASS IQLearnFS_ME                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of regression of estimated main effects component of outcome #
#----------------------------------------------------------------------#
#   qFunc    : estimated outcome at each first stage treatment opt     #
# yMainHatSS : Estimated Second-Stage main effects                     #
#----------------------------------------------------------------------#
.checkValidity_IQLearnFS_ME <- function(object) {

  errors <- character()

  if( !is.na(object@estimatedValue) ) {
    msg <- "estimatedValue must be NA"
    errors <- c(errors, msg)
  }

  if( !all(is.na(object@optimalTx)) ) {
    msg <- "optimalTx must be NA"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "IQLearnFS_ME", 
         slots = c(qFunc      = "matrix",
                   yMainHatSS = "numeric"),
         contains = c("IQLearnBase", "IQLearnFS", "DynTxRegime"),
         validity = .checkValidity_IQLearnFS_ME)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      IQLearnFS_ME GENERICS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".newIQLearnFS_ME", 
           def = function(moMain, moCont, response, ...){
                   standardGeneric(".newIQLearnFS_ME")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       IQLearnFS_ME METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method                              # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_ME                             #
#   returns                                                            #
# a string describing method                                           #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnFS_ME"), 
          definition = function(object){ 
                         msg <- paste("IQ-Learning: Regression of",
                                      "estimated outcome main effects")
                         return( msg )
                       } )

#----------------------------------------------------------------------#
# Print the key results of method.                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnFS_ME                                  #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "IQLearnFS_ME"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         print(as(x,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# Show the key results of method.                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_ME                             #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "IQLearnFS_ME"), 
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         show(as(object,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# IQ-Learning Method for First Stage - Main Effects of Second Stage    #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj or NULL                       #
# moCont   : an object of class modelObj or NULL                       #
#            note that at least 1 of moMain and moCont must be defined #
# response : object of class IQLearSS                                  #
# data     : data.frame of covariates                                  #
# txName   : string; column header of treatment variable in data       #
# iter     : maximum number of iterations if iterative algorithm used  #
# suppress : T/F indicating if prints to screen are to be executed     #
#   returns                                                            #
# an object of class IQLearnFS_ME.                                     #
#----------------------------------------------------------------------#
.iqLearnFS_ME <- function(moMain,
                          moCont,
                          data,
                          response,
                          txName,
                          iter,
                          suppress) {

  if( !suppress ) {
    cat("IQ-Learning Algorithm\n")
    cat("Regression of estimated main effects component of outcome.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = NULL, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # The response is taken to be the estimated main effects component #
  # of the second-stage regression.                                  #
  #------------------------------------------------------------------#
  response <- fittedMain(response)

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

  qf <- .predictAllTreatments(object = est, data = data, response = response)

  result <- new("IQLearnFS_ME",
                "outcome"        = est,
                "qFunc"          = qf$vals,
                "yMainHatSS"     = response,
                "call"           = NULL,
                "optimalTx"      = NA,
                "estimatedValue" = NA)

  return( result )

}

setMethod(f = ".newIQLearnFS_ME",
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_ME)

setMethod(f = ".newIQLearnFS_ME",
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_ME)

setMethod(f = ".newIQLearnFS_ME",
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_ME)

