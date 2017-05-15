# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        CLASS IQLearnFS_C                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# Results of regression of estimated contrasts component of outcome    #
#----------------------------------------------------------------------#
#  txVec      : treatment vector from training data                    #
#  qFunc      : estimated contrast at each first stage treatment opt   #
#  residuals  : residuals of the fit                                   #
#  yContHatSS : Estimated contrast component of outcome                #
#  fitObj     : OutcomeRegression object.                              #
#----------------------------------------------------------------------#
.checkValidity_IQLearnFS_C <- function(object) {

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
setClass(Class = "IQLearnFS_C", 
         slots = c(txVec      = "numeric",
                   qFunc      = "matrix",
                   residuals  = "numeric",
                   yContHatSS = "numeric"),
         contains = c("IQLearnBase", "IQLearnFS", "DynTxRegime"),
         validity = .checkValidity_IQLearnFS_C)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       IQLearnFS_C GENERICS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".newIQLearnFS_C", 
           def = function(moMain, moCont, response, ...){
                   standardGeneric(".newIQLearnFS_C")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       IQLearnFS_C METHODS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method                              # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_C                              #
#   returns                                                            #
# a string describing method                                           #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnFS_C"), 
          definition = function(object){ 
                         msg <- paste("IQ-Learning: Regression of",
                                      "estimated outcome contrasts")
                         return( msg ) 
                       } )

#----------------------------------------------------------------------#
# Print the key results of method.                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnFS_C                                   #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "IQLearnFS_C"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         print(as(x,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the residuals of the contrast after modeling                #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_C                              #
#   returns                                                            #
# residuals of the regression                                          #
#----------------------------------------------------------------------#
setMethod(f = "residuals",
          signature = c(object="IQLearnFS_C"),
          definition = function(object, ...){ 
                         return( object@residuals ) 
                       } )

#----------------------------------------------------------------------#
# Retrieve the standard deviation                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_C                              #
#   returns                                                            #
# numeric, standard deviation of residuals                             #
#----------------------------------------------------------------------#
setMethod(f = "sd", 
          signature = c(x = "IQLearnFS_C"), 
          definition = function(x, na.rm=FALSE){ 
                         resid <- residuals(x)
                         stdDev <- sd(resid)
                         return( stdDev ) 
                       } )

#----------------------------------------------------------------------#
# Show the key results of method.                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_C                              #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "IQLearnFS_C"), 
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
# data     : data.frame of covariates                                  #
# response : object of class IQLearSS                                  #
# txName   : string; column header of treatment variable in data       #
# iter     : maximum number of iterations if iterative algorithm used  #
# suppress : T/F indicating if prints to screen are to be executed     #
#   returns                                                            #
# an object of class IQLearnFS_C .                                     #
#----------------------------------------------------------------------#
.iqLearnFS_C <- function(moMain,
                         moCont,
                         response,
                         data,
                         txName,
                         iter,
                         suppress) {

  if( !suppress ) {
    cat("IQ-Learning Algorithm\n")
    cat("Regression of estimated contrasts component of outcome.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = NULL, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # The response is taken to be the estimated contrast component of  #
  # the second-stage regression.                                     #
  #------------------------------------------------------------------#
  response <- fittedCont(response)

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

  residual <- response - drop(predict(est, data))

  qf <- .predictAllTreatments(object = est, data = data, response = response)

  txVec <- integer(nrow(data)) + 1L
  txVec[data[,txName] == .getSuperSet(txInfo)[1L]] <- -1L

  result <- new("IQLearnFS_C",
                "txVec"          = txVec,
                "qFunc"          = qf$vals,
                "residuals"      = residual,
                "yContHatSS"     = response,
                "outcome"        = est,
                "call"           = NULL,
                "optimalTx"      = NA,
                "estimatedValue" = NA)

  return( result )

}

setMethod(f = ".newIQLearnFS_C",
          signature = c(moMain   = "modelObj",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_C)

setMethod(f = ".newIQLearnFS_C",
          signature = c(moMain   = "modelObj",
                        moCont   = "NULL",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_C)

setMethod(f = ".newIQLearnFS_C",
          signature = c(moMain   = "NULL",
                        moCont   = "modelObj",
                        response = "IQLearnSS"), 
          definition = .iqLearnFS_C)
