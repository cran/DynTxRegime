# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       CLASS IQLearnFS_VHet                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# Results obtained from a call to iqLearnVar method w/models           #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#   fitObj    : OutcomeRegression object.                              #
#   residuals : stdized residuals of contrast after modeling           #
#   scale     : Scaling factor for stdization                          #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
.checkValidity_IQLearnFS_VHet <- function(object) {

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
setClass(Class = "IQLearnFS_VHet", 
         slots = c(    scale = "numeric",
                       qFunc = "matrix",
                   residuals = "numeric"),
         contains = c("IQLearnBase", "IQLearnFS", "DynTxRegime"),
         validity = .checkValidity_IQLearnFS_VHet)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                     IQLearnFS_VHet GENERICS                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".newIQLearnFS_VHet", 
           def = function(object, moMain, moCont, ...){
                   standardGeneric(".newIQLearnFS_VHet")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      IQLearnFS_VHet METHODS                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method                              # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_VHet                           #
#   returns                                                            #
# a string describing method                                           #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnFS_VHet"), 
          definition = function(object){ 
                         msg <- paste("IQ-Learning: Variance",
                                      "log-linear model" ) 
                         return( msg )
                       } )


#----------------------------------------------------------------------#
# Print the key results of method.                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnFS_VHet                                #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "IQLearnFS_VHet"), 
          definition = function(x, ...){
                         cat("\n", DTRstep(x), "\n")
                         print(as(x,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# Generate qqplot                                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class IQLearnFS_VHet                                #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "qqplot",
          signature = c(x = "IQLearnFS_VHet",
                        y = "ANY",
                        plot.it = "ANY",
                        xlab = "ANY",
                        ylab = "ANY"),
          definition = function(x, ...){
                         x <- x@residuals
                         qqnorm(x, ...)
                         qqline(x)
                       } )

#----------------------------------------------------------------------#
# Retrieve the residuals of the contrast after modeling                #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_VHet                           #
#   returns                                                            #
# object determined by regression class                                #
#----------------------------------------------------------------------#
setMethod(f = "residuals",
          signature = c(object="IQLearnFS_VHet"),
          definition = function(object, ...){ 
                         return( object@residuals ) 
                       } )

#----------------------------------------------------------------------#
# Show the key results of method.                                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class IQLearnFS_VHet                           #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "IQLearnFS_VHet"), 
          definition = function(object){
                         cat("\n", DTRstep(object), "\n")
                         show(as(object,"IQLearnBase"))
                       } )

#----------------------------------------------------------------------#
# IQ-Learning Method for First Stage - Variance Log-Linear             #
#----------------------------------------------------------------------#
#   params                                                             #
# object   : an object of class IQLearnFS_C                            #
# moMain   : an object of class modelObj or NULL                       #
# moCont   : an object of class modelObj or NULL                       #
#            note that at least 1 of moMain and moCont must be defined #
# data     : data.frame of covariates                                  #
# iter     : maximum number of iterations if iterative algorithm used  #
# suppress : T/F indicating if prints to screen are to be executed     #
#   returns                                                            #
# an object of class IQLearnFS_VHet                                    #
#----------------------------------------------------------------------#
.iqLearnFS_VHet <- function(object, 
                            moMain,  
                            moCont,  
                            data,  
                            iter, 
                            suppress){

  if( !suppress ) {
    cat("IQ-Learning Algorithm\n")
    cat("Regression of residuals using log-linear modeling.\n")
  }

  txName <- .getTxName(object@outcome@txInfo)

  data[,txName] <- as.integer(round(object@txVec,0L))

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = NULL, 
                       txName = txName, 
                       data = data,
                       suppress = TRUE)

  #------------------------------------------------------------------#
  # Retrieve residuals of first-stage fit of second-stage contrast   #
  #------------------------------------------------------------------#
  resid <- residuals(object)

  #------------------------------------------------------------------#
  # Response is ln(r*r)                                              #
  #------------------------------------------------------------------#
  response <- log(resid*resid)

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
  # Retrieve the fitted response for patients                        #
  #------------------------------------------------------------------#
  fitted <- predict(est, data)

  #------------------------------------------------------------------#
  # Standardize the fitted residuals                                 #
  #------------------------------------------------------------------#
  sig <- exp(fitted / 2.0)
  stdResids <- as.vector(resid / sig)
  sdr <- sd(stdResids)
  #------------------------------------------------------------------#
  # ln(E[r^2/|rHat|^2] - (E[r/|rHat|])^2)                            #
  #------------------------------------------------------------------#
  sd.stdResids <- 2.0 * log(sdr)
  sig <- sig*(sdr)
  #------------------------------------------------------------------#
  # r/|rHat| 1/sqrt(E[r^2/|rHat|^2] - (E[r/|rHat|])^2)               #
  #------------------------------------------------------------------#
  stdResids <- as.vector(resid) / sig

  qf <- .predictAllTreatments(object = est, data = data, response = response)

  result <- new("IQLearnFS_VHet",
                "residuals"      = drop(stdResids),
                "qFunc"          = qf$vals,
                "scale"          = sd.stdResids,
                "outcome"        = est, 
                "call"           = NULL,
                "optimalTx"      = NA,
                "estimatedValue" = NA)

}

setMethod(f = ".newIQLearnFS_VHet",
          signature = c(object = "IQLearnFS_C",
                        moMain = "modelObj",
                        moCont = "modelObj"), 
          definition = .iqLearnFS_VHet)

setMethod(f = ".newIQLearnFS_VHet",
          signature = c(object = "IQLearnFS_C",
                        moMain = "modelObj",
                        moCont = "NULL"), 
          definition = .iqLearnFS_VHet)

setMethod(f = ".newIQLearnFS_VHet",
          signature = c(object = "IQLearnFS_C",
                        moMain = "NULL",
                        moCont = "modelObj"), 
          definition = .iqLearnFS_VHet)
