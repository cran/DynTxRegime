# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                 TypedSimpleFitNoSubsets GENERICS                 + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".newTypedSimpleFit", 
           def = function(moMain, moCont, txInfo, ...){
                   standardGeneric(".newTypedSimpleFit")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                  TypedSimpleFitNoSubsets METHODS                 + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Create main effects component of model formula                       #
#----------------------------------------------------------------------#
.main <- function(model) {

  tempMain <- terms(model)
  mainPart <- paste(attr(tempMain,"term.labels"), collapse="+")

  if( attr(tempMain,"intercept") < 0.5 ) {
    mainPart <- paste("0 + ", mainPart, sep="")
  }

  return( mainPart )

}

#----------------------------------------------------------------------#
# Create contrast component of model formula                           #
#----------------------------------------------------------------------#
.contrast <- function(model, txName) {

  tempCont <- terms(model)
  contPart <- paste(attr(tempCont,"term.labels"), collapse="+")

  if( attr(tempCont,"intercept") > 0.5 ) {
    contPart <- paste(txName, "+", txName, ":(", contPart, ")")

  } else {
    contPart <- paste(txName, ":(", contPart, ")")
  } 

  return(contPart)

}

.fitFuncNoSubsets <- function(moObject, 
                              response,  
                              data,
                              suppress) {
  #------------------------------------------------------------------#
  # Obtain fit                                                       #
  #------------------------------------------------------------------#
  fitObj <- try(modelObj::fit(object = moObject, 
                              data = data, 
                              response = response),
                silent = FALSE)

  if( is(fitObj, "try-error") ) {
    UserError("input",
              "unable to obtain fit for outcome regression")
  }

  #------------------------------------------------------------------#
  # Ensure that no parameter estimates are NA                        #
  #------------------------------------------------------------------#
  if( any(is.na(modelObj::coef(fitObj))) ) {
    UserError("input",
              "fit results in NA parameter estimates")
  }

  if( !suppress ) {
    cat("Outcome regression analysis:\n")
    print(fitObj)
  }

  return(fitObj)
}


#----------------------------------------------------------------------#
# Combines main effects and contrasts models to obtain parameter       #
# estimates in a single call to fit method.                            #
#----------------------------------------------------------------------#
# input arguments                                                      #
# moMain   : an object of class modelObj or NULL,                      #
# moCont   : an object of class modelObj or NULL,                      #
#            (Note at least one of moMain and moCont must not be NULL) #
# txInfo   : treatment information                                     #
# data     : data.frame of covariates and treatment histories          #
# response : response vector                                           #
# suppress : T/F indicating if prints to screen are executed           #
#   returns                                                            #
# an object of class TypedSimpleFitNoSubsets.                          #
#----------------------------------------------------------------------#
setMethod(f = ".newTypedSimpleFit", 
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txInfo = "TxInfoNoSubsets"), 
          definition = function(moMain,
                                moCont, 
                                txInfo,
                                data,
                                response, 
                                suppress){

                         txName <- .getTxName(txInfo)

                         #-------------------------------------------#
                         # Combine models                            #
                         #-------------------------------------------#
                         mainPart <- .main(model = model(moMain))
                         contPart <- .contrast(model = model(moCont), 
                                               txName = txName)

                         newForm <- paste("~", mainPart, "+", contPart)

                         if( !suppress ) {
                           cat("Combined outcome regression model:",
                                newForm, ".\n")
                         }

                         moMain@model <- as.formula(newForm)

                         result <- .fitFuncNoSubsets(moObject = moMain,
                                                     response = response,
                                                     data = data,
                                                     suppress = suppress)

                         result <- new(Class = "TypedSimpleFitNoSubsets",
                                       type = "Combined",
                                       txInfo = txInfo,
                                       result)

                         return(result)
                       } )

setMethod(f = ".newTypedSimpleFit", 
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        txInfo = "TxInfoNoSubsets"), 
          definition = function(moMain,
                                moCont, 
                                txInfo,
                                data,
                                response, 
                                suppress){

                         txName <- .getTxName(txInfo)

                         if( !suppress ) {
                           cat("moMain only outcome regression model.\n")
                         }

                         result <- .fitFuncNoSubsets(moObject = moMain,
                                                     response = response,
                                                     data = data,
                                                     suppress = suppress)

                         result <- new("TypedSimpleFitNoSubsets",
                                       type = "moMain",
                                       txInfo = txInfo,
                                       result)

                         return(result)

                       } )


setMethod(f = ".newTypedSimpleFit", 
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        txInfo = "TxInfoNoSubsets"), 
          definition = function(moMain,
                                moCont, 
                                txInfo,
                                data,
                                response, 
                                suppress){

                         txName <- .getTxName(txInfo)

                         #-------------------------------------------#
                         # Add treatment to contrast model           #
                         #-------------------------------------------#
                         contPart <- .contrast(model = model(moCont), 
                                               txName = txName)

                         newForm <- paste("~ -1 +", contPart)

                         moCont@model <- as.formula(newForm)

                         if( !suppress ) {
                           cat("moCont only outcome regression model.\n")
                         }

                         result <- .fitFuncNoSubsets(moObject = moCont,
                                                     response = response,
                                                     data = data,
                                                     suppress = suppress)

                         result <- new("TypedSimpleFitNoSubsets",
                                       type = "moCont",
                                       txInfo = txInfo,
                                       result)

                         return(result)

                       } )

#----------------------------------------------------------------------#
# Combines main effects and contrasts models to obtain parameter       #
# estimates in a single call to fit method.                            #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj or NULL,                      #
# moCont   : an object of class modelObj or NULL,                      #
#            (Note at least one of moMain and moCont must not be NULL) #
# txInfo   : treatment information                                     #
# data     : data.frame of covariates and treatment histories          #
# response : response vector                                           #
# suppress : T/F indicating if prints to screen are executed           #
#   returns                                                            #
# an object of class TypedSimpleFitWithSubsets.                        #
#----------------------------------------------------------------------#
setMethod(f = ".newTypedSimpleFit", 
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txInfo = "TxInfoWithSubsets"), 
          definition = function(moMain,
                                moCont, 
                                txInfo,
                                data,
                                response, 
                                suppress){

                         txName <- .getTxName(txInfo)

                         #-------------------------------------------#
                         # Combine model to create a single model    #
                         #-------------------------------------------#
                         mainPart <- .main(model = model(moMain))
                         contPart <- .contrast(model = model(moCont), 
                                               txName = txName)

                         newForm <- paste("~", mainPart, "+", contPart)

                         if( !suppress ) {
                           cat("Combined outcome regression model:",
                                newForm, ".\n")
                         }

                         moMain@model <- as.formula(newForm)

                         result <- .fitFuncNoSubsets(moObject = moMain,
                                                     response = response,
                                                     data = data,
                                                     suppress = suppress)

                         result <- new("TypedSimpleFitWithSubsets",
                                       type = "Combined",
                                       txInfo = txInfo,
                                       result)

                         return(result)
                       } )

setMethod(f = ".newTypedSimpleFit", 
          signature = c(moMain = "modelObj",
                        moCont = "NULL",
                        txInfo = "TxInfoWithSubsets"), 
          definition = function(moMain,
                                moCont, 
                                txInfo,
                                data,
                                response, 
                                suppress){

                         txName <- .getTxName(txInfo)

                         if( !suppress ) {
                           cat("moMain only outcome regression model.\n")
                         }

                         result <- .fitFuncNoSubsets(moObject = moMain,
                                                     response = response,
                                                     data = data,
                                                     suppress = suppress)

                         result <- new("TypedSimpleFitWithSubsets",
                                       type = "moMain",
                                       txInfo = txInfo,
                                       result)

                         return(result)

                       } )


setMethod(f = ".newTypedSimpleFit", 
          signature = c(moMain = "NULL",
                        moCont = "modelObj",
                        txInfo = "TxInfoWithSubsets"), 
          definition = function(moMain,
                                moCont, 
                                txInfo,
                                data,
                                response, 
                                suppress){

                         txName <- .getTxName(txInfo)

                         #-------------------------------------------#
                         # Add treatment to contrast model           #
                         #-------------------------------------------#
                         contPart <- .contrast(model = model(moCont), 
                                               txName = txName)

                         newForm <- paste("~ -1 +", contPart)

                         moCont@model <- as.formula(newForm)


                         if( !suppress ) {
                           cat("moCont only outcome regression model.\n")
                         }

                         result <- .fitFuncNoSubsets(moObject = moCont,
                                                     response = response,
                                                     data = data,
                                                     suppress = suppress)

                         result <- new("TypedSimpleFitWithSubsets",
                                       type = "moCont",
                                       txInfo = txInfo,
                                       result)

                         return(result)

                       } )
