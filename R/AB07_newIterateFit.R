setGeneric(name = ".newIterateFit", 
           def = function(moMain, moCont, txInfo, ...){
                   standardGeneric(".newIterateFit")
                 })

#----------------------------------------------------------------------#
# Fit main effects and contrasts models iteratively.                   #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj,                              #
# moCont   : an object of class modelObj,                              #
# response : response vector                                           #
# txInfo   : treatment information                                     #
# data     : data.frame of covariates                                  #
# max.iter : maximum number of iterations                              #
# suppress : T/F indicating if prints to screen are to be executed     #
#   returns                                                            #
# an object of class IterateFitNoSubsets                               #
#----------------------------------------------------------------------#
.iterateFitNoSubsets <- function(moMain, 
                                 moCont, 
                                 txInfo, 
                                 response, 
                                 data, 
                                 max.iter, 
                                 suppress) {

  result <- .iterateFit(moMain = moMain,
                        moCont = moCont,
                        response = response,
                        txInfo = txInfo,
                        data = data,
                        max.iter = max.iter,
                        suppress = suppress)

  result <- new("IterateFitNoSubsets",
                "fitObjME" = result$fitObjME,
                "fitObjC"  = result$fitObjC)

  return(result)
}

setMethod(f = ".newIterateFit", 
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txInfo = "TxInfoNoSubsets"), 
          definition = .iterateFitNoSubsets)

#----------------------------------------------------------------------#
# Fit main effects and contrasts models iteratively.                   #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj,                              #
# moCont   : an object of class modelObj,                              #
# response : response vector                                           #
# txInfo   : treatment information                                     #
# data     : data.frame of covariates                                  #
# max.iter : maximum number of iterations                              #
# suppress : T/F indicating if prints to screen are to be executed     #
#   returns                                                            #
# an object of class IterateFitWithSubsets                             #
#----------------------------------------------------------------------#
.iterateFitWithSubsets <- function(moMain, 
                                   moCont, 
                                   response, 
                                   txInfo, 
                                   data, 
                                   max.iter, 
                                   suppress) {

  result <- .iterateFit(moMain = moMain,
                        moCont = moCont,
                        response = response,
                        txInfo = txInfo,
                        data = data,
                        max.iter = max.iter,
                        suppress = suppress)

  result <- new("IterateFitWithSubsets",
                "fitObjME" = result$fitObjME,
                "fitObjC"  = result$fitObjC)

  return(result)
}

setMethod(f = ".newIterateFit", 
          signature = c(moMain = "modelObj",
                        moCont = "modelObj",
                        txInfo = "TxInfoWithSubsets"), 
          definition = .iterateFitWithSubsets)

#----------------------------------------------------------------------#
# Fit main effects and contrasts models iteratively.                   #
#----------------------------------------------------------------------#
#   params                                                             #
# moMain   : an object of class modelObj,                              #
# moCont   : an object of class modelObj,                              #
# response : response vector                                           #
# txInfo   : treatment information                                     #
# data     : data.frame of covariates                                  #
# max.iter : maximum number of iterations                              #
# suppress : T/F indicating if prints to screen are to be executed     #
#   returns                                                            #
# an object of class IterateFitWithSubsets                             #
#----------------------------------------------------------------------#
.iterateFit <- function(moMain, 
                        moCont, 
                        response, 
                        txInfo, 
                        data, 
                        max.iter, 
                        suppress) {

  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Set tolerance for equivalent results.                            #
  #------------------------------------------------------------------#
  tol <- 1.5e-8

  #------------------------------------------------------------------#
  # Initialize contrast component of response to zero                #
  #------------------------------------------------------------------#
  fittedCont <- numeric(nrow(data))
  response <- drop(response)

  contrast.old <- numeric(nrow(data)) + 1.0/0.0
  main.old <- numeric(nrow(data)) + 1.0/0.0

  #------------------------------------------------------------------#
  # Iterate until the number of iterations reaches max.iter or       #
  # difference between iterations reaches tol                        #
  #------------------------------------------------------------------#
  iter <- 0L
  while(TRUE){

    #--------------------------------------------------------------#
    # Obtain fit for main effects                                  #
    #--------------------------------------------------------------#
    tempY <- response - fittedCont

    fitMain <- .newTypedSimpleFit(moMain = moMain,
                                  moCont = NULL, 
                                  txInfo = txInfo,
                                  data = data,
                                  response = tempY, 
                                  suppress = TRUE)

    fittedMain <- drop(predict(object = fitMain, newdata = data))

    #--------------------------------------------------------------#
    # Redefine response to be contrast.                            #
    #--------------------------------------------------------------#
    tempY <- response - fittedMain

    #--------------------------------------------------------------#
    # Obtain fit                                                   #
    #--------------------------------------------------------------#
    fitCont <- .newTypedSimpleFit(moMain = NULL,
                                  moCont = moCont, 
                                  txInfo = txInfo,
                                  data = data,
                                  response = tempY, 
                                  suppress = TRUE)

    #--------------------------------------------------------------#
    # Calculate fitted contrast                                    #
    #--------------------------------------------------------------#
    fittedCont <- drop(predict(object = fitCont, newdata = data))

    #--------------------------------------------------------------#
    # Compare the main effects and contrasts components of this    #
    # iteration to those of last to determine max difference       #
    #--------------------------------------------------------------#
    tst <- isTRUE(all.equal(contrast.old, fittedCont, tolerance=tol))
    tst <- isTRUE(all.equal(main.old, fittedMain, tolerance=tol)) && tst
    if( tst ) break

    #--------------------------------------------------------------#
    # Store current iteration values for comparison at next iter   #
    #--------------------------------------------------------------#
    contrast.old <- fittedCont
    main.old <- fittedMain
    iter <- iter + 1L
    if( iter > max.iter ){
      warning("convergence not attained within max iterations")
      break
    }
  }

  if( any(is.na(coef(fitMain))) || any(is.na(coef(fitCont))) ) {
    UserError("input",
              "fit results in NA parameter estimates")
  }

  if( !suppress ) {
    cat("Fit outcome regression in", iter, "iterations.\n")
    cat("Main Effects regression results:\n")
    print(fitMain)
    cat("Contrast regression results:\n")
    print(fitCont)
  }

  result <- list("fitObjME" = fitMain,
                 "fitObjC"  = fitCont)

  return(result)
}


