setGeneric(name = ".newOptimalClass", 
           def = function(moPropen, moMain, moCont, moClass, ...){
                   standardGeneric(".newOptimalClass")
                 })

#----------------------------------------------------------------------#
# IPWE classification routine                                          #
#----------------------------------------------------------------------#
#  params                                                              #
#  moPropen : modelObj for propensity                                  #
#  moMain   : NULL                                                     #
#  moCont   : NULL                                                     #
#  moClass  : modelObj for classification                              #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  txName   : character name of treatment in data                      #
#  iter     : ignored                                                  #
#  suppress : T/F indicating if screen prints are suppressed           #
#----------------------------------------------------------------------#
.ipwe <- function(moPropen, 
                  moMain,
                  moCont,
                  moClass, 
                  data, 
                  response, 
                  txName, 
                  iter,
                  suppress, ...) {

  if( !suppress ) {
    cat("Inverse Probability Weighted Estimator - Classification.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = NULL, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Calculate contrast                                               #
  #------------------------------------------------------------------#
  contrast <- .contrastIPWE(txInfo = txInfo, 
                            propensity = propen, 
                            data = data,
                            response = response)

  #------------------------------------------------------------------#
  # Obtain classification fit                                        #
  #------------------------------------------------------------------#
  classFit <- .classFun(contrast = contrast$contrast, 
                        moClass = moClass,  
                        data = data)

  #------------------------------------------------------------------#
  # Obtain classification prediction from fit                        #
  #------------------------------------------------------------------#
  grp1 <- classFit$opt == "1"
  estResponse <- sum(contrast$contrast[grp1]) / nrow(data) + 
                 contrast$mean.mu0

  #------------------------------------------------------------------#
  # Estimate optimal treatment                                       #
  #------------------------------------------------------------------#
  optTx <- drop(predict(classFit$cf, newdata = data))

  if( !suppress ) {
    cat("Classification Analysis\n")
    print(classFit$cf)
    cat("Value:", estResponse, "\n")
  }

  oc1 <- new("OptimalClassIPWE",
             "classif"        = classFit$cf,
             "propen"         = propen,
             "estimatedValue" = estResponse,
             "optimalTx"      = optTx,
             "call"           = NULL)

  return( oc1 )

}

setMethod(f = ".newOptimalClass",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "NULL",
                        moClass  = "modelObj"), 
          definition = .ipwe)

#----------------------------------------------------------------------#
# prepares data and calls classification method specified by user.     #
#----------------------------------------------------------------------#
# contrast : Vector of the contrast function for each sample.          #
# moClass  : an object of class modelObj, which defines the models and #
#            R methods to be used to obtain parameter estimates and    #
#            predictions for the classification                        #
#            See ?modelObj for details.                                #
#                                                                      #
#            It is assumed that the solver.method contains             #
#              weights : A vector of weights to be used in the fitting #
#                        process.                                      #
# data     : data frame of covariates and response                     #
#----------------------------------------------------------------------#
#= Returns a list                                                     =#
#=   cf: classification fit object                                    =#
#=  opt: optimal treatment regime for training set                    =#
#----------------------------------------------------------------------#
.classFun <- function(contrast, moClass, data){

  #------------------------------------------------------------------#
  # Classification weight variable.                                  #
  #------------------------------------------------------------------#
  weights <- abs(contrast)

  #------------------------------------------------------------------#
  # Normalize weights                                                #
  #------------------------------------------------------------------#
  norm.weights <- weights / sum(weights)

  #------------------------------------------------------------------#
  # Add weights to formal arguments of classification method         #
  #------------------------------------------------------------------#
  cArgs <- solverArgs(moClass)
  cArgs[[ "weights" ]] <- norm.weights
  solverArgs(moClass) <- cArgs

  #------------------------------------------------------------------#
  # Classification labels                                            #
  #------------------------------------------------------------------#
  ZinternalZ <- as.numeric(contrast > -1.5e-8)
  ZinternalZ <- as.factor(ZinternalZ)

  #------------------------------------------------------------------#
  # Obtain classification fit using fit routine of modelObj          #
  #------------------------------------------------------------------#
  cf <- fit(object = moClass, data = data, response = ZinternalZ)

  #------------------------------------------------------------------#
  # Predict classification of training set                           #
  #------------------------------------------------------------------#
  pred <- predict(object = cf, newdata = data)

  if( !is(pred, "factor") ) {
    pred <- as.factor(pred)
  }
  levs <- levels(pred)

  if( length(levs) == 1L ) {
    tst <- c("0","1") %in% levs
    if( !any(tst) )  {
      msg <- "unable to resolve class designation returned by predict"
      UserError("input", msg)
    }
  } else if( length(levs) == 2L ) {
    tst <- c("0","1") %in% levs
    if( !all(tst) ) {
      pred <- factor(pred, labels=c("0","1"))
    }
  } else {
    msg <- "classification prediction method returns more than 2 classes"
    UserError("input", msg)
  }
  
  return(list("cf" = cf, "opt" = pred))
}


#----------------------------------------------------------------------#
# IPWE contrast function for a single decision point binary tx.        #
#----------------------------------------------------------------------#
# txInfo     : an object of class txInfo                               #
# propensity : propensity fit object                                   #
# data       : data frame of covariates                                #
# response   : a response vector                                       #
#----------------------------------------------------------------------#
#= Returns a list                                                     =#
#=    constrast, mean.mu0                                             =#
#----------------------------------------------------------------------#
.contrastIPWE <- function(txInfo, 
                          propensity, 
                          data,
                          response){

  #------------------------------------------------------------------#
  # Obtain matrix of propensity for treatment                        #
  #------------------------------------------------------------------#
  pm <- predict(object = propensity, newdata = data)

  #------------------------------------------------------------------#
  # Retrieve treatment column header                                 #
  #------------------------------------------------------------------#
  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Retrieve super set of treatment options                          #
  #------------------------------------------------------------------#
  fSetSuperSet <- .getSuperSet(txInfo)

  #------------------------------------------------------------------#
  # Convert treatment to 0/1 notation                                #
  #------------------------------------------------------------------#
  txVec <- numeric(nrow(data))
  txVec[data[,txName] == fSetSuperSet[1L]] <- 0L
  txVec[data[,txName] == fSetSuperSet[2L]] <- 1L

  #------------------------------------------------------------------#
  # Calculate IPWE contrast function.                                #
  #------------------------------------------------------------------#
  ym <- { txVec / pm[,2L] - (1.0 - txVec) / pm[,1L] } * response

  #------------------------------------------------------------------#
  # Calculate the mean value of patients that received base tx       #
  #------------------------------------------------------------------#
  mmu <- (1.0 - txVec) / pm[,1L] * response
  mmu <- mean(mmu)

  return(list("contrast" = ym,
              "mean.mu0" = mmu))
}


#----------------------------------------------------------------------#
# IPWE classification routine                                          #
#----------------------------------------------------------------------#
#  params                                                              #
#  moPropen : modelObj for propensity                                  #
#  moMain   : modelObj for main effects of outcome                     #
#  moCont   : modelObj for contrasts of outcome                        #
#  moClass  : modelObj for classification                              #
#  data     : data.frame of covariates and treatment history           #
#  response : outcome of interest                                      #
#  txName   : character name of treatment in data                      #
#  iter     : maximum # of iterations of iterative algorithm used      #
#  suppress : T/F indicating if screen prints are suppressed           #
#----------------------------------------------------------------------#
.aipwe <- function(moPropen, moMain, moCont, moClass,
                   data, response, txName, iter, suppress){

  if( !suppress ) {
    cat("Augmented Inverse Probability Weighted Estimator")
    cat(" - Classification.\n")
  }

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(fSet = NULL, 
                       txName = txName, 
                       data = data,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Fit outcome regression models                                    #
  #------------------------------------------------------------------#
  outcome <- .newOutcomeRegression(moMain = moMain, 
                                   moCont = moCont, 
                                   data = data,
                                   response = response,
                                   txInfo = txInfo,
                                   iter = iter,
                                   suppress = suppress)

  #------------------------------------------------------------------#
  # Calculate contrast                                               #
  #------------------------------------------------------------------#
  contrast <- .contrastAIPWE(outcome = outcome, 
                             txInfo = txInfo, 
                             propensity = propen, 
                             data = data,
                             response = response)

  #------------------------------------------------------------------#
  # Obtain classification fit                                        #
  #------------------------------------------------------------------#
  classFit <- .classFun(contrast = contrast$contrast, 
                        moClass = moClass,  
                        data = data)

  #------------------------------------------------------------------#
  # Obtain classification prediction from fit                        #
  #------------------------------------------------------------------#
  grp1 <- classFit$opt == "1"
  estResponse <- sum(contrast$contrast[grp1]) / nrow(data) + 
                 contrast$mean.mu0

  #------------------------------------------------------------------#
  # Estimate optimal treatment                                       #
  #------------------------------------------------------------------#
  optTx <- drop(predict(classFit$cf, newdata = data))

  if( !suppress ) {
    cat("Classification Analysis\n")
    print(classFit$cf)
    cat("Value:", estResponse, "\n")
  }

  oc1 <- new("OptimalClassAIPWE",
             "classif"        = classFit$cf,
             "propen"         = propen,
             "outcome"        = outcome,
             "estimatedValue" = estResponse,
             "optimalTx"      = optTx,
             "call"           = NULL)

  return( oc1 )

}

setMethod(f = ".newOptimalClass",    
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "modelObj",
                        moClass  = "modelObj"), 
          definition = .aipwe)

setMethod(f = ".newOptimalClass",    
          signature = c(moPropen = "modelObj",
                        moMain   = "modelObj",
                        moCont   = "NULL",
                        moClass  = "modelObj"), 
          definition = .aipwe)

setMethod(f = ".newOptimalClass",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "modelObj",
                        moClass  = "modelObj"), 
          definition = .aipwe)

#----------------------------------------------------------------------#
# AIPWE contrast function for a single decision point binary tx.       #
#----------------------------------------------------------------------#
# outcome   : an object of type SimpleFit or IterateFit                #
# txInfo    : an object of class TxInfo                                #
# propensity: A matrix of propensity scores.                           #
# data      : data frame of covariates                                 #
# response  : a response vector                                        #
#----------------------------------------------------------------------#
#= Returns a list                                                     =#
#=    constrast, mean.mu0                                             =#
#----------------------------------------------------------------------#
.contrastAIPWE <- function(outcome, 
                           txInfo, 
                           propensity, 
                           data,
                           response){

  #------------------------------------------------------------------#
  # Retrieve superset of treatment                                   #
  #------------------------------------------------------------------#
  fSetSuperSet <- .getSuperSet(txInfo)

  #------------------------------------------------------------------#
  # Retrieve treatment variable name                                 #
  #------------------------------------------------------------------#
  txName <- .getTxName(txInfo)

  #------------------------------------------------------------------#
  # Predict propensity of treatment for training data                #
  #------------------------------------------------------------------#
  pm <- predict(object = propensity, newdata = data)

  n <- nrow(data)

  #------------------------------------------------------------------#
  # Extract observed treatment                                       #
  #------------------------------------------------------------------#
  txVec <- numeric(n)
  txVec[data[,txName] == fSetSuperSet[1L]] <- 0L
  txVec[data[,txName] == fSetSuperSet[2L]] <- 1L

  #------------------------------------------------------------------#
  # Duplicate dataset for each treatment option                      #
  #------------------------------------------------------------------#
  dft <- rbind(data, data)
  dft[,txName] <- c(rep(fSetSuperSet[1L],n), 
                    rep(fSetSuperSet[2L],n))

  #------------------------------------------------------------------#
  # Predict outcome                                                  #
  #------------------------------------------------------------------#
  mu <- predict(object = outcome, newdata = dft)
  mu <- matrix(mu, ncol = 2L)

  #------------------------------------------------------------------#
  # Calculate AIPWE contrast function.                               #
  #------------------------------------------------------------------#
  ym <- txVec / pm[,2L] * response -
        (1.0 - txVec) / pm[,1L] * response -
        (txVec - pm[,2L]) / pm[,2L] * mu[,2L] - 
        (txVec - pm[,2L]) / pm[,1L] * mu[,1L]

  mmu <- (1.0 - txVec) / pm[,1L] * response -
         (txVec - pm[,2L]) / pm[,1L] * mu[,1L]

  mmu <- mean(mmu)

  return(list("contrast" = ym,
              "mean.mu0" = mmu))
}

