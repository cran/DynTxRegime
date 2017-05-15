setGeneric(name = ".newEARL", 
           def = function(moPropen, moMain, moCont, ...){
                   standardGeneric(".newEARL")
                 } )

#----------------------------------------------------------------------#
# moPropen  : modelObj for propensity fit                              #
# moMain    : modelObj for main effects of outcome (missing)           #
# moCont    : modelObj for contrasts of outcome (missing)              #
# data      : data.frame of covariates and treatment                   #
# response  : vector of response                                       #
# txName    : column header of data for treatment variable             #
# regime    : formula object for decision function                     #
# lambda    : tuning parameters to be considered                       #
# cvFolds   : number of cross validation folds                         #
# surrogate : character description of the surrogate loss-function     #
# guess     : Initial parameter guess for optimization routine         #
# txVec     : treatment coded as +/- 1.0                               #
# suppress  : T/F indicating of prints should be displayed             #
#----------------------------------------------------------------------#
.EARLIPWE <- function(moPropen, 
                      moMain,
                      moCont,
                      data,  
                      response,  
                      txName,  
                      regime,
                      lambdas,  
                      cvFolds,  
                      surrogate,
                      guess,
                      txVec,
                      suppress,...){

  #------------------------------------------------------------------#
  # Obtain model matrix for decision rule                            #
  #------------------------------------------------------------------#
  x <- try(model.matrix(object = regime, data = data), silent = TRUE)

  if( is(x,"try-error") ) {
    UserError("input",
              paste("at least one element of the",
                    "covariates in regime was not",
                    "found in data"))
  }

  #------------------------------------------------------------------#
  # Determine if decision function includes an intercept             #
  #------------------------------------------------------------------#
  hasIntercept <- attr(terms(regime),"intercept") == 1L
  if( hasIntercept ) x <- x[,-1L]

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(txName = txName, 
                       data = data, 
                       fSet = NULL,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve fitted propensity for treatment                         #
  #------------------------------------------------------------------#
  pr <- predict(object = propen, newdata = data)
  prWgt <- numeric(nrow(data))
  prWgt[txVec >  0.5] <- pr[txVec >  0.5, 2L]
  prWgt[txVec < -0.5] <- pr[txVec < -0.5, 1L]

  #------------------------------------------------------------------#
  # Determine the best parameter value(s)                            #
  #------------------------------------------------------------------#
  if( cvFolds > 0L ) {
    cvResult <- .crossValidation1Par(optimFunc = ".newEARLOptim",
                                     valueFunc = ".valueFuncOWL",
                                     response = response,
                                     prWgt = prWgt,
                                     txVec = txVec,
                                     cvFolds = cvFolds,
                                     lambdas = lambdas,
                                     x = x,
                                     suppress = suppress,
                                     surrogate = surrogate,
                                     guess = guess)
  } else {
    cvResult <- list()
    cvResult$value <- NULL
    cvResult$lambda <- lambdas[1L]
  }

  #------------------------------------------------------------------#
  # Obtain training results at the lambda and kernel parameter.      #
  #------------------------------------------------------------------#
  trainResult <- .newEARLOptim(subset = 1L:nrow(x),
                               x = x,
                               lambda = cvResult$lambda, 
                               txVec = txVec,
                               prWgt = prWgt,
                               response = response,
                               surrogate = surrogate,
                               guess = guess,
                               suppress = suppress)

  if( is(trainResult, "NULL") ) return( NULL )

  #------------------------------------------------------------------#
  # Estimate optimal treatment for training data                     #
  #------------------------------------------------------------------#
  optVec <- .predictOptimalTx(x = trainResult, newdata = x)

  value <- .valueFuncOWL(subset = 1L:nrow(x), 
                         optTx = optVec$optimalTx, 
                         txVec = txVec,
                         prWgt = prWgt, 
                         response = response)

  if( !suppress ) {
    cat("Estimated value:", value, "\n")
  }

  fSetSuperSet <- .getSuperSet(txInfo)
  topt <- NULL
  topt[optVec$optimalTx < -0.5] <- fSetSuperSet[1L]
  topt[optVec$optimalTx >  0.5] <- fSetSuperSet[2L]

  result <- new("EARLIPWE",
                "propen"          = propen,
                "optim"           = trainResult,
                "decisionFunc"    = optVec$decisionFunc,
                "crossValidation" = cvResult$cv,
                "regime"          = regime,
                "txInfo"          = txInfo,
                "estimatedValue"  = value,
                "optimalTx"       = topt,
                "call"            = NULL)

  return(result)
}

setMethod(f = ".newEARL",    
          signature = c(moPropen = "modelObj",
                        moMain   = "NULL",
                        moCont   = "NULL"), 
          definition = .EARLIPWE)

#----------------------------------------------------------------------#
# moPropen  : modelObj for propensity fit                              #
# moMain    : modelObj for the main effects.                           #
# moCont    : modelObj for the contrasts                               #
# data      : data.frame of covariates and treatment                   #
# response  : vector of response                                       #
# txName    : column header of data for treatment variable             #
# regime    : formula object for regime                                #
# lambda    : tuning parameters to be considered                       #
# cvFolds   : number of cross validation folds                         #
# surrogate : character description of the surrogate loss-function     #
# iter      : maximum number of iterations to use if fitting moMain    #
#             and moCont separately.                                   #
# txVec     : treatment coded as +/- 1.0                               #
# guess     : Initial parameter estimates for optimization             #
# suppress  : T/F indicating if screen prints should be executed       #
#----------------------------------------------------------------------#
.EARLAIPWE <- function(moPropen, 
                       moMain,
                       moCont,
                       data,  
                       response,  
                       txName,  
                       regime,  
                       lambdas,  
                       cvFolds,  
                       surrogate,
                       iter, 
                       txVec,
                       guess,
                       suppress){

  #------------------------------------------------------------------#
  # Obtain model matrix for decision function.                       #
  #------------------------------------------------------------------#
  x <- try(model.matrix(regime,data), silent = TRUE)

  if( is(x,"try-error") ) {
    UserError("input",
              paste("at least one element of the",
                    "covariates in regime was not",
                    "found in data"))
  }

  #------------------------------------------------------------------#
  # Determine if decision rule includes an intercept                 #
  #------------------------------------------------------------------#
  hasIntercept <- attr(terms(regime),"intercept") == 1L
  if( hasIntercept ) x <- x[,-1L]

  #------------------------------------------------------------------#
  # Process treatment information.                                   #
  #------------------------------------------------------------------#
  txInfo <- .newTxInfo(txName = txName, 
                       data = data, 
                       fSet = NULL,
                       suppress = suppress)

  #------------------------------------------------------------------#
  # Fit propensity models                                            #
  #------------------------------------------------------------------#
  propen <- .newPropensityRegression(moPropen = moPropen, 
                                     txInfo = txInfo, 
                                     data = data,
                                     suppress = suppress)

  #------------------------------------------------------------------#
  # Retrieve fitted propensity for treatment                         #
  #------------------------------------------------------------------#
  pr <- predict(object = propen, newdata = data)
  prWgt <- numeric(nrow(data))
  prWgt[txVec >  0.5] <- pr[txVec >  0.5, 2L]
  prWgt[txVec < -0.5] <- pr[txVec < -0.5, 1L]

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
  # Duplicate dataset for each treatment option                      #
  #------------------------------------------------------------------#
  dft <- rbind(data, data)
  levs <- .getLevels(txInfo, data[,txName])

  dft[,txName] <- c(rep(levs[1L], nrow(data)), 
                    rep(levs[2L], nrow(data)))

  #------------------------------------------------------------------#
  # Predict outcome                                                  #
  #------------------------------------------------------------------#
  mu <- predict(object = outcome, newdata = dft)
  mu <- matrix(mu, ncol = 2L)

  #------------------------------------------------------------------#
  # Determine the best parameter value(s)                            #
  #------------------------------------------------------------------#
  if( cvFolds > 0L ) {
    cvResult <- .crossValidation1Par(optimFunc = '.newEARLOptim',
                                     valueFunc = '.valueFuncEARLAIPWE',
                                     response = response,
                                     prWgt = prWgt,
                                     txVec = txVec,
                                     cvFolds = cvFolds,
                                     lambdas = lambdas,
                                     x = x,
                                     suppress = suppress,
                                     surrogate = surrogate,
                                     mu = mu,
                                     guess = guess)
  } else {
    cvResult <- list()
    cvResult$cv <- NULL
    cvResult$lambda <- lambdas[1L]
  }

  #------------------------------------------------------------------#
  # Obtain training results at the lambda parameter.                 #
  #------------------------------------------------------------------#
  trainResult <- .newEARLOptim(subset = 1L:nrow(x),
                               x = x,
                               lambda = cvResult$lambda, 
                               txVec = txVec,
                               prWgt = prWgt,
                               response = response,
                               surrogate = surrogate,
                               mu = mu,
                               guess = guess,
                               suppress = suppress)

  if( is(trainResult, "NULL") ) return(NULL)

  #------------------------------------------------------------------#
  # Estimate optimal treatment for training data                     #
  #------------------------------------------------------------------#
  optVec <- .predictOptimalTx(x = trainResult, newdata = x)

  value <- .valueFuncEARLAIPWE(subset = 1L:nrow(x), 
                               optTx = optVec$optimalTx, 
                               txVec = txVec,
                               prWgt = prWgt, 
                               response = response, 
                               mu = mu)

  if( !suppress ) {
    cat("Estimated value:", value, "\n")
  }

  fSetSuperSet <- .getSuperSet(txInfo)
  topt <- NULL
  topt[optVec$optimalTx < -0.5] <- fSetSuperSet[1L]
  topt[optVec$optimalTx >  0.5] <- fSetSuperSet[2L]

  result <- new("EARLAIPWE",
                "decisionFunc"    = optVec$decisionFunc,
                "propen"          = propen,
                "optim"           = trainResult,
                "outcome"         = outcome,
                "crossValidation" = cvResult$cv,
                "regime"          = regime,
                "txInfo"          = txInfo,
                "estimatedValue"  = value,
                "optimalTx"       = topt,
                "call"            = NULL)

  .checkEARLAIPWE(result)

  return(result)
}

setMethod(f = ".newEARL",    
          signature = c(moPropen  = "modelObj",
                        moMain    = "modelObj",
                        moCont    = "modelObj"), 
          definition = .EARLAIPWE)

setMethod(f = ".newEARL",    
          signature = c(moPropen  = "modelObj",
                        moMain    = "modelObj",
                        moCont    = "NULL"), 
          definition = .EARLAIPWE)

setMethod(f = ".newEARL",    
          signature = c(moPropen  = "modelObj",
                        moMain    = "NULL",
                        moCont    = "modelObj"), 
          definition = .EARLAIPWE)


.valueFuncEARLAIPWE <- function(subset, 
                                optTx, 
                                txVec, 
                                prWgt, 
                                response, 
                                mu, ...){

  optTx <- optTx[subset]
  txVec <- txVec[subset]
  prWgt <- prWgt[subset]
  response <- response[subset]
  mu <- mu[subset,,drop=FALSE]

  posOpt <- optTx > 0.5
  posTx <- txVec > 0.5

  ind <- {!posTx & !posOpt} | {posTx & posOpt}

  mec <- mu[,1L]
  mec[posOpt] <- mu[posOpt, 2L]

  val <- {response - mec} * ind / prWgt + mec
  badVal <- is.infinite(val) | is.na(val) | is.nan(val)

  val[badVal ] <- 0.0

  value <- mean(val)

  return( value )
}
