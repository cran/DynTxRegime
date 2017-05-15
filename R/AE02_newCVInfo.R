setGeneric(name = ".newCVInfo", 
           def = function(lambdas, kparam, ...){
                   standardGeneric(".newCVInfo")
                 })

#----------------------------------------------------------------------#
# Single parameter cross-validation algorithm                          #
#----------------------------------------------------------------------#
# optimFunc : character name of the function being optimized           #
# valueFunc : character name of the function for determining value     #
# response  : vector of response                                       #
# prWgt     : Weights defined as propensity for tx received            #
# txVec     : vector of treatment coded as +/- 1                       #
# cvFolds   : integer number of cross-validation steps                 #
# lambdas   : vector of parameter values                               #
# x         : matrix of covariates and treatment histories             #
# suppress  : T/F indicating of prints to screen are executed          #
#----------------------------------------------------------------------#
.crossValidation1Par <- function(optimFunc,
                                 valueFunc,
                                 response,
                                 prWgt,
                                 txVec,
                                 cvFolds,
                                 lambdas,
                                 x,
                                 suppress, 
                                 kparam, ...) {

  if( missing(kparam) ) kparam <- NULL

  #------------------------------------------------------------------#
  # Number of regularization parameters to consider.                 #
  #------------------------------------------------------------------#
  nl <- length(lambdas)
  
  #------------------------------------------------------------------#
  # Number of patients in sample                                     #
  #------------------------------------------------------------------#
  n <- length(response)

  #------------------------------------------------------------------#
  # Storage matrix for values                                        #
  #------------------------------------------------------------------#
  valueLambda <- matrix(data = 0.0, 
                        ncol = nl, 
                        nrow = 1L,
                        dimnames = list(NULL,round(lambdas,3L)))

  #------------------------------------------------------------------#
  # Randomly sample data.                                            #
  #------------------------------------------------------------------#
  samplen <- sample(n)

  for( k in 1L:nl ) {

    if( !suppress ) {
      cat("Cross-validation for lambda =", lambdas[k], "\n")
    }

    #----------------------------------------------------------#
    # For each lambda obtain value.                            #
    # eachLambda returns the average value over the            #
    # successful folds or NA if no valid solutions were        #
    # found.                                                   #
    #----------------------------------------------------------#
    valueLambda[1L,k] <- .eachLambda(optimFunc = optimFunc,
                                     valueFunc = valueFunc,
                                     lambda = lambdas[k], 
                                     samplen = samplen,  
                                     folds = cvFolds,  
                                     x = x,  
                                     response = response,
                                     prWgt = prWgt,
                                     txVec = txVec,
                                     suppress = suppress, 
                                     kparam = kparam, ...)

  }

  #------------------------------------------------------------------#
  # Determine which lambda leads to largest value                    #
  #------------------------------------------------------------------#
  ivl <- which.max(valueLambda)

  lambda <- lambdas[ivl]

  if( !suppress ) {
    cat("Selected parameter: lambda =", lambda, "\n")
  }

  result <- new("CVInfo1Par",
                "value" = valueLambda,
                "pars" = lambdas,
                "opt"  = lambda)

  result <- list("cv" = result,
                 "lambda" = lambda)

  return(result)
}   

setMethod(f = ".newCVInfo",   
          signature = c(lambdas = "numeric",
                        kparam = "missing"), 
          definition = .crossValidation1Par)

setMethod(f = ".newCVInfo",   
          signature = c(lambdas = "numeric",
                        kparam = "NULL"), 
          definition = .crossValidation1Par)

#----------------------------------------------------------------------#
# Two parameter cross-validation algorithm                             #
#----------------------------------------------------------------------#
# optimFunc : character name of the function being optimized           #
# valueFunc : character name of the function for determining value     #
# response  : vector of response                                       #
# prWgt     : Weights defined as propensity for tx received            #
# x         : matrix of covariates and treatment histories             #
# txVec     : vector of treatment coded as +/- 1                       #
# cvFolds   : integer number of cross-validation steps                 #
# lambdas   : vector of parameter values                               #
# kernel    : character of kernel function to use                      #
# kparam    : vector of kernel parameters to consider                  #
# suppress  : T/F indicating of prints to screen are executed          #
#----------------------------------------------------------------------#
.crossValidation2Par <- function(optimFunc,
                                 valueFunc,
                                 response,
                                 prWgt,
                                 txVec,
                                 cvFolds,
                                 lambdas,
                                 x, 
                                 kernel,
                                 kparam,
                                 suppress, ...) {

  #------------------------------------------------------------------#
  # Number of regularization parameters to consider.                 #
  #------------------------------------------------------------------#
  nl <- length(lambdas)

  #------------------------------------------------------------------#
  # Process kernel information.                                      #
  #------------------------------------------------------------------#
  if( kernel == "radial" ) {
    #--------------------------------------------------------------#
    # If kernel is radial, np is the number of gamma parameters.   #
    #--------------------------------------------------------------#
    np <- length(kparam)
  } else {
    #--------------------------------------------------------------#
    # If kernel is poly or linear, only 1 parameter                #
    #--------------------------------------------------------------#
    np <- 1L
  }

  #------------------------------------------------------------------#
  # If only one kernel parameter use 1 parameter cross-validation    #
  # algorithm                                                        #
  #------------------------------------------------------------------#
  if( np == 1L ) {
    result <- .crossValidation1Par(optimFunc = optimFunc,
                                   valueFunc = valueFunc,
                                   response = response,
                                   prWgt = prWgt,
                                   txVec = txVec,
                                   cvFolds = cvFolds,
                                   lambdas = lambdas,
                                   x = x,
                                   suppress = suppress, 
                                   kernel = kernel,
                                   kparam = kparam, ...)

    result <- c("kparam" = kparam, result)

    return( result )
  }

  #------------------------------------------------------------------#
  # Storage matrix for CV results for each lambda/kparam pair.       #
  #------------------------------------------------------------------#
  valueLambda <- matrix(data = 0.0, 
                        nrow = nl, 
                        ncol = np,
                        dimnames = list(round(lambdas,3L),  
                                        round(kparam,3L)))

  #------------------------------------------------------------------#
  # Randomly sample data.                                            #
  #------------------------------------------------------------------#
  n <- length(response)
  samplen <- sample(n)

  for( j in 1L:np ) { 
    for( k in 1L:nl ) {

      if( !suppress ) {
        cat("Cross-validation for lambda =", lambdas[k],
            "kparam =", kparam[j], "\n")
      }

      #----------------------------------------------------------#
      # For each lambda, parameter combination obtain value.     #
      # .eachLambda returns the average value over the           #
      # successful folds or NA if no valid solutions were        #
      # found.                                                   #
      #----------------------------------------------------------#
      valueLambda[k,j] <- .eachLambda(optimFunc = optimFunc,
                                      valueFunc = valueFunc,
                                      lambda = lambdas[k], 
                                      samplen = samplen,  
                                      folds = cvFolds,  
                                      x = x,  
                                      response = response,
                                      prWgt = prWgt,
                                      kernel = kernel,
                                      kparam = kparam[j],
                                      txVec = txVec,
                                      suppress = suppress, ...)
    }
  }

  #------------------------------------------------------------------#
  # Identify tuning parameter pair that leads to the largest         #
  # average value. Note that if more than one pair leads to the      #
  # largest average value, the pair with the smallest lambda and     #
  # the largest kernel parameter is selected.                        #
  #------------------------------------------------------------------#
  bestSigma <- apply(X = valueLambda, 
                     MARGIN = 1L, 
                     FUN = function(x){
                             res <- length(x) - which.max(rev(x)) + 1L
                             return( res )
                           } )

  ivl <- which.max(apply(X = valueLambda, 
                         MARGIN = 1L, 
                         FUN = max, 
                         na.rm = TRUE))

  result <- new("CVInfo2Par",
                "value" = valueLambda,
                "pars" = lambdas,
                "opt"  = lambdas[ivl],
                "pars2" = kparam,
                "opt2" = kparam[bestSigma[ivl]])

  lambda <- lambdas[ivl]

  kparam <- kparam[bestSigma[ivl]]

  if( !suppress ) {
    cat("Selected parameters\n")
    cat("lambda =", lambda, "\n")
    cat("kparam =", kparam, "\n")
  }

  result <- list("cv" = result,
                 "kparam" = kparam,
                 "lambda" = lambda)


  return(result)
}

setMethod(f = ".newCVInfo",   
          signature = c(lambda = "numeric",
                        kparam = "numeric"), 
          definition = .crossValidation2Par)

#----------------------------------------------------------------------#
# Returns the average value at the current lambda or NA if not able    #
# to obtain a valid solution.                                          #
#----------------------------------------------------------------------#
# optimFunc : character name of the function being optimized           #
# valueFunc : character name of the function for determining value     #
# lambda    : parameter value                                          #
# samplen   : integer vector of indices for sample                     #
# folds     : integer number of cross-validation steps                 #
# x         : matrix of covariates and treatment histories             #
# response  : vector of response                                       #
# prWgt     : Weights defined as propensity for tx received            #
# txVec     : vector of treatment coded as +/- 1                       #
# suppress  : T/F indicating of prints to screen are executed          #
# ...       : additional parameters passed to optimFunc                #
#----------------------------------------------------------------------#
.eachLambda <- function(optimFunc,
                        valueFunc,
                        lambda, 
                        samplen,  
                        folds,  
                        x,
                        response,
                        prWgt,
                        txVec,
                        suppress, ...) {

  #------------------------------------------------------------------#
  # Initialize value to zero.                                        #
  #------------------------------------------------------------------#
  value <- 0.0

  #------------------------------------------------------------------#
  # Number of records.                                               #
  #------------------------------------------------------------------#
  n <- length(samplen)

  i <- 1L
  fold_cnt <- 0L

  argList <- list(...)

  if( !("guess" %in% names(argList)) ) {
    guess <- NULL
  } else {
    guess <- argList$guess
  }

  #------------------------------------------------------------------#
  # Cycle through folds of cross-validation.                         #
  #------------------------------------------------------------------#
  while( i <= folds ) {

    if( !suppress ) {
      cat("Fold", i, "of", folds, "\n")
    }

    #--------------------------------------------------------------#
    # Separate data set into test and train sets.                  #
    #--------------------------------------------------------------#
    tst_idx <- samplen[seq(i, n, by = folds)]
    trn_idx <- setdiff(1L:n, tst_idx)

    #--------------------------------------------------------------#
    # Perform training step.                                       #
    #--------------------------------------------------------------#
    argList <- list(...)

    argList[[ "subset" ]] <- trn_idx
    argList[[ "x"      ]] <- x
    argList[[ "lambda" ]] <- lambda
    argList[[ "txVec"  ]] <- txVec
    argList[[ "prWgt"  ]] <- prWgt
    argList[[ "response" ]] <- response
    argList[[ "suppress" ]] <- suppress
    if( !is(guess,"NULL") ) argList[[ "guess" ]] <- guess


    train <- do.call(optimFunc, argList)

    #--------------------------------------------------------------#
    # increment the fold count                                     #
    #--------------------------------------------------------------#
    i <- i + 1L

    #--------------------------------------------------------------#
    # If there was an error, cycle to next fold                    #
    #--------------------------------------------------------------#
    if( is(train, "NULL") ) {
      if( !suppress ) {
        cat("Optimization not successful.\n")
      }
      next
    }

    guess <- unname(regimeCoef(train))

    #--------------------------------------------------------------#
    # increment the number of successful folds                     #
    #--------------------------------------------------------------#
    fold_cnt <- fold_cnt + 1L

    #--------------------------------------------------------------#
    # Perform prediction step on test set.                         #
    # Prediction method returns a vector of optimal txs coded as   #
    # -1.0/1.0                                                     #
    #--------------------------------------------------------------#
    optVec <- .predictOptimalTx(x = train, newdata = x)$optimalTx

    ind <- {{optVec < 0} & {txVec < 0.0}} |
           {{optVec > 0} & {txVec > 0.0}}

    #--------------------------------------------------------------#
    # If any received the optimal treatment, add their contribution#
    # to the value equation.                                       #
    #--------------------------------------------------------------#
    if( any(ind) ) {

      argList <- list(...)
      if( "mu" %in% names(argList) ) {
        argList <- list("mu" = argList[["mu"]])
      } else {
        argList <- list()
      }

      argList[[ "subset" ]] <- tst_idx
      argList[[ "optTx" ]] <- optVec
      argList[[ "txVec" ]] <- txVec
      argList[[ "prWgt" ]] <- prWgt
      argList[[ "response" ]] <- response

      val <- do.call(valueFunc, argList)

      value <- value + val
               
      if( !suppress ) {
        cat("value:", val, "\n")
      }
    }
  }

  if( fold_cnt > 0.5 ) {
    #--------------------------------------------------------------#
    # average value over successful folds                          #
    #--------------------------------------------------------------#
    value <- value / fold_cnt
    if( !suppress ) {
      cat("Average value over successful folds:", value, "\n")
    }
  } else {
    value <- NA
  }

  return(value)

}

