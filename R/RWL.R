#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# RWL : Main function call for Residual Weighted Learning              #
#                                                                      #
#    Xin Zhou, Nicole Mayer-Hamblett, Umer Khan, and Michael R Kosork  #
#    (2016+)                                                           #
#    Residual weighted learning for estimating individualized          #
#    treatment rules. Journal of the American Statistical Association, #
#    in press.                                                         #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                      #
# ...     : ignored. Used to require names input.                      #
#                                                                      #
# moPropen: an object of class modelObj, which defines the model and   #
#           R methods to be used to obtain parameter estimates and     #
#           predictions for the propensity for treatment.              #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#                                                                      #
#           If the prediction method specified in moPropen returns     #
#           predictions for only a subset of the categorical tx data,  #
#           it is assumed that the base level defined by levels(tx)[1] #
#           is the missing category.                                   #
#                                                                      #
#           Note that it is assumed that the columns of the predictions#
#           are ordered in accordance with the vector returned by      #
#           levels().                                                  #
#                                                                      #
# moMain  : a single object of class modelObj or NULL                  #
#                                                                      #
#           Define the models and R methods to be used to obtain       #
#           parameter estimates and predictions for the main effects   #
#           component of the outcome regression.                       #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#                                                                      #
# data    : a data frame of the covariates and tx histories            #
#           tx variables will be recast as factors if not provided as  #
#           such.                                                      #
#                                                                      #
# reward  : response vector                                            #
#                                                                      #
# txName  : a character object.                                        #
#           The column header of \emph{data} that corresponds to the   #
#           tx covariate                                               #
#                                                                      #
# regime  : a formula object or a character vector.                    #
#           The covariates to be included in classification            #
#                                                                      #
# lambdas : A numeric object or a numeric vector object giving the     #
#           penalty tuning parameter. If more than 1 is provided,      #
#           the finite set of values to be considered in the           #
#           cross-validation algorithm                                 #
#                                                                      #
# cvFolds : If cross-validation is to be used to select the tuning     #
#           parameters, the number of folds.                           #
#                                                                      #
# kernel  : a character object.                                        #
#           must be one of {linear, poly, radial}                      #
#                                                                      #
# kparam  : a numeric object of NULL.                                  #
#           If kernel = linear, kparam is ignored.                     #
#           If kernel = poly, kparam is the degree of the polynomial   #
#           If kernel = radial, kparam is the inverse bandwidth of the #
#           kernel. If a vector of bandwidth parameters is given,      #
#           cross-validation will be used to select the parameter      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
rwl <- function(...,
                moPropen,  
                moMain,
                data, 
                reward,  
                txName,  
                regime, 
                lambdas = 2.0,
                cvFolds = 0L,
                kernel = "linear",
                kparam = NULL,
                responseType = "continuous",
                guess = NULL,
                verbose = TRUE) { 

  #------------------------------------------------------------------#
  # Verify that moPropen is given and is a modelObj.                 #
  #------------------------------------------------------------------#
  if( missing(moPropen) ) {
    UserError("input",
              "moPropen must be provided")
  }
  if( !is(moPropen,'modelObj') ) {
    UserError("input", "'moPropen' must be an object of class modelObj")
  }

  #------------------------------------------------------------------#
  # Verify that at least one modeling object is provided for outcome #
  #------------------------------------------------------------------#
  if( missing(moMain) ) moMain <- NULL
  if( is(moMain, "NULL") ) {
    UserError("input", "use owl() for outcome weighted learning")
  }

  #------------------------------------------------------------------#
  # Verify that if moMain is given it is a modelObj.                 #
  #------------------------------------------------------------------#
  if( !is(moMain,'modelObj') ){
    UserError("input", "'moMain' must be an object of class modelObj")
  }

  #------------------------------------------------------------------#
  # data must be provided as a data.frame object.                    #
  #------------------------------------------------------------------#
  if( !is(data, "data.frame") ) {
    UserError("input", "'data' must be a data.frame")
  }

  #------------------------------------------------------------------#
  # reward must be vector                                            #
  #------------------------------------------------------------------#
  if( is(reward, "data.frame") ) reward <- data.matrix(reward)
  if( is(reward, "matrix") ) {
    if( ncol(reward) != 1L ) {
      UserError("input", 
                "'reward' must be a vector")
    }
    reward <- drop(reward)
  }
  if( length(reward) != nrow(data) ){
    UserError("input", 
              "'reward' must be a vector")
  }

  #------------------------------------------------------------------#
  # Verify treatment is appropriately coded.                         #
  #------------------------------------------------------------------#
  data <- .checkTxData(txName, data)

  #------------------------------------------------------------------#
  # Treatment vector coded as -1.0/1.0                               #
  #------------------------------------------------------------------#
  txVec <- .checkBinaryTx(txName, data)
  if( !isTRUE(all.equal(txVec, data[,txName])) ) {
    cat("Treatment variable converted to {-1,1}\n")
    data[,txName] <- as.integer(round(txVec,0))
  }

  #------------------------------------------------------------------#
  # regime must be a formula.                                        #
  #------------------------------------------------------------------#
  if( !is(regime, "formula") ) {
    UserError("input", "'regime' must be a formula")
  }

  #------------------------------------------------------------------#
  # lambdas must be numeric.                                         #
  #------------------------------------------------------------------#
  if( !is(lambdas, "numeric") ) {
      UserError("input",
                "'lambdas' must be a numeric")
  }

  #------------------------------------------------------------------#
  # Verify kernel information                                        #
  #------------------------------------------------------------------#
  kernel <- tolower(kernel)
  if( missing(kparam) ) kparam <- NULL

  kparam <- .checkKernel(kernel, kparam, cvFolds)

  if( !is(cvFolds, "integer") ) cvFolds <- as.integer(round(cvFolds,0L))

  #------------------------------------------------------------------#
  # If not using cross-validation to estimate lambda, verify that    #
  # only one lambda value is given. If more than 1, ignore all but   #
  # the first element.                                               #
  #------------------------------------------------------------------#
  if( is(cvFolds, "NULL") || {cvFolds < 1L} ) {
    cvFolds <- 0L
    if( length(lambdas) > 1L ) {
      warning("only first lambda value considered")
      lambdas <- lambdas[1L]
    }
    if( length(kparam) > 1L ) {
      warning("only first kparam value considered")
      kparam <- kparam[1L]
    }
  }

  if( {cvFolds > 0L} && 
      {length(lambdas) == 1L} && 
      {length(kparam) == 1L} ) {
    cvFolds <- 0L
    warning(paste("cross-validation not performed;",
                  "only one pair of tuning parameters provided"))
  }
    
  #------------------------------------------------------------------#
  # txName must be an object of class character                      #
  #------------------------------------------------------------------#
  if( !is(responseType, "character") ) {
    UserError("input",
              "'responseType' must be a character")
  }

  responseType <- tolower(responseType)

  if( !(responseType %in% c("continuous", "binary", "count")) ) {
    UserError("input",
              "'responseType' must be one of {continuous, binary, count}")
  }

  if( !is(verbose, "logical") ) {
    UserError("input",
              "'verbose' must be a TRUE/FALSE")
  }

  result <- .newRWL(moPropen = moPropen,
                    moMain = moMain,
                    data = data,
                    response = reward,
                    txName = txName,
                    regime = regime,
                    lambdas = lambdas,
                    cvFolds = cvFolds,
                    kernel = kernel,
                    kparam = kparam,
                    txVec = txVec,
                    responseType = responseType,
                    guess = guess,
                    suppress = !verbose)
    
  result@call <- match.call()

  return(result)

}
