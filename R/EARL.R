#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
# EARL : Main function call for Efficient augmentation and relaxation  #
#        learning for treatment regimes using observational data       #
#                                                                      #
#    Ying-Qi Zhao, Eric Laber, Sumona Saha and Bruce E. Sands          #
#    (2016+)                                                           #
#    Efficient augmentation and relaxation learning for treatment      #
#    regimes using observational data                                  #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
#                                                                      #
# ...      : ignored. Used to require names input.                     #
#                                                                      #
# moPropen : an object of class modelObj, which defines the model and  #
#            R methods to be used to obtain parameter estimates and    #
#            predictions for the propensity for treatment.             #
#            See ?modelObj and/or ?modelObjSubset for details.         #
#                                                                      #
#            If the prediction method specified in moPropen returns    #
#            predictions for only a subset of the categorical tx data, #
#            it is assumed that the base level defined by levels(tx)[1]#
#            is the missing category.                                  #
#                                                                      #
#            Note it is assumed that the columns of the predictions    #
#            are ordered in accordance with the vector returned by     #
#            levels().                                                 #
#                                                                      #
# moMain  : a single object of class modelObj or NULL                  #
#                                                                      #
#           Define the models and R methods to be used to obtain       #
#           parameter estimates and predictions for the main effects   #
#           component of the outcome regression.                       #
#           See ?modelObj and/or ?modelObjSubset for details.          #
#                                                                      #
# moCont   : a single object of class modelObj or NULL                 #
#                                                                      #
#            Define the models and R methods to be used to obtain      #
#            parameter estimates and predictions for the contrasts     #
#            component of the outcome regression.                      #
#            See ?modelObj and/or ?modelObjSubset for details.         #
#                                                                      #
# data     : a data frame of the covariates and tx histories           #
#            tx variables will be recast as factors if not provided as #
#            such.                                                     #
#                                                                      #
# response : response vector                                           #
#                                                                      #
# txName   : a character object.                                       #
#            The column header of \emph{data} that corresponds to the  #
#            tx covariate                                              #
#                                                                      #
# regime   : a formula object or a character vector.                   #
#            The covariates to be included in classification           #
#                                                                      #
# iter     : an integer                                                #
#                                                                      #
#            >=1 if moMain and moCont are to be fitted iteratively     #
#            The value is the maximum number of iterations.            #
#            Note the iterative algorithms is as follows:              #
#            Y = Ymain + Ycont                                         #
#             (1) hat(Ycont) = 0                                       #
#             (2) Ymain = Y - hat(Ycont)                               #
#             (3) fit Ymain ~ moMain                                   #
#             (4) set Ycont = Y - hat(Ymain)                           #
#             (5) fit Ycont ~ moCont                                   #
#             (6) Repeat steps (2) - (5) until convergence or          #
#             a maximum of iter iterations.                            #
#                                                                      #
#            <=0 moMain and moCont will be combined and fit as a       #
#            single object.                                            #
#                                                                      #
#            Note that if iter <= 0, all non-model components of the   #
#            moMain and moCont must be identical                       #
#                                                                      #
# lambdas  : A numeric object or a numeric vector object giving the    #
#            penalty tuning parameter. If more than 1 is provided,     #
#            the finite set of values to be considered in the          #
#            cross-validation algorithm                                #
#                                                                      #
# cvFolds  : If cross-validation is to be used to select the tuning    #
#            parameters, the number of folds.                          #
#                                                                      #
# surrogate: a character object.                                       #
#            must be one of {logit, exp, sqhinge, hinge}               #
#            convex surrogate of the indicator function.               #
#                                                                      #
#::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::#
earl <- function(...,
                 moPropen,
                 moMain,
                 moCont,
                 data,
                 response,
                 txName,
                 regime, 
                 iter = 0L,
                 lambdas = 0.5,
                 cvFolds = 0L,
                 surrogate = "hinge",
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
  # moMain must be either an object of class modelObj or NULL        #
  #------------------------------------------------------------------#
  if( missing(moMain) ) moMain <- NULL
  if( !is(moMain, 'modelObj') && !is(moMain,'NULL') ) {
    UserError("input",
              "'moMain' must be one of {modelObj, NULL}")
  }

  #------------------------------------------------------------------#
  # moCont must be either an object of class modelObj or NULL        #
  #------------------------------------------------------------------#
  if( missing(moCont) ) moCont <- NULL
  if( !is(moCont, 'modelObj') && !is(moCont,'NULL') ) {
    UserError("input",
              "'moCont' must be one of {modelObj, NULL}")
  }

  #------------------------------------------------------------------#
  # If either {moMain, moCont} is NULL, iterative algorithm is not   #
  # appropriate.                                                     #
  #------------------------------------------------------------------#
  if( is(moMain, "NULL") || is(moCont, "NULL") ) {
    iter = 0L
  }

  #------------------------------------------------------------------#
  # data must be an object of class data.frame                       #
  #------------------------------------------------------------------#
  if( !is(data, "data.frame") ) {
    UserError("input", "'data' must be a data.frame")
  }

  #------------------------------------------------------------------#
  # response must be an object of class vector                       #
  #------------------------------------------------------------------#
  if( is(response, "data.frame") ) response <- data.matrix(response)
  if( is(response, "matrix") ) {
    if( ncol(response) != 1L ) {
      UserError("input", 
                "'response' must be a vector")
    }
    response <- drop(response)
  }
  if( length(response) != nrow(data) ){
    UserError("input", 
              "'response' must be a vector")
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
  }

  #------------------------------------------------------------------#
  # Verify request for CV is appropriate.                            #
  #------------------------------------------------------------------#
  if( {cvFolds > 0L} && 
      {length(lambdas) == 1L} ) {
    cvFolds <- 0L
    warning(paste("cross-validation not performed;",
                  "only one tuning parameter provided"))
  }

  #------------------------------------------------------------------#
  # Verify surrogate selection.                                      #
  #------------------------------------------------------------------#
  surrogate <- tolower(surrogate)
  if( !(surrogate %in% c("hinge", "logit", "exp", "sqhinge")) ) {
    UserError("input",
              "surrogate must be one of {'hinge', 'logit', 'exp', 'sqhinge'}")
  }
    
  #------------------------------------------------------------------#
  # iter must be an integer                                          #
  #------------------------------------------------------------------#
  if( !is(iter, "integer") ) iter <- as.integer(round(iter,0L))

  if( !is(verbose, "logical") ) {
    UserError("input", "'verbose' must be a TRUE/FALSE")
  }

  result <- .newEARL(moPropen = moPropen,  
                     moMain = moMain,
                     moCont = moCont,
                     data = data, 
                     response = response,  
                     txName = txName,  
                     regime = regime, 
                     lambdas = lambdas,
                     cvFolds = cvFolds,
                     surrogate = surrogate,
                     iter = iter,
                     suppress = !verbose,
                     txVec = txVec,
                     guess = guess)

  result@call <- match.call()

  return(result)
}
