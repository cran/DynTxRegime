# October 26, 2018
#
#' Efficient Augmentation and Relaxation Learning
#'
#' @references Ying-Qi Zhao, Eric Laber, Sumona Saha and Bruce E. Sands
#'    (2016+)
#'    Efficient augmentation and relaxation learning for treatment
#'    regimes using observational data
#'
#' @param ... Used primarily to require named input. However, inputs for
#'   the optimization methods can be sent through the ellipsis. If surrogate
#'   is hinge, the optimization method is dfoptim::hjk(). For all other 
#'   surrogates, stats::optim() is used.    
#' @param moPropen An object of class modelObj or modelObjSubset, which 
#'   defines the model and
#'   R methods to be used to obtain parameter estimates and
#'   predictions for the propensity for treatment.
#'   See ?moPropen for details.
#' @param moMain An object of class modelObj or modelObjSubset, which 
#'   defines the model and
#'   R methods to be used to obtain parameter estimates and
#'   predictions for the main effects of the outcome.
#'   See ?modelObj for details.
#' @param moCont An object of class modelObj or modelObjSubset, which 
#'   defines the model and
#'   R methods to be used to obtain parameter estimates and
#'   predictions for the contrasts of the outcome.
#'   See ?modelObj for details.
#' @param data A data frame of the covariates and tx histories
#' @param response The response variable.
#' @param txName A character object.
#'   The column header of \emph{data} that corresponds to the tx covariate
#' @param regime A formula object or a list of formula objects.
#'   The covariates to be included in classification. If a list is provided,
#'   this specifies that there is an underlying subset structure -- fSet must
#'   then be defined.
#' @param iter Maximum number of iterations for outcome regression
#' @param fSet A function or NULL defining subset structure
#' @param lambdas A numeric object or a numeric vector object giving the
#'   penalty tuning parameter. If more than 1 is provided,
#'   the finite set of values to be considered in the
#'   cross-validation algorithm
#' @param cvFolds If cross-validation is to be used to select the tuning
#'   parameters, the number of folds.
#' @param surrogate The surrogate 0-1 loss function must be one of
#'   logit, exp, hinge, sqhinge, huber
#' @param kernel A character object.
#'   must be one of {linear, poly, radial}
#' @param kparam A numeric object of NULL.
#'   If kernel = linear, kparam is ignored.
#'   If kernel = poly, kparam is the degree of the polynomial
#'   If kernel = radial, kparam is the inverse bandwidth of the
#'   kernel. If a vector of bandwidth parameters is given,
#'   cross-validation will be used to select the parameter
#' @param verbose An integer or logical. If 0, no screen prints are generated. If 1,
#'   screen prints are generated with the exception of optimization results
#'   obtained in iterative algorithm. If 2, all screen prints are generated.
#'
#' @return an EARL object
#'
#' @family statistical methods
#' @family single decision point methods
#' @family weighted learning methods
#'
#' @include S_class_EARL.R
#'
#' @examples
#' 
#' # Load and process data set
#' data(bmiData)
#' 
#' # define the negative 12 month change in BMI from baseline
#' y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]
#' 
#' # propensity model
#' moPropen <- buildModelObj(model = ~parentBMI+month4BMI,
#'                           solver.method = 'glm',
#'                           solver.args = list('family'='binomial'),
#'                           predict.method = 'predict.glm',
#'                           predict.args = list(type='response'))
#' 
#' # outcome model
#' moMain <- buildModelObj(model = ~parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~parentBMI+month4BMI,
#'                         solver.method = 'lm')
#'
#' fitEARL <- earl(moPropen = moPropen, moMain = moMain, moCont = moCont,
#'               data = bmiData, response = y12,  txName = 'A2', 
#'               regime = ~ parentBMI + month4BMI,
#'               surrogate = 'logit', kernel = 'poly', kparam = 2)
#' 
#' ##Available methods
#' 
#'   # Coefficients of the regression objects
#'   coef(fitEARL)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(fitEARL)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(fitEARL)
#' 
#'   # Value object returned by regression methods
#'   fitObject(fitEARL)
#' 
#'   # Summary of optimization routine
#'   optimObj(fitEARL)
#' 
#'   # Estimated optimal treatment for training data
#'   optTx(fitEARL)
#' 
#'   # Estimated optimal treatment for new data
#'   optTx(fitEARL, bmiData)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(fitEARL)
#'
#'   # Plots if defined by regression methods
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(fitEARL)
#'   plot(fitEARL, suppress = TRUE)
#' 
#'   # Value object returned by propensity score regression method
#'   propen(fitEARL)
#' 
#'   # Parameter estimates for decision function
#'   regimeCoef(fitEARL)
#' 
#'   # Show main results of method
#'   show(fitEARL)
#' 
#'   # Show summary results of method
#'   summary(fitEARL)
#'  
#' @export
earl <- function(...,
                 moPropen,
                 moMain,
                 moCont,
                 data,
                 response,
                 txName,
                 regime, 
                 iter = 0L,
                 fSet = NULL,
                 lambdas = 0.5,
                 cvFolds = 0L,
                 surrogate = "hinge",
                 kernel = "linear",
                 kparam = NULL,
                 verbose = 2L) {

  # verify moPropen provided and is modelObj
  if (missing(x = moPropen)) moPropen <- NULL
  if (is.null(x = moPropen)) stop("moPropen must be provided")
  moPropen <- .checkModelObjOrListModelObjSubset(object = moPropen, 
                                                 nm = 'moPropen')

  # if subset structure specified in moPropen, ensure fSet is a function
  if (is(object = moPropen, class2 = "ModelObj_SubsetList")) {
    if (is.null(x = fSet)) {
      stop("if subset structure in moPropen, fSet must be provided.")
    }
  }


  # verify moMain provided and is modelObj
  if (missing(x = moMain)) moMain <- NULL
  moMain <- .checkModelObjOrListModelObjSubset(object = moMain, nm = 'moMain')

  # if subset structure specified in moMain, ensure fSet is a function
  if (is(object = moMain, class2 = "ModelObj_SubsetList")) {
    if (is.null(x = fSet)) {
      stop("if subset structure in moMain, fSet must be provided.")
    }
  }

  # verify moCont provided and is modelObj
  if (missing(x = moCont)) moCont <- NULL
  moCont <- .checkModelObjOrListModelObjSubset(object = moCont, nm = 'moCont')

  # if subset structure specified in moCont, ensure fSet is a function
  if (is(object = moCont, class2 = "ModelObj_SubsetList")) {
    if (is.null(x = fSet)) {
      stop("if subset structure in moCont, fSet must be provided.")
    }
  }

  # if both moCont and moMain are provided, must both be of same class
  if (is.null(x = moMain) || is.null(x = moCont)) {
    iter <- NULL
  } else {
    if (is(object = moCont, class2 = "ModelObj_SubsetList") && 
        !is(object = moMain, class2 = "ModelObj_SubsetList")) {
      stop("moMain and moCont must both be ModelObjSubset or both be modelObj")
    }

    if (is(object = moCont, class2 = "modelObj") && 
        !is(object = moMain, class2 = "modelObj")) {
      stop("moMain and moCont must both be ModelObjSubset or both be modelObj")
    }
  } 

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # response must be a vector
  response <- .verifyVectorResponse(response = response)

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # regime must be formula or a list of formula
  regime <- .verifyRegime(regime = regime, fSet = fSet)

  if (is.list(x = kernel)) {

    if (!is.function(x = fSet)) {
      stop("fSet must be a function when using multiple kernels")
    }

    if (any(is.null(x = names(x = kernel)))) {
      stop("if multiple kernels, kernel must be a named list")
    }

    if (any(nchar(x = names(x = kernel)) == 0L)) {
      stop("if multiple kernels, kernel must be a named list")
    }

    if (is.list(x = kparam)) {
      if (!all(names(x = kparam) %in% names(x = kernel)) |
          !all(names(x = kernel) %in% names(x = kparam))) {
        stop("names of kernel and kparam list elements do not match")
      }
    }

    if (!is.list(x = regime)) {
      stop("a list of regimes is required for multiple kernels")
    }

    if (!all(names(x = regime) %in% names(x = kernel)) |
        !all(names(x = kernel) %in% names(x = regime))) {
      stop("names of kernel and regime list elements do not match")
    }

    kernelObj <- list()
    cvHold <- NULL

    for (i in 1L:length(x = kernel)) {
      kname <- names(x = kernel)[i]

      # verify cross-validation quantities
      cvVerified <- .verifyCV(lambdas = lambdas, 
                              cvFolds = cvFolds,  
                              kparam = kparam[[ kname ]])

      if (!is.null(x = cvVerified$cvFolds)) {
        cvHold <- cvVerified$cvFolds
      }

      # define kernel
      kernelObj[[ kname ]] <- .newKernelObj(kernel = tolower(kernel[[ i ]]),
                                            data = data,
                                            model = regime[[ kname ]],
                                            kparam = cvVerified$kparam)@kernel
    }
    kernelObj <- new("SubsetList", kernelObj)
    cvVerified$cvFolds <- cvVerified$cvFolds
  } else {
    # verify cross-validation quantities
    cvVerified <- .verifyCV(lambdas = lambdas, 
                            cvFolds = cvFolds,  
                            kparam = kparam)

    # define kernel
    kernelObj <- .newKernelObj(kernel = tolower(kernel),
                               data = data,
                               model = regime,
                               kparam = cvVerified$kparam)@kernel

  }

  # fSet must be NULL or a function.
  fSet <- .verifyFSet(fSet = fSet)

  # define surrogate
  surrogate <- .verifySurrogate(surrogate = surrogate)
    
  # iter must be a positive integer or NULL
  iter <- .verifyIter(iter = iter)

  # verbose must 0, 1, 2
  if (is.logical(x = verbose)) verbose <- verbose*1L
  verbose <- as.integer(x = round(x = verbose))
  if (verbose > 2L) verbose <- 2L
  if (verbose < 0L) verbose <- 0L

  result <- .newEARL(moPropen = moPropen,  
                     moMain = moMain,
                     moCont = moCont,
                     data = data, 
                     response = response,  
                     txName = txName,  
                     lambdas = cvVerified$lambdas,
                     cvFolds = cvVerified$cvFolds,
                     surrogate = surrogate,
                     iter = iter,
                     guess = NULL,
                     kernel = kernelObj,
                     fSet = fSet,
                     suppress = verbose, ...)

  result@analysis@call <- match.call()

  return( result )

}
