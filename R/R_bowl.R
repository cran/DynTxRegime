# October 26, 2018
#
#' Backwards Outcome Weighted Learning. 
#'
#' Function performs a single step
#'   of the bowl method. Multiple decision points can be analyzed by
#'   repeated calls, as is done for qLearn() and optimalClass().
#'
#' @references Yingqi Zhao, Donglin Zeng, Eric B. Laber, Michael R. Kosorok
#'    (2015)
#'    New statistical learning methods for estimating optimal dynamic
#'    treatment regimes.
#'    Journal of the American Statistical Association,
#'    110:510, 583--598.
#'
#' @param ... Used primarily to require named input. However, inputs for
#'   the optimization methods can be sent through the ellipsis. If surrogate
#'   is hinge, the optimization method is dfoptim::hjk(). For all other 
#'   surrogates, stats::optim() is used.    
#' @param moPropen An object of class modelObj or modelObjSubset, which 
#'   defines the model and
#'   R methods to be used to obtain parameter estimates and
#'   predictions for the propensity for tx.
#'   See ?moPropen for details.
#' @param data A data frame of the covariates and tx histories.
#' @param reward The response vector.
#' @param txName A character object.
#'   The column header of \emph{data} that corresponds to the tx covariate
#' @param regime A formula object or a list of formula objects.
#'   The covariates to be included in the decision function/kernel. 
#'   If a list is provided,
#'   this specifies that there is an underlying subset structure -- fSet must
#'   then be defined. For subsets, the name of each element of the list must 
#'   correspond to the name of a subset. If a regime is to be estimated using
#'   multiple subsets combined, each subset must be included in the name and
#'   separated by a comma (no spaces). 
#' @param response A numeric vector.
#'   The same as reward above. Allows for naming convention followed in most
#'   DynTxRegime methods.
#' @param BOWLObj NULL or \code{\link{BOWL-class}} object returned from 
#'   previous call to bowl(). If NULL, indicates that the function call is 
#'   for the first STEP of the BOWL algorithm (i.e., the final decision point). 
#'   If a \code{\link{BOWL-class}} object, assumed that the object was 
#'   returned by the preceding step of the BOWL algorithm.
#' @param lambdas A numeric object or a numeric vector object giving the
#'   penalty tuning parameter(s). If more than 1 is provided,
#'   the set of tuning parameter values to be considered in the
#'   cross-validation algorithm (note that cvFolds must be positive in this case).
#' @param cvFolds If cross-validation is to be used to select the tuning
#'   parameters and/or kernel parameters, the number of folds.
#' @param kernel A character object.
#'   Must be one of \{'linear', 'poly', 'radial'\}
#' @param kparam A numeric object. \cr
#'   If kernel = linear, kparam is ignored. \cr
#'   If kernel = poly, kparam is the degree of the polynomial. \cr
#'   If kernel = radial, kparam is the inverse bandwidth of the
#'   kernel. If a vector of bandwidth parameters is given,
#'   cross-validation will be used to select the parameter
#'   (note that cvFolds must be positive in this case).
#' @param fSet A function or NULL defining subset structure. See ?fSet for
#'   details.
#' @param surrogate The surrogate 0-1 loss function. Must be one of
#'   \{'logit', 'exp', 'hinge', 'sqhinge', 'huber'\}.
#' @param verbose An integer or logical. If 0, no screen prints are generated. If 1,
#'   screen prints are generated with the exception of optimization results
#'   obtained in iterative algorithm. If 2, all screen prints are generated.
#'
#' @return a \code{\link{BOWL-class}} object
#'
#' @include R_class_BOWL.R
#'
#' @family statistical methods
#' @family weighted learning methods
#' @family multiple decision point methods
#'
#' @export
#'
#' @examples
#'  
#' # Load and process data set
#' data(bmiData)
#' 
#' # define the negative 12 month change in BMI from baseline
#' y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]
#' 
#' # define the negative 4 month change in BMI from baseline
#' y4 <- -100*(bmiData[,5L] - bmiData[,4L])/bmiData[,4L]
#' 
#' # reward for second stage
#' rewardSS <- y12 - y4
#' 
#' #### Second-stage regression
#' 
#' # Constant propensity model
#' moPropen <- buildModelObj(model = ~1,
#'                           solver.method = 'glm',
#'                           solver.args = list('family'='binomial'),
#'                           predict.method = 'predict.glm',
#'                           predict.args = list(type='response'))
#' 
#' fitSS <- bowl(moPropen = moPropen,
#'               data = bmiData, reward = rewardSS,  txName = 'A2', 
#'               regime = ~ parentBMI + month4BMI)
#' 
#' ##Available methods
#' 
#'   # Coefficients of the propensity score regression
#'   coef(fitSS)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(fitSS)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(fitSS)
#' 
#'   # Value object returned by propensity score regression method
#'   fitObject(fitSS)
#' 
#'   # Summary of optimization routine
#'   optimObj(fitSS)
#' 
#'   # Estimated optimal treatment for training data
#'   optTx(fitSS)
#' 
#'   # Estimated optimal treatment for new data
#'   optTx(fitSS, bmiData)
#' 
#'   # Plots if defined by propensity regression method
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(fitSS)
#'   plot(fitSS, suppress = TRUE)
#' 
#'   # Value object returned by propensity score regression method
#'   propen(fitSS)
#' 
#'   # Parameter estimates for decision function
#'   regimeCoef(fitSS)
#' 
#'   # Show main results of method
#'   show(fitSS)
#' 
#'   # Show summary results of method
#'   summary(fitSS)
#'  
#' #### First-stage regression
#' 
#' # Constant propensity model
#' fitFS <- bowl(moPropen = moPropen,
#'               data = bmiData, reward = y4,  txName = 'A1', 
#'               regime = ~ gender + parentBMI,
#'               BOWLObj = fitSS, lambdas = c(0.5, 1.0), cvFolds = 4L)
#' 
#' ##Available methods for fitFS are as shown above for fitSS
#'
#'   # Results of the cross-validation
#'   cvInfo(fitFS)
#' 
bowl <- function(...,
                 moPropen,  
                 data, 
                 reward,  
                 txName,
                 regime,
                 response,
                 BOWLObj = NULL,
                 lambdas = 2.0,
                 cvFolds = 0L,
                 kernel = "linear",
                 kparam = NULL,
                 fSet = NULL,
                 surrogate = 'hinge',
                 verbose = 2L) {

  # verify moPropen provided and is modelObj or modelObjSubset
  if (missing(x = moPropen)) moPropen <- NULL
  if (is.null(x = moPropen)) stop("moPropen must be provided")
  moPropen <- .checkModelObjOrListModelObjSubset(object = moPropen, 
                                                 nm = 'moPropen')

  # if subset structure specified in moPropen, ensure fSet is a function
  if (is(object = moPropen, class2 = "ModelObj_SubsetList")) {
    if (!is.function(x = fSet)) {
      stop("if subset structure in moPropen, fSet must be provided")
    }
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # verify that a reward vector is provided.
  if (missing(x = response)) response <- NULL
  if (missing(x = reward)) reward <- NULL
  response <- .verifyResponseReward(response = response, reward = reward)

  # response must be a vector
  response <- .verifyVectorResponse(response = response) + 1e-6

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # regime must be formula or a list of formula
  regime <- .verifyRegime(regime = regime, fSet = fSet)

  # if BOWLObj not provided default to NULL
  if (missing(x = BOWLObj)) BOWLObj <- NULL

  # BOWLObj must be BOWL or NULL
  if (!is(object = BOWLObj, class2 = "BOWL") && !is.null(x = BOWLObj)) {
    stop(paste("BOWLObj must be a BOWL object from a previous",
               "call or NULL if analysis is for the final",
               "decision point"))
  }

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
  # fSet can be a function with non-list regime and modelObj moPropen when
  # single tx options exist for some patients.
  if (!is.function(x = fSet) && !is.null(x = fSet)) {
    stop("fSet must be NULL or a function")
  }

  # define surrogate
  surrogate <- .verifySurrogate(surrogate = surrogate)

  # verbose must 0, 1, 2
  if (is.logical(x = verbose)) verbose <- verbose*1L
  verbose <- as.integer(x = round(x = verbose))
  if (verbose > 2L) verbose <- 2L
  if (verbose < 0L) verbose <- 0L

  obj <- .newBOWL(BOWLObj = BOWLObj,
                  moPropen = moPropen,
                  fSet = fSet,
                  data = data,
                  response = response,
                  txName = txName,
                  lambdas = cvVerified$lambdas,
                  cvFolds = cvVerified$cvFolds,
                  kernel = kernelObj,
                  surrogate = surrogate,
                  suppress = verbose,
                  guess = NULL, ...)

  obj@analysis@call <- match.call()

  return( obj )

}
