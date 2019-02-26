# October 26, 2018
#
#' Residual Weighted Learning
#'
#' @references Xin Zhou, Nicole Mayer-Hamblett, Umer Khan, and Michael R Kosorok
#'    (2017)
#'    Residual weighted learning for estimating individualized
#'    treatment rules. Journal of the American Statistical Association,
#'    112, 169--187.
#'
#' @param ... Used primarily to require named input. However, inputs for
#'   the optimization methods can be sent through the ellipsis. The
#'   optimization method is stats::optim().
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
#' @param data A data frame of the covariates and tx histories
#' @param reward The response vector
#' @param txName A character object.
#'   The column header of \emph{data} that corresponds to the tx covariate
#' @param regime A formula object or a list of formula objects.
#'   The covariates to be included in classification. If a list is provided,
#'   this specifies that there is an underlying subset structure -- fSet must
#'   then be defined.
#' @param response A numeric vector.
#'   The reward. Allows for naming convention followed in most
#'   DynTxRegime methods.
#' @param fSet A function or NULL defining subset structure
#' @param lambdas A numeric object or a numeric vector object giving the
#'   penalty tuning parameter. If more than 1 is provided,
#'   the finite set of values to be considered in the
#'   cross-validation algorithm
#' @param cvFolds If cross-validation is to be used to select the tuning
#'   parameters, the number of folds.
#' @param kernel A character object.
#'   must be one of {linear, poly, radial}
#' @param kparam A numeric object of NULL.
#'   If kernel = linear, kparam is ignored.
#'   If kernel = poly, kparam is the degree of the polynomial
#'   If kernel = radial, kparam is the inverse bandwidth of the
#'   kernel. If a vector of bandwidth parameters is given,
#'   cross-validation will be used to select the parameter
#' @param responseType A character indicating if response is continuous, binary
#'   or count data.
#' @param verbose An integer or logical. If 0, no screen prints are generated. If 1,
#'   screen prints are generated with the exception of optimization results
#'   obtained in iterative algorithm. If 2, all screen prints are generated.
#'
#' @return an RWL object
#'
#' @include Q_class_.rwl.R Q_class_RWL.R
#'
#' @family statistical methods
#' @family weighted learning methods
#' @family single decision point methods
#'
#' @export
#' @examples
#' \dontrun{ 
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
#' fitRWL <- rwl(moPropen = moPropen, moMain = moMain,
#'               data = bmiData, reward = y12,  txName = 'A2', 
#'               regime = ~ parentBMI + month4BMI,
#'               kernel = 'radial', kparam = 1.5)
#' 
#' ##Available methods
#' 
#'   # Coefficients of the regression objects
#'   coef(fitRWL)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(fitRWL)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(fitRWL)
#' 
#'   # Value object returned by regression methods
#'   fitObject(fitRWL)
#' 
#'   # Summary of optimization routine
#'   optimObj(fitRWL)
#' 
#'   # Estimated optimal treatment for training data
#'   optTx(fitRWL)
#' 
#'   # Estimated optimal treatment for new data
#'   optTx(fitRWL, bmiData)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(fitRWL)
#'
#'   # Plots if defined by regression methods
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(fitRWL)
#'   plot(fitRWL, suppress = TRUE)
#' 
#'   # Value object returned by propensity score regression method
#'   propen(fitRWL)
#' 
#'   # Parameter estimates for decision function
#'   regimeCoef(fitRWL)
#' 
#'   # Show main results of method
#'   show(fitRWL)
#' 
#'   # Show summary results of method
#'   summary(fitRWL)
#'  }
rwl <- function(...,
                moPropen,  
                moMain,
                data, 
                reward,  
                txName,  
                regime, 
                response,
                fSet = NULL,
                lambdas = 2.0,
                cvFolds = 0L,
                kernel = "linear",
                kparam = NULL,
                responseType = "continuous",
                verbose = 2L) { 

  # verify moPropen provided and is modelObj
  if (missing(x = moPropen)) moPropen <- NULL
  if (is.null(x = moPropen)) stop("moPropen must be provided")
  moPropen <- .checkModelObjOrListModelObjSubset(object = moPropen, 
                                                 nm = 'moPropen')

  # if subset structure specified in moPropen, ensure fSet is a function
  if (is(object = moPropen, class2 = "ModelObj_SubsetList")) {
    if (is.null(x = fSet)) {
      stop("if subset structure in moPropen, fSet must be provided")
    }
  }

  # verify moMain provided and is modelObj
  if (missing(x = moMain)) moMain <- NULL
  if (is.null(x = moMain)) stop("moMain must be provided")
  moMain <- .checkModelObjOrListModelObjSubset(object = moMain, nm = 'moMain')

  # if subset structure specified in moMain, ensure fSet is a function
  if (is(object = moMain, class2 = "ModelObj_SubsetList")) {
    if (is.null(x = fSet)) {
      stop("if subset structure in moMain, fSet must be provided.")
    }
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # verify that a reward vector is provided.
  if (missing(x = response)) response <- NULL
  if (missing(x = reward)) reward <- NULL
  response <- .verifyResponseReward(response = response, reward = reward)

  # response must be vector
  response <- .verifyVectorResponse(response = response)

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # fSet must be NULL or a function.
  # fSet can be a function with non-list regime and modelObj moPropen when
  # single tx options exist for some patients.
  if (!is.function(x = fSet) && !is.null(x = fSet)) {
    stop("fSet must be NULL or a function")
  }

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

  # responseType must be an object of class character
  if (!is.character(x = responseType)) stop("'responseType' must be a character")

  responseType <- tolower(responseType)

  if (!(responseType %in% c("continuous", "binary", "count"))) {
    stop("'responseType' must be one of {continuous, binary, count}")
  }

  # verbose must 0, 1, 2
  if (is.logical(verbose)) verbose <- verbose*1L
  verbose <- as.integer(x=round(x = verbose))
  if (verbose > 2L) verbose <- 2L
  if (verbose < 0L) verbose <- 0L

  # only smooth ramp surrogate can be used for rwl
  surrogate <- new("SmoothRampSurrogate")

  result <- .newRWL(moPropen = moPropen,
                    moMain = moMain,
                    responseType = responseType,
                    data = data,
                    response = response,
                    txName = txName,
                    lambdas = cvVerified$lambdas,
                    cvFolds = cvVerified$cvFolds,
                    surrogate = surrogate,
                    guess = NULL,
                    kernel = kernelObj,
                    fSet = fSet,
                    suppress = verbose, ...)
    
  result@analysis@call <- match.call()

  return( result )

}
