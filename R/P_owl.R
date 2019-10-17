# October 26, 2018
#
#' Outcome Weighted Learning
#'
#' @references Yingqi Zhao, Donglin Zeng, A. John Rush,
#'   Michael R. Kosorok (2012)
#'  Estimated individualized treatment rules using outcome weighted
#'  learning. Journal of the American Statistical Association,
#'  107(409): 1106-1118. PMCID: 3636816
#'
#' @param ... Used primarily to require named input. However, inputs for
#'   the optimization methods can be sent through the ellipsis. If surrogate
#'   is hinge, the optimization method is kernlab::ipop(). For all other 
#'   surrogates, stats::optim() is used.    
#' @param moPropen An object of class modelObj, which defines the model and
#'   R methods to be used to obtain parameter estimates and
#'   predictions for the propensity for treatment.
#'   See ?moPropen for details.
#' @param data A data frame of the covariates and tx histories
#' @param reward The response vector
#' @param txName A character object.
#'   The column header of \emph{data} that corresponds to the tx covariate
#' @param regime A formula object or a character vector.
#'   The covariates to be included in classification
#' @param response A numeric vector.
#'   The reward. Allows for naming convention followed in most
#'   DynTxRegime methods.
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
#' @param surrogate The surrogate 0-1 loss function must be one of
#'   logit, exp, hinge, sqhinge, huber
#' @param verbose An integer or logical. If 0, no screen prints are generated. If 1,
#'   screen prints are generated with the exception of optimization results
#'   obtained in iterative algorithm. If 2, all screen prints are generated.
#'
#' @return an OWL object
#'
#' @include P_class_.owl.R P_class_OWL.R
#'
#' @family statistical methods
#' @family weighted learning methods
#' @family single decision point methods
#'
#' @export
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
#' fitOWL <- owl(moPropen = moPropen,
#'               data = bmiData, reward = y12,  txName = 'A2', 
#'               regime = ~ parentBMI + month4BMI,
#'               surrogate = 'hinge', kernel = 'linear', kparam = NULL)
#' 
#' ##Available methods
#' 
#'   # Coefficients of the propensity score regression
#'   coef(fitOWL)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(fitOWL)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(fitOWL)
#' 
#'   # Value object returned by propensity score regression method
#'   fitObject(fitOWL)
#' 
#'   # Summary of optimization routine
#'   optimObj(fitOWL)
#' 
#'   # Estimated optimal treatment for training data
#'   optTx(fitOWL)
#' 
#'   # Estimated optimal treatment for new data
#'   optTx(fitOWL, bmiData)
#' 
#'   # Plots if defined by propensity regression method
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(fitOWL)
#'   plot(fitOWL, suppress = TRUE)
#' 
#'   # Value object returned by propensity score regression method
#'   propen(fitOWL)
#' 
#'   # Parameter estimates for decision function
#'   regimeCoef(fitOWL)
#' 
#'   # Show main results of method
#'   show(fitOWL)
#' 
#'   # Show summary results of method
#'   summary(fitOWL)
#'  
owl <- function(...,
                moPropen,
                data,
                reward,
                txName,
                regime,
                response,
                lambdas = 2.0,
                cvFolds = 0L,
                kernel = "linear",
                kparam = NULL,
                surrogate = 'hinge',
                verbose = 2L){

  # verify moPropen provided and is modelObj
  if (missing(x = moPropen)) moPropen <- NULL
  if (is.null(x = moPropen)) stop("moPropen must be provided")
  if (!is(object = moPropen, class2 = "modelObj")) {
    stop("moPropen must be modelObj")
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # verify that a reward vector is provided.
  if (missing(x = response)) response <- NULL
  if (missing(x = reward)) reward <- NULL
  response <- .verifyResponseReward(response = response, reward = reward)

  # response must be vector
  response <- .verifyVectorResponse(response = response)
  response <- response + 1e-6

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # treatments must be binary
  txVec <- .checkBinaryTx(txName = txName, data = data)

  # verify cross-validation quantities
  cvVerified <- .verifyCV(lambdas = lambdas, cvFolds = cvFolds, kparam = kparam)

  # kernel model (regime) must be formula.
  if (!is(object = regime, class2 = "formula")) {
    stop("regime must be a formula")
  }

  kernelObj <- .newKernelObj(kernel = tolower(kernel),
                             data = data,
                             model = regime,
                             kparam = cvVerified$kparam)

  # verify surrogate information
  surrogate <- .verifySurrogate(surrogate = surrogate)

  # verbose must 0, 1, 2
  if (is.logical(verbose)) verbose <- verbose*1L
  verbose <- as.integer(x=round(x = verbose))
  if (verbose > 2L) verbose <- 2L
  if (verbose < 0L) verbose <- 0L

  result <- .newOWL(moPropen = moPropen,
                    data = data,
                    response = response,
                    txName = txName,
                    lambdas = cvVerified$lambdas,
                    cvFolds = cvVerified$cvFolds,
                    kernel = kernelObj@kernel,
                    fSet = NULL,
                    surrogate = surrogate,
                    suppress = verbose,
                    guess = NULL, ...)

  result@analysis@call <- match.call()

  return( result )

}
