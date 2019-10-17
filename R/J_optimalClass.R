# October 26, 2018
#
#' Classification Perspective
#'
#' @name optimalClass
#'
#' @references Baqun Zhang, Anastasios A. Tsiatis, Marie Davidian, Min Zhang and
#'  Eric B. Laber. "Estimating optimal tx regimes from a classification
#'  perspective." Stat 2012; 1: 103-114. 
#' 
#' Note that this method is a single decision point, binary treatment
#' method. For multiple decision points, can be called repeatedly. 
#' 
#'
#' @param ... Included to require named inputs
#' @param moPropen An object of class modelObj, which defines the models and R
#'           methods to be used to obtain parameter estimates and
#'           predictions for the propensity for treatment. 
#'           See ?moPropen for details.
#' @param moMain An object of class modelObj, which defines the models and R
#'           methods to be used to obtain parameter estimates and
#'           predictions for for the main effects component of the 
#'           outcome regression. 
#'           See ?modelObj for details.
#'           NULL is an appropriate value. 
#' @param moCont An object of class modelObj, which defines the models and R
#'           methods to be used to obtain parameter estimates and
#'           predictions for for the contrasts component of the
#'           outcome regression. 
#'           See ?modelObj for details.
#'           NULL is an appropriate value. 
#' @param moClass An object of class modelObj, which defines the
#'           models and R methods to be used to obtain parameter 
#'           estimates and predictions for the classification. 
#'           See ?modelObj for details.
#' @param data A data frame of the covariates and tx histories 
#' @param response The response vector 
#' @param txName An character giving the column header of the column in data
#'           that contains the tx covariate. 
#' @param iter An integer
#'           See ?iter for details 
#' @param fSet A function or NULL. 
#'           This argument allows the user to specify the subset of tx 
#'           options available to a patient. 
#'           See ?fSet for details of allowed structure
#' @param verbose A logical 
#'           If FALSE, screen prints are suppressed. 
#' 
#' @return an object of class OptimalClass 
#'
#' @family statistical methods
#' @family single decision point methods
#' @family multiple decision point methods
#' @export
#'
#' @include J_class_OptimalClass.R
#' @examples
#' 
#' # Load and process data set
#' data(bmiData)
#' 
#' # define the negative 12 month change in BMI from baseline
#' y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]
#' 
#' # Define the propensity for treatment model and methods.
#' moPropen <- buildModelObj(model =  ~ 1, 
#'                           solver.method = 'glm', 
#'                           solver.args = list('family'='binomial'),
#'                           predict.method = 'predict.glm',
#'                           predict.args = list(type='response'))
#'
#' # classification model
#' library(rpart)
#' moClass <- buildModelObj(model = ~parentBMI+month4BMI+race+gender,
#'                          solver.method = 'rpart',
#'                          solver.args = list(method="class"),
#'                          predict.args = list(type='class'))
#'
#' #### Second-Stage Analysis using IPW
#' fitSS_IPW <- optimalClass(moPropen = moPropen, 
#'                           moClass = moClass,
#'                           data = bmiData, response = y12,  txName = 'A2')
#' 
#' # outcome model
#' moMain <- buildModelObj(model = ~parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' #### Second-Stage Analysis using AIPW
#' fitSS_AIPW <- optimalClass(moPropen = moPropen, 
#'                            moMain = moMain, moCont = moCont,
#'                            moClass = moClass,
#'                            data = bmiData, response = y12,  txName = 'A2')
#' 
#' ##Available methods
#' 
#'   # Retrieve the classification regression object
#'   classif(object = fitSS_AIPW)
#'
#'   # Coefficients of the outcome regression objects
#'   coef(object = fitSS_AIPW)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(object = fitSS_AIPW)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(x = fitSS_AIPW)
#' 
#'   # Value object returned by outcome regression method
#'   fitObject(object = fitSS_AIPW)
#' 
#'   # Estimated optimal treatment and decision functions for training data
#'   optTx(x = fitSS_AIPW)
#' 
#'   # Estimated optimal treatment and decision functions for new data
#'   optTx(x = fitSS_AIPW, newdata = bmiData)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(object = fitSS_AIPW)
#'   outcome(object = fitSS_IPW)
#'
#'   # Plots if defined by outcome regression method
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(x = fitSS_AIPW)
#'   plot(x = fitSS_AIPW, suppress = TRUE)
#' 
#'   # Retrieve the value object returned by propensity regression method
#'   propen(object = fitSS_AIPW)
#'
#'   # Show main results of method
#'   show(object = fitSS_AIPW)
#' 
#'   # Show summary results of method
#'   summary(object = fitSS_AIPW)
#'  
#' #### First-stage Analysis using AIPW
#' 
#'  # Define the propensity for treatment model and methods.
#'  moPropen <- buildModelObj(model =  ~ 1, 
#'                            solver.method = 'glm', 
#'                            solver.args = list('family'='binomial'),
#'                            predict.method = 'predict.glm',
#'                            predict.args = list(type='response'))
#'
#' # classification model
#' moClass <- buildModelObj(model = ~parentBMI+baselineBMI+race+gender,
#'                          solver.method = 'rpart',
#'                          solver.args = list(method="class"),
#'                          predict.args = list(type='class'))
#'
#' # outcome model
#' moMain <- buildModelObj(model = ~parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' fitFS_AIPW <- optimalClass(moPropen = moPropen, 
#'                            moMain = moMain, moCont = moCont,
#'                            moClass = moClass,
#'                            data = bmiData, response = fitSS_AIPW,  
#'                            txName = 'A1')
#' 
#' ##Available methods for fitFS_AIPW are as shown above for fitSS_AIPW
#'
optimalClass <- function(..., 
                         moPropen,
                         moMain,
                         moCont,
                         moClass,
                         data,
                         response,
                         txName,
                         iter = 0L,
                         fSet = NULL,
                         verbose = TRUE){

  # verify moPropen is modelObj or ModelObjSubset
  if (missing(x = moPropen)) moPropen <- NULL
  if (is.null(x = moPropen)) stop("moPropen must be provided")
  moPropen <- .checkModelObjOrListModelObjSubset(object = moPropen, 
                                                 nm = 'moPropen')

  # verify moMain is NULL, modelObj, or list of ModelObjSubset
  if (missing(x = moMain)) moMain <- NULL
  moMain <- .checkModelObjOrListModelObjSubset(object = moMain, nm = 'moMain')

  # verify moCont is NULL, modelObj, or list of ModelObjSubset
  if (missing(x = moCont)) moCont <- NULL
  moCont <- .checkModelObjOrListModelObjSubset(object = moCont, nm = 'moCont')

  # if both moCont and moMain are provided, must both be of same class
  if (is.null(x = moMain) && is.null(x = moCont)) {
    if (verbose) cat("IPW value estimator\n")
  } else if (is.null(x = moMain) || is.null(x = moCont)) {
    if (verbose) cat("AIPW value estimator\n")
    iter <- NULL
  } else {
    if (verbose) cat("AIPW value estimator\n")
    if ( is(object = moCont, class2 = "ModelObj_SubsetList") && 
        !is(object = moMain, class2 = "ModelObj_SubsetList")) {
      stop("moMain and moCont must be of same class")
    }

    if ( is(object = moCont, class2 = "modelObj") && 
        !is(object = moMain, class2 = "modelObj")) {
      stop("moMain and moCont must be of same class")
    }
  } 

  # verify moClass is modelObj or ModelObjSubset
  if (missing(x = moClass)) moClass <- NULL
  if (is.null(x = moClass)) stop("moClass must be provided")
  moClass <- .checkModelObjOrListModelObjSubset(object = moClass, 
                                                nm = 'moClass')

  if (is(object = moPropen, class2 = "ModelObj_SubsetList") ||
      is(object = moMain, class2 = "ModelObj_SubsetList") || 
      is(object = moCont, class2 = "ModelObj_SubsetList") || 
      is(object = moClass, class2 = "ModelObj_SubsetList")) {
    if (is.null(x = fSet)) {
      stop("fSet must be provided when subset modeling requested")
    }
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # response must be OptimalClass or vector
  if (!is(object = response, class2 = "OptimalClass")) {
    response <- .verifyVectorResponse(response = response)
  }

  if (!is(object = response, class2 = "OptimalClass") && 
      !is.vector(x = response)) {
    stop(paste0("response must be a vector of responses or ",
                "an object returned by a prior call to optimalClass()"))
  }

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # iter must be a positive integer or NULL
  iter <- .verifyIter(iter = iter)

  # fSet must be NULL or a function.
  fSet <- .verifyFSet(fSet = fSet)

  # verbose must be logical
  verbose <- .verifyVerbose(verbose = verbose)

  result <- .newOptimalClass(moPropen = moPropen,
                             moMain = moMain,
                             moCont = moCont,
                             moClass = moClass,
                             data = data,
                             response = response,
                             txName = txName,
                             iter = iter,
                             fSet = fSet,
                             suppress = !verbose)

  result@analysis@call <- match.call()

  return( result )

}
