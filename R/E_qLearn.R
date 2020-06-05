# May 31, 2020
#
#' A Step of the Q-Learning Algorithm
#' 
#'  Performs a single step of the Q-Learning algorithm.
#'    If an object of class \code{QLearn} is passed through input response, 
#'    it is assumed that the \code{QLearn} object is the value object returned
#'    from the preceding step of the Q-Learning algorithm, and
#'    the value fit by the regression is taken from the \code{QLearn} object.
#'    If a vector is passed through input response, it is assumed that the
#'    call if for the first step in the Q-Learning algorithm, and 
#'    models are fit using the provided response.
#'
#'
#' @name qLearn
#'
#' @param ... ignored. Provided to require named inputs.
#'
#' @param moMain  An object of class modelObj or a list of objects of class
#'   modelObjSubset, which define the models and R methods to be used to
#'   obtain parameter estimates and predictions for the main effects component 
#'   of the outcome regression. \cr
#'   See ?modelObj and/or ?modelObjSubset for details. \cr
#'   NULL is an acceptable value if moCont is defined.
#'
#' @param moCont An object of class modelObj or a list of objects of class
#'   modelObjSubset, which define the models and R methods to be used to 
#'   obtain parameter estimates and predictions for the contrasts component 
#'   of the outcome regression. \cr
#'   See ?modelObj and/or ?modelObjSubset for details. \cr
#'   NULL is an acceptable value if moMain is defined.
#'
#' @param data A data frame of covariates and treatment history.
#'
#' @param response A response vector or object of class QLearn from a previous
#'   Q-Learning step.
#'
#' @param txName A character string giving column header of treatment variable
#'   in data
#'
#' @param fSet NULL or a function. This argument allows the user to specify 
#'   the subset of treatment options available to a patient. 
#'   See ?fSet for details of allowed structure
#'
#' @param iter An integer. See ?iter for details
#'
#' @param verbose A logical. If TRUE, screen prints are generated.
#'
#' @return An object of class \link{QLearn-class}
#'
#' @family statistical methods
#' @family multiple decision point methods
#' @family single decision point methods
#'
#' @include E_class_QLearn.R
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
#' # outcome model
#' moMain <- buildModelObj(model = ~parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' #### Second-Stage Analysis
#' fitSS <- qLearn(moMain = moMain, moCont = moCont,
#'                 data = bmiData, response = y12,  txName = 'A2')
#' 
#' ##Available methods
#' 
#'   # Coefficients of the outcome regression objects
#'   coef(fitSS)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(fitSS)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(fitSS)
#' 
#'   # Value object returned by outcome regression method
#'   fitObject(fitSS)
#' 
#'   # Estimated optimal treatment and decision functions for training data
#'   optTx(fitSS)
#' 
#'   # Estimated optimal treatment and decision functions for new data
#'   optTx(fitSS, bmiData)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(fitSS)
#'
#'   # Plots if defined by outcome regression method
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(fitSS)
#'   plot(fitSS, suppress = TRUE)
#' 
#'   # Show main results of method
#'   show(fitSS)
#' 
#'   # Show summary results of method
#'   summary(fitSS)
#'  
#' #### First-stage Analysis
#' 
#' # outcome model
#' moMain <- buildModelObj(model = ~parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' fitFS <- qLearn(moMain = moMain, moCont = moCont,
#'                 data = bmiData, response = fitSS,  txName = 'A1')
#' 
#' ##Available methods for fitFS are as shown above for fitSS
#'
qLearn <- function(...,
                   moMain,
                   moCont, 
                   data, 
                   response, 
                   txName, 
                   fSet = NULL, 
                   iter = 0L,
                   verbose = TRUE) {

  # verify moMain is NULL, modelObj, or list of ModelObjSubset
  if (missing(x = moMain)) moMain <- NULL
  moMain <- .checkModelObjOrListModelObjSubset(object = moMain, nm = 'moMain')

  # verify moCont is NULL, modelObj, or list of ModelObjSubset
  if (missing(x = moCont)) moCont <- NULL
  moCont <- .checkModelObjOrListModelObjSubset(object = moCont, nm = 'moCont')

  # if both moCont and moMain are provided, must both be of same class
  if (is.null(x = moMain) && is.null(x = moCont)) {
    stop("must provide moMain and/or moCont")
  } else if (is.null(x = moMain) || is.null(x = moCont)) {
    iter <- NULL
  } else {
    if (is(object = moCont, class2 = "ModelObj_SubsetList") && 
        !is(object = moMain, class2 = "ModelObj_SubsetList")) {
      stop("moMain and moCont must both be ModelObjSubset")
    }

    if (is(object = moCont, class2 = "modelObj") && 
        !is(object = moMain, class2 = "modelObj")) {
      stop("moMain and moCont must both be modelObj")
    }
  } 

  # if subset structure specified in moMain, ensure fSet is a function
  if (is(object = moMain, class2 = "ModelObj_SubsetList") ||
      is(object = moCont, class2 = "ModelObj_SubsetList")) {
    if (!is.function(x = fSet)) {
      stop("if subset structure in moMain/moCont, fSet must be provided")
    }
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # response must be QLearn or vector
  if (!is(object = response, class2 = "QLearn")) {
    response <- .verifyVectorResponse(response = response)
  }

  if (!is(object = response, class2 = "QLearn") && !is.vector(x = response)) {
    stop(paste0("response must be a vector of responses or ",
                "an object returned by a prior call to qLearn()"))
  }

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # iter must be a positive integer or NULL
  iter <- .verifyIter(iter = iter)

  # fSet must be NULL or a function.
  fSet <- .verifyFSet(fSet = fSet)

  # verbose must be logical
  verbose <- .verifyVerbose(verbose = verbose)

  result <- .newQLearn(moMain = moMain,
                       moCont = moCont,
                       fSet = fSet,
                       response = response,
                       data = data,
                       txName = txName,
                       iter = iter,
                       suppress = !verbose)

  result@analysis@call <- match.call()

  return( result )
}
