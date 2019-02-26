# October 11, 2018
#
#' Interactive Q-Learning
#' 
#'  The complete interactive Q-Learning algorithm.
#'
#' @name iqLearn
#'
#' @param ... ignored. Provided to require named inputs.
#' @param moMain  An object of class modelObj or a list of objects of class
#'   modelObjSubset, which define the models and R methods to be used to
#'   obtain parameter estimates and predictions for the main effects component 
#'   of the outcome regression. See ?modelObj and/or ?modelObjSubset for 
#'   details. NULL is an acceptable value if moCont is defined.
#' @param moCont An object of class modelObj or a list of objects of class
#'   modelObjSubset, which define the models and R methods to be used to 
#'   obtain parameter estimates and predictions for the contrasts component 
#'   of the outcome regression. See ?modelObj and/or ?modelObjSubset for 
#'   details. NULL is an acceptable value if moMain is defined.
#' @param data A data frame of covariates and treatment history.
#' @param response For the second stage analysis, the response vector.
#'    For first stage analyses, the value object returned by iqLearnSS().
#' @param object The value object returned by iqLearFSC()
#' @param txName A character string giving column header of treatment variable
#'   in data
#' @param iter An integer. See ?iter for details
#' @param verbose A logical. If TRUE, screen prints are generated.
#'
#' @usage
#'
#'   ## Second-Stage Analysis
#' iqLearnSS(..., moMain, moCont, data, response, txName, iter = 0L, 
#'           verbose = TRUE)
#' 
#' ## First-Stage Analysis for Fitted Main Effects
#' iqLearnFSM(..., moMain, moCont, data, response, txName, iter = 0L, 
#'            verbose = TRUE)
#'
#' ## First-Stage Analysis for Fitted Contrasts
#' iqLearnFSC(..., moMain, moCont, data, response, txName, iter = 0L, 
#'            verbose = TRUE)
#'
#' ## First-Stage Analysis of Contrast Variance Log-Linear Model
#' iqLearnFSV(..., object, moMain, moCont, data, iter = 0L, verbose = TRUE)
#'
#' @family statistical methods
#' @family multiple decision point methods
#'
#' @include E_class_IQLearnSS.R 
#' @aliases iqLearnSS iqLearnFSM iqLearnFSV iqLearnFSC
#'
#' @references Laber, EB, Linn, KA, and Stefanski, LA (2014).
#'   Interactive model building for Q-Learning.
#'   Biometrika, 101, 831--847. PMCID: PMC4274394.
#'
#' @examples
#' 
#' # Load and process data set
#' data(bmiData)
#' 
#' # define the negative 12 month change in BMI from baseline
#' y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]
#' 
#' #### Full Interactive Q-Learning Algorithm
#'
#' ### Second-Stage Analysis
#'
#' # outcome model
#' moMain <- buildModelObj(model = ~parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+month4BMI,
#'                         solver.method = 'lm')
#' 
#' fitSS <- iqLearnSS(moMain = moMain, moCont = moCont,
#'                    data = bmiData, response = y12,  txName = 'A2')
#' 
#' ### First-Stage Analysis Main Effects Term
#'
#' # main effects model
#' moMain <- buildModelObj(model = ~parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' fitFSM <- iqLearnFSM(moMain = moMain, moCont = moCont,
#'                      data = bmiData, response = fitSS,  txName = 'A1')
#' 
#' ### First-Stage Analysis Contrasts Term
#'
#' # contrasts model
#' moMain <- buildModelObj(model = ~parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~race + parentBMI+baselineBMI,
#'                         solver.method = 'lm')
#' 
#' fitFSC <- iqLearnFSC(moMain = moMain, moCont = moCont,
#'                      data = bmiData, response = fitSS,  txName = 'A1')
#' 
#' ### First-Stage Analysis Contrasts Variance - Log-linear
#'
#' # contrasts variance model
#' moMain <- buildModelObj(model = ~baselineBMI,
#'                         solver.method = 'lm')
#' 
#' moCont <- buildModelObj(model = ~baselineBMI,
#'                         solver.method = 'lm')
#' 
#' fitFSV <- iqLearnFSV(object = fitFSC, moMain = moMain, moCont = moCont,
#'                      data = bmiData)
#' 
#' ####Available methods
#' 
#'   ### Estimated value
#'   estimator(x = fitFSC, y = fitFSM, z = fitFSV, w = fitSS, dens = 'nonpar')
#'
#'   ## Estimated optimal treatment and decision functions for training data
#'   ## Second stage optimal treatments
#'   optTx(x = fitSS)
#'
#'   ## First stage optimal treatments when contrast variance is modeled.
#'   optTx(x = fitFSM, y = fitFSC, z = fitFSV, dens = 'nonpar')
#'
#'   ## First stage optimal treatments when contrast variance is constant.
#'   optTx(x = fitFSM, y = fitFSC, dens = 'nonpar')
#' 
#'   ## Estimated optimal treatment and decision functions for new data
#'   ## Second stage optimal treatments
#'   optTx(x = fitSS, bmiData)
#'
#'   ## First stage optimal treatments when contrast variance is modeled.
#'   optTx(x = fitFSM, y = fitFSC, z = fitFSV, dens = 'nonpar', bmiData)
#'
#'   ## First stage optimal treatments when contrast variance is constant.
#'   optTx(x = fitFSM, y = fitFSC, dens = 'nonpar', bmiData)
#' 
#' ### The following methods are available for all objects: fitSS, fitFSM,
#' ### fitFSC and fitFSV. We include only one here for illustration.
#'
#'   # Coefficients of the outcome regression objects
#'   coef(object = fitSS)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(object = fitFSM)
#' 
#'   # Value object returned by outcome regression method
#'   fitObject(object = fitFSC)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(object = fitFSV)
#'
#'   # Plots if defined by outcome regression method
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(x = fitSS)
#'   plot(x = fitSS, suppress = TRUE)
#' 
#'   # Show main results of method
#'   show(object = fitFSM)
#' 
#'   # Show summary results of method
#'   summary(object = fitFSV)
#' 
NULL

#' @export
iqLearnSS <- function(..., 
                      moMain,
                      moCont,
                      data,
                      response,
                      txName,
                      iter = 0L,
                      verbose = TRUE) {

  # moMain must be either an object of class modelObj or NULL
  if (missing(x = moMain)) moMain <- NULL
  if (!is(object = moMain, class2 = "modelObj") && !is.null(x = moMain)) {
    stop("moMain must be one of {modelObj, NULL}")
  }

  # moCont must be either an object of class modelObj or NULL
  if (missing(x = moCont)) moCont <- NULL
  if (!is(object = moCont, class2 = "modelObj") && !is.null(x = moCont)) {
    stop("moCont must be one of {modelObj, NULL}")
  }

  # at least one of {moMain, moCont} must be an object of class modelObj. If 
  # either is NULL, iterative algorithm is not appropriate.
  if (is.null(x = moMain) && is.null(x = moCont)) {
    stop("must provide moMain and/or moCont")
  } else if (is.null(x = moMain) || is.null(x = moCont)) {
    iter <- NULL
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # response must a vector
  response <- .verifyVectorResponse(response = response)

  # verify treatment is appropriately coded.
  data <- .checkTxData(txName = txName, data = data)

  # treatments must be binary
  # Note that NAs are allowed
  txVec <- .checkBinaryTx(txName = txName, data = data)
  if (!isTRUE(x = all.equal(target = txVec, current = data[,txName]))) {
    cat("Treatment variable converted to {-1,1}\n")
    data[,txName] <- as.integer(x = round(x = txVec))
  }

  # iter must be a positive integer or NULL
  iter <- .verifyIter(iter = iter)

  # verbose must be logical
  verbose <- .verifyVerbose(verbose = verbose)

  result <- .newIQLearnSS(moMain = moMain,
                          moCont = moCont,
                          data = data,
                          response = response,
                          txName = txName,
                          iter = iter,
                          suppress = !verbose)
                         
  result@analysis@call <- match.call()

  return(result)
  
}
