# October 26, 2018
#
#' Missing or Coarsened Data Perspective - Genetic Algorithm
#'
#' @references Baqun Zhang, Anastasios A. Tsiatis, Eric B. Laber & Marie Davidian,
#'   "A Robust Method for Estimating Optimal Treatment Regimes",
#'   Biometrics, 68, 1010-1018.
#'
#' @references Baqun Zhang, Anastasios A. Tsiatis, Eric B. Laber & Marie Davidian,
#'   "Robust estimation of optimal treatment regimes for sequential
#'   treatment decisions", Biometrika (2013) pp.1-14.
#'
#' @name optimalSeq
#'
#' @param moPropen An object of class modelObj, a list of objects of class
#'   modelObj, or a list of object of class modelObjSubset,
#'   which define the models and R methods to be used to obtain
#'   parameter estimates and predictions for the propensity for
#'   treatment.
#'   See ?moPropen for details.
#'
#' @param moMain An object of class modelObj, a list of objects of class
#'   modelObj, or a list of object of class modelObjSubset,
#'   which define the models and R methods to be used to obtain
#'   parameter estimates and predictions for the main effects
#'   component of the outcome regression.
#'   See ?modelObj and/or ?modelObjSubset for details.
#'   NULL is an acceptable input if IPWE is desired or there is
#'   no main effects component of the outcome regression model.
#'
#' @param moCont An object of class modelObj, a list of objects of class
#'   modelObj, or a list of object of class modelObjSubset,
#'   which define the models and R methods to be used to obtain
#'   parameter estimates and predictions for the contrasts
#'   component of the outcome regression.
#'   See ?modelObj and/or ?modelObjSubset for details.
#'   NULL is an acceptable input if IPWE is desired or there is
#'   no contrast component of the outcome regression model.
#'
#' @param data A data frame of the covariates and tx history
#'
#' @param response The response vector
#'
#' @param txName A vector of characters.
#'   The column headers of \emph{data} that correspond to the tx
#'   covariate for each decision point.
#'   The ordering should be sequential, i.e., the 1st element
#'   gives column name for the 1st decision point tx, the
#'   2nd gives column name for the 2nd decision point tx,
#'   etc.
#'
#' @param regimes A function or a list of functions.
#'   For each decision point, a function defining the tx
#'   rule. For example, if the tx rule is : I(eta_1 < x1),
#'   regimes is defined as
#'   regimes <- function(a,data) \{as.numeric(a < data$x1)\}
#'   THE LAST ARGUMENT IS ALWAYS TAKEN TO BE THE DATA.FRAME
#'
#' @param fSet A function or a list of functions.
#'   This argument allows the user to specify the subset of tx
#'   options available to a patient or the subset of patients
#'   that will be modeled uniquely.
#'   see ?fSet for details
#'
#' @param refit No longer used
#'
#' @param iter An integer. See ?iter for details
#'
#' @param verbose A logical. If FALSE, screen prints are suppressed.
#'
#' @param ... Additional arguments required by rgenoud. At a minimum this
#'   should include Domains, pop.size and starting.values.
#'   See ?rgenoud for more information.
#'
#' @include H_class_OptimalSeq.R H_class_OptimalSeqMissing.R 
#' @include H_class_OptimalSeqCoarsened.R
#'
#' @return An object inheriting from class OptimalSeq
#'
#' @family statistical methods
#' @family single decision point methods
#' @family multiple decision point methods
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
#' # Define the propensity for treatment model and methods.
#' # Will use constant model for both decision points
#' moPropen <- buildModelObj(model =  ~ 1, 
#'                           solver.method = 'glm', 
#'                           solver.args = list('family'='binomial'),
#'                           predict.method = 'predict.glm',
#'                           predict.args = list(type='response'))
#' moPropen <- list(moPropen, moPropen)
#'
#' # outcome model second stage
#' moMain2 <- buildModelObj(model = ~parentBMI+month4BMI,
#'                          solver.method = 'lm')
#' 
#' moCont2 <- buildModelObj(model = ~race + parentBMI+month4BMI,
#'                          solver.method = 'lm')
#' 
#' # outcome model first stage
#' moMain1 <- buildModelObj(model = ~parentBMI+baselineBMI,
#'                          solver.method = 'lm')
#' 
#' moCont1 <- buildModelObj(model = ~race + parentBMI+baselineBMI,
#'                          solver.method = 'lm')
#' 
#' moMain <- list(moMain1, moMain2)
#' moCont <- list(moCont1, moCont2)
#'
#' # regime function second stage
#' regime2 <- function(eta1, eta2, data) {
#'              tst <- {data$parentBMI > eta1} & {data$month4BMI > eta2}
#'              rec <- rep('MR', nrow(x = data))
#'              rec[!tst] <- 'CD'
#'              return( rec )
#'            }
#' 
#' # regime function first stage
#' regime1 <- function(eta1, eta2, data) {
#'              tst <- {data$parentBMI > eta1} & {data$baselineBMI > eta2}
#'              rec <- rep('MR', nrow(x = data))
#'              rec[!tst] <- 'CD'
#'              return( rec )
#'            }
#'
#' regimes <- list(regime1, regime2)
#' 
#' #### Analysis using AIPW
#' \dontrun{
#' fit_AIPW <- optimalSeq(moPropen = moPropen, 
#'                        moMain = moMain, moCont = moCont,
#'                        regimes = regimes,
#'                        data = bmiData, response = y12,  txName = c('A1','A2'),
#'                        Domains = cbind(rep(0,4),rep(100,4)),
#'                        pop.size = 100, starting.values = rep(25,4))
#' 
#' ##Available methods
#' 
#'   # Coefficients of the regression objects
#'   coef(object = fit_AIPW)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(object = fit_AIPW)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(x = fit_AIPW)
#' 
#'   # Value object returned by regression methods
#'   fitObject(object = fit_AIPW)
#' 
#'   # Retrieve the results of genetic algorithm
#'   genetic(object = fit_AIPW)
#'
#'   # Estimated optimal treatment and decision functions for training data
#'   optTx(x = fit_AIPW)
#' 
#'   # Estimated optimal treatment and decision functions for new data
#'   optTx(x = fit_AIPW, newdata = bmiData)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(object = fit_AIPW)
#'
#'   # Plots if defined by regression methods
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(x = fit_AIPW)
#'   plot(x = fit_AIPW, suppress = TRUE)
#' 
#'   # Retrieve the value object returned by propensity regression method
#'   propen(object = fit_AIPW)
#'
#'   # Show main results of method
#'   show(object = fit_AIPW)
#' 
#'   # Show summary results of method
#'   summary(object = fit_AIPW)
#' }
#' #### Single Decision Point Analysis using IPW
#'
#' # Define the propensity for treatment model and methods.
#' moPropen <- buildModelObj(model =  ~ 1, 
#'                           solver.method = 'glm', 
#'                           solver.args = list('family'='binomial'),
#'                           predict.method = 'predict.glm',
#'                           predict.args = list(type='response'))
#'
#' # regime function second stage
#' regime <- function(eta1, eta2, data) {
#'             tst <- {data$parentBMI > eta1} & {data$month4BMI > eta2}
#'             rec <- rep('MR', nrow(x = data))
#'             rec[!tst] <- 'CD'
#'             return( rec )
#'           }
#' \dontrun{
#' fit_IPW <- optimalSeq(moPropen = moPropen, 
#'                       regimes = regime,
#'                       data = bmiData, response = y12,  txName = 'A2',
#'                       Domains = cbind(rep(0,2),rep(100,2)),
#'                       pop.size = 100, starting.values = rep(25,2))
#' 
#' ##Available methods
#' 
#'   # Coefficients of the regression objects
#'   coef(object = fit_IPW)
#' 
#'   # Description of method used to obtain object
#'   DTRstep(object = fit_IPW)
#' 
#'   # Estimated value of the optimal treatment regime for training set
#'   estimator(x = fit_IPW)
#' 
#'   # Value object returned by regression method
#'   fitObject(object = fit_IPW)
#' 
#'   # Retrieve the results of genetic algorithm
#'   genetic(object = fit_IPW)
#'
#'   # Estimated optimal treatment and decision functions for training data
#'   optTx(x = fit_IPW)
#' 
#'   # Estimated optimal treatment and decision functions for new data
#'   optTx(x = fit_IPW, newdata = bmiData)
#' 
#'   # Value object returned by outcome regression method
#'   outcome(object = fit_IPW)
#'
#'   # Plots if defined by outcome regression method
#'   dev.new()
#'   par(mfrow = c(2,4))
#' 
#'   plot(x = fit_IPW)
#'   plot(x = fit_IPW, suppress = TRUE)
#' 
#'   # Retrieve the value object returned by propensity regression method
#'   propen(object = fit_IPW)
#'
#'   # Show main results of method
#'   show(object = fit_IPW)
#' 
#'   # Show summary results of method
#'   summary(object = fit_IPW)
#' }
optimalSeq <- function(...,
                       moPropen,
                       moMain,
                       moCont,
                       data,
                       response,
                       txName,
                       regimes,
                       fSet = NULL,
                       refit = FALSE,
                       iter = 0L,
                       verbose = TRUE) {

  if (!requireNamespace(package = "rgenoud", quietly = TRUE)) {
    stop("optimalSeq requires the rgenoud package, available on CRAN.")
  }

  # verify moPropen modelObj, list of modelObj, or list of ModelObjSubset
  if (missing(x = moPropen)) moPropen <- NULL
  if (is.null(x = moPropen)) stop("moPropen must be provided")
  moPropen <- .checkModelObjOrListModelObjSubsetOrList(object = moPropen, 
                                                       nm = 'moPropen')

  # verify moMain is NULL, modelObj, list of modelObj, or list of ModelObjSubset
  if (missing(x = moMain)) moMain <- NULL
  moMain <- .checkModelObjOrListModelObjSubsetOrList(object = moMain, 
                                                     nm = 'moMain')

  # verify moCont is NULL, modelObj, list of modelObj, or list of ModelObjSubset
  if (missing(x = moCont)) moCont <- NULL
  moCont <- .checkModelObjOrListModelObjSubsetOrList(object = moCont, 
                                                     nm = 'moCont')

  if (is.null(x = moMain) && is.null(x = moCont)) {
    cat("IPW estimator will be used\n")
  } else if (is.null(x = moMain) || is.null(x = moCont)) {
    iter <- NULL
  } else if (!is.null(x = moMain) && !is.null(x = moCont)) {
    if (is(object = moMain, class2 = "modelObj") && 
        !is(object = moCont, class2 = "modelObj")) {
      stop("moMain and moCont must be of same class")
    }
    if (is(object = moMain, class2 = "ModelObj_SubsetList") && 
        !is(object = moCont, class2 = "ModelObj_SubsetList")) {
      stop("moMain and moCont must be of same class")
    }
    if (is(object = moMain, class2 = "ModelObj_DecisionPointList") && 
        !is(object = moCont, class2 = "ModelObj_DecisionPointList")) {
      stop("moMain and moCont must be of same class")
    }
    if (is(object = moMain, class2 = "ModelObj_DecisionPointList")) {
      if (is(object = moCont[[ 1L ]], class2 = "ModelObj_SubsetList") && 
          !is(object = moMain[[ 1L ]], class2 = "ModelObj_SubsetList")) {
        stop("all moMain and moCont must both be ModelObjSubset")
      }

      if (is(object = moCont[[ 1L ]], class2 = "modelObj") && 
          !is(object = moMain[[ 1L ]], class2 = "modelObj")) {
        stop("all moMain and moCont must both be modelObj")
      }
    }
  }

  # regimes must be a function or a list of functions
  if (missing(x = regimes)) stop("regimes must be provided")

  if (!is.function(x = regimes)) {
    if (is.list(x = regimes)) {
      if (!is.function(x = regimes[[ 1L ]])) {
        stop("regimes must be a function or a list of functions")
      }
    } else {
      stop("regimes must be a function or a list of functions")
    }
  }

  # data must be provided as a data.frame object.
  data <- .verifyDataFrame(data = data)

  # response must a vector
  response <- .verifyVectorResponse(response = response)

  nDP <- length(x = txName)

  # number of moPropen provided must match nDP
  if (is(object = moPropen, class2 = "ModelObj_DecisionPointList")) {
    if (length(x = moPropen) != nDP) {
      stop("incorrect number of propensity models provided")
    }
  } else if (is(object = moPropen, class2 = "modelObj") || 
             is(object = moPropen, class2 = "ModelObj_SubsetList")) {
    if (nDP != 1L) {
      stop("incorrect number of propensity models provided")
    }
  }

  # number of moMain provided must be NULL or match nDP
  if (is(object = moMain, class2 = "ModelObj_DecisionPointList")) {
    if (length(x = moMain) != nDP) {
      stop("incorrect number of main effect models provided")
    }
  } else if (is(object = moMain, class2 = "modelObj") || 
             is(object = moMain, class2 = "ModelObj_SubsetList")) {
    if (nDP != 1L) {
      stop("incorrect number of main effect models provided")
    }
  }

  # number of moCont provided must be NULL or match nDP
  if (is(object = moCont, class2 = "ModelObj_DecisionPointList")) {
    if (length(x = moCont) != nDP) {
      stop("incorrect number of contrast models provided")
    }
  } else if (is(object = moCont, class2 = "modelObj") || 
             is(object = moCont, class2 = "ModelObj_SubsetList")) {
    if (nDP != 1L) {
      stop("incorrect number of contrast models provided")
    }
  }

  # verify treatment is appropriately coded.
  for (i in 1L:length(x = txName)) {
    data <- .checkTxData(txName = txName[i], data = data)
  }

  # convert regimes input into local class RegimeObj
  regimeObj <- .newRegimeObj(object = regimes)

  if (is(object = regimeObj@regime, class2 = "DecisionPointList")) {
    if (nDP == 1L || nDP != length(x = regimeObj@regime)) {
      stop("incorrect number of regimes provided")
    }
  } else if (nDP != 1L & 
             !is(object = regimeObj@regime, class2 = "DecisionPointList")) {
    stop("incorrect number of regimes provided")
  }

  if (nDP == 1L) {
    if (!is.function(x = fSet) && !is.null(x = fSet)) {
      stop("fSet must be NULL or a function")
    }
  } else {
    if (!is.list(x = fSet) && !is.null(x = fSet)) {
      stop("fSet must be NULL or a list of functions")
    }
    if (is.list(x = fSet)) {
      if (length(x = fSet) != nDP) {
        stop("incorrect number of fSet provided")
      }
      for (i in 1L:nDP) {
        if (!is.function(x = fSet[[ i ]])) {
          stop("all elements for fSet list must be functions")
        }
      }
    }
  }

  if (nDP == 1L) {
    if (is(object = moPropen, class2 = "ModelObj_SubsetList")) {
      if (!is.function(x = fSet)) {
        stop("if subset structure in moPropen, fSet must be provided")
      }
    }
    if (is(object = moMain, class2 = "ModelObj_SubsetList")) {
      if (!is.function(x = fSet)) {
        stop("if subset structure in moMain, fSet must be provided")
      }
    }
    if (is(object = moCont, class2 = "ModelObj_SubsetList")) {
      if (!is.function(x = fSet)) {
        stop("if subset structure in moCont, fSet must be provided")
      }
    }

  } else if (nDP > 1L) {
    if (is(object = moPropen[[ 1L ]], class2 = "ModelObj_SubsetList")) {
      if (!is.function(x = fSet[[ 1L ]])) {
        stop("if subset structure in moPropen, fSet must be provided")
      }
    }
    if (is(object = moMain[[ 1L ]], class2 = "ModelObj_SubsetList")) {
      if (!is.function(x = fSet[[ 1L ]])) {
        stop("if subset structure in moMain, fSet must be provided")
      }
    }
    if (is(object = moCont[[ 1L ]], class2 = "ModelObj_SubsetList")) {
      if (!is.function(x = fSet[[ 1L ]])) {
        stop("if subset structure in moCont, fSet must be provided")
      }
    }
  }

  # iter must be a positive integer or NULL
  iter <- .verifyIter(iter = iter)

  # verbose must be logical
  verbose <- .verifyVerbose(verbose = verbose)

  # verify presence of required input for rgenoud
  argsList <- list(...)

  tst <- c("Domains", "pop.size", "starting.values") %in% names(x = argsList)

  if (!all(tst)) {
    stop("Domains, pop.size, and starting.values for rgenoud are required inputs")
  }

  result <- .newOptimalSeq(moPropen = moPropen,
                           moMain = moMain,
                           moCont = moCont,
                           data = data,
                           response = response,
                           txName = txName,
                           regimesObj = regimeObj,
                           fSet = fSet,
                           iter = iter,
                           suppress = !verbose,
                           argsList = argsList)
    
  result@analysis@call <- match.call()

  return( result )

}
