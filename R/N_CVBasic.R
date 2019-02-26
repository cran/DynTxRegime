# October 26, 2018

#' Class \code{CVBasic}
#'
#' Class \code{CVBasic} holds cross-validation procedure parameters
#'
#' @name CVBasic-class
#'
#' @slot folds An integer
#' @slot sample A lsit
#'
#' @keywords internal
setClass(Class = "CVBasic",
         slots = c(folds = "ANY",
                   sample = "list"),
         prototype = prototype(folds = NULL, sample = list()))

#' @rdname DynTxRegime-internal-api
setMethod(f = "initialize",
          signature = c(.Object = "CVBasic"),
          definition = function(.Object, cvFolds, txVec, ...) {

              if (is.null(x = cvFolds) ) return( .Object )

              txBreaks <- list()
              txOpts <- sort(x = unique(x = txVec))
              ntx <- length(x = txOpts)

              for (i in 1L:ntx) {
                tst <- which(x = txVec == txOpts[i])
                if (length(x = tst) > 1L) {
                  txBreaks[[ i ]] <- sample(x = tst)
                } else {
                  txBreaks[[ i ]] <- tst
                }
                if (length(x = txBreaks[[ i ]]) < cvFolds) {
                  cat("unable to represent all treatment in all folds\n")
                  stop("consider decreasing number of folds")
                }
              }

              .Object@folds <- cvFolds
              .Object@sample <- txBreaks

              return( .Object )
            })

#' An n-Fold Cross Validation Step
#'
#' @name newCVStep
#'
#' @param cvObject Information regarding folds and treatment groups
#' @param methodObject Information needed for method specific objective function
#' @param lambda numeric A single tuning parameter value
#' @param suppress integer indicating printing preference
#' @param ... additional inputs.
#'
#' @return The average value across all successfully trained folds
#'
#' @keywords internal
.newCVStep <- function(cvObject,
                       methodObject,
                       lambda,
                       suppress, ...) {

  # if cross-validation not requested return NULL
  if (is.null(x = cvObject@folds) ) return( NULL )

  value <- 0.0

  i <- 1L
  fold_cnt <- 0L

  while (i <= cvObject@folds) {

    if (suppress != 0L) cat("Fold", i, "of", cvObject@folds, "\n")

    # data is broken into treatment groups
    tst_idx <- NULL
    trn_idx <- NULL
    for (j in 1L:length(x = cvObject@sample)) {
      # determine how many samples are in group j
      nt <- length(x = cvObject@sample[[ j ]])
      # take every fold_th member of group j for testing
      idx <- seq(from = i, to = nt, by = cvObject@folds)
      tstTemp <- cvObject@sample[[ j ]][idx]
      # keep all other members for training
      trn_idx <- c(trn_idx, setdiff(x = cvObject@sample[[ j ]], 
                                                y = tstTemp))
      tst_idx <- c(tst_idx, tstTemp)
    }

    # train on this data
    train <- .trainForValue(methodObject = methodObject,
                            train_subset = trn_idx,
                            test_subset = tst_idx,
                            lambda = lambda,
                            suppress = suppress, ...)

    i <- i + 1L

    # if train is NULL - training was not successful
    if (is.null(x = train)) {
      if (suppress != 0L) cat("Optimization not successful.\n")
      next
    }

    # if training was successful, add value and go to next fold
    fold_cnt <- fold_cnt + 1L
    if (suppress != 0L) cat("value:", train$dtrObj@optimal@estimatedValue, "\n")
    value <- value + train$dtrObj@optimal@estimatedValue
  }

  if (fold_cnt > 0L) {
    # if at least one fold was successful return value
    value <- value / fold_cnt
    if (suppress != 0L) {
      cat("Average value over successful folds:", value, "\n")
    }
    return( value )
  } else {
    # if no folds were successful return NULL
    return( NULL )
  }

}

# procedure 
#   obtains parameter estimates for train_subset
#   uses estimated parameters to calculate 
#      value for test_subset
#      decision function for test_subset
#      optimal tx for test_subset
#
# returns NULL if optimization not successful
# returns list if optimization successful
#   list contains "optimObj" (a OptimObj) and "dtrObj" (a DynTxRegime)
.trainForValue <-  function(methodObject,
                            train_subset,
                            test_subset,
                            lambda,
                            suppress, ...) {

  # subset methodObject to training set
  trainObj <- .subsetObject(methodObject = methodObject, subset = train_subset)

  if (!is(object = methodObject@kernel, class2 = "LinearKernel")) {
    trainObj@pars <- c(trainObj@pars[1L],trainObj@pars[-1L][train_subset])
  }

  # optimize objective function for training set
  trainResult <- .newOptimObj(lambda = lambda,
                              methodObject = trainObj,
                              suppress = suppress, ...)

  # if training was not successful return NULL 
  if (is.null(x = trainResult@optim)) return( NULL )

  # subset methodObject to test set
  testObj <- .subsetObject(methodObject = methodObject, subset = test_subset)

  # calculate decision function and optimal tx for test set
  opts <- .predictOptimalTx(x = trainResult, newdata = testObj@x)

  # calculate value for test set
  val <- .valueFunc(optTx = opts$optimalTx, methodObject = testObj)

  optObj <- new(Class = "OptimalObj",
                "optimal" = new(Class = "OptimalInfo",
                                "optimalTx" = opts$optimalTx,
                                "decisionFunc" = opts$decisionFunc,
                                "estimatedValue" = val))

  dtrObj <- new(Class = "DynTxRegime",
                optObj,
                "call" = NULL)

  return( list("optimObj" = trainResult, "dtrObj" = dtrObj) )

}
