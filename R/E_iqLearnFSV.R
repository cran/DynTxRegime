# October 30, 2018

#' @export
iqLearnFSV <- function(...,
                       object, 
                       moMain, 
                       moCont, 
                       data, 
                       iter = 0L,
                       verbose = TRUE) {

  if (!is(object = object, class2 = 'IQLearnFS_C')) {
    stop("object must be an object returned by iqLearnFSC()")
  }

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

  # treatments must be binary
  # Note that NAs are allowed
  txName <- .getTxName(object = object@analysis)
  txVec <- .checkBinaryTx(txName = txName, data = data)
  if (!isTRUE(x = all.equal(target = txVec, current = data[,txName]))) {
    cat("Treatment variable converted to {-1,1}\n")
    data[,txName] <- as.integer(x = round(x = txVec))
  }

  # iter must be a positive integer or NULL
  iter <- .verifyIter(iter = iter)

  # verbose must be logical
  verbose <- .verifyVerbose(verbose = verbose)

  result <- .newIQLearnFS_VHet(object = object, 
                               moMain = moMain,  
                               moCont = moCont,  
                               data = data,  
                               iter = iter,
                               suppress = !verbose)

  result@analysis@call <- match.call()

  return( result )

}
