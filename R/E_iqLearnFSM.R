# October 30, 2018
#' @export
iqLearnFSM <- function(...,
                       moMain,
                       moCont,
                       data,
                       response,
                       txName,
                       iter = 0L,
                       verbose = TRUE){

  # moMain must be either an object of class modelObj or NULL
  if (missing(x = moMain)) moMain <- NULL
  if (!is(object = moMain, class2 = "modelObj") && 
      !is.null(x = moMain)) {
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

  # response must an IQLearnSS object
  if (!is(object = response, class2 = "IQLearnSS")) {
    stop("response must be an object returned by iqLearnSS()")
  }

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

  result <- .newIQLearnFS_ME(moMain = moMain,
                             moCont = moCont,
                             data = data,
                             response = response,
                             txName = txName,
                             iter = iter,
                             suppress = !verbose)

  result@analysis@call <- match.call()

  return(result)
}
