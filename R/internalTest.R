.verifyCV <- function(lambdas, cvFolds, kparam) {

  # lambdas must be numeric.
  if (!is.numeric(x = lambdas)) stop("lambdas must be a numeric")

  # if cvFolds provided as NULL reset to 0 for testing convenience
  if (is.null(x = cvFolds)) cvFolds <- 0L

  # cvFolds must be an integer.
  if (!is.numeric(x = cvFolds)) stop("cvFolds must be a numeric")

  cvFolds <- as.integer(x = round(x = cvFolds))

  # cvFolds must be non-negative.
  if (cvFolds < 0) {
    stop("cvFolds must be non-negative")
  }

  # if kparam provided as NULL reset to 0 for testing convenience
  if (is.null(x = kparam)) kparam <- 0.0

  # if not using cross-validation to estimate lambda/kparam, verify that
  # only one value is given. If more than 1, ignore all but the first element.

  if (cvFolds == 0L) {

    if (length(x = lambdas) > 1L) {
      warning("only first lambda value considered")
      lambdas <- lambdas[1L]
    }
    if (length(x = kparam) > 1L) {
      warning("only first kparam value considered")
      kparam <- kparam[1L]
    }
  }

  # if both kparam and lambdas are of length 1, cvFolds set to NULL to
  # turn off cross-validation procedure
  if ({length(x = lambdas) == 1L} && {length(x = kparam) == 1L}) {
    cvFolds <- NULL
  }

  # multiple lambdas provided to internal methods as an array
  if (length(x = lambdas) > 1L) lambdas <- array(data = lambdas)

  # multiple kparam provided to internal methods as an array
  if (length(x = kparam) > 1L) kparam <- array(data = kparam)

  return( list("cvFolds" = cvFolds, "lambdas" = lambdas, "kparam" = kparam) )
}

.verifySurrogate <- function(surrogate) {

  surrogate <- tolower(surrogate)
  if (surrogate == "logit") {
    message("Note that the logit function was changed in version 4.13")
    surrogate <- new("LogitSurrogate")
  } else if (surrogate == "exp") {
    surrogate <- new("ExpSurrogate")
  } else if (surrogate == "sqhinge") {
    surrogate <- new("SqHingeSurrogate")
  } else if (surrogate == "hinge") {
    surrogate <- new("HingeSurrogate")
  } else if (surrogate == "huber") {
    surrogate <- new("HuberHingeSurrogate")
  } else {
    stop("surrogate is not recognized")
  }

  return( surrogate )

}

.verifyIter <- function(iter) {
  # iter must be a positive integer or NULL
  if (!is.null(x = iter)) {
    if (is.numeric(x = iter)) {
      iter <- as.integer(x = round(x = iter))
    } else {
      stop("iter must be integer, numeric, or NULL")
    }
    if (length(x = iter) > 1) stop("only one value allowed")
    if (iter < 0L) stop("iter must be non-negative or NULL")
    if (iter == 0L) iter <- NULL
  }

  return( iter )
}

.verifyRegime <- function(regime, fSet) {
  # regime must be formula or a list of formula
  if (!is(object = regime, class2 = "formula") && !is.list(x = regime)) {
    stop("regime must be a formula or a list of formula")
  }

  if (is.list(x = regime)) {
    # if regime is a list, ensure all elements are formula
    tst <- lapply(X = regime, 
                  FUN = function(x){
                      is(object = x, class2 = "formula")
                    })
    if (!all(unlist(x = tst))) {
      stop("if regime is list, all elements must be a formula")
    }

    # if regime is a list, subset structure assumed and must be provided
    # through fSet
    if (is.null(x = fSet)) {
      stop("if regime is a list, fSet must be specified")
    }

    # if regime is a list, elements must be named
    if (any(is.null(x = names(x = regime)))) {
      stop("regime must be a named list")
    }

    if (any(nchar(x = names(x = regime)) == 0L)) {
      stop("regime must be a named list")
    }
  }

  return( regime )
}

.verifyVectorResponse <- function(response) {

  if (is.data.frame(x = response)) response <- data.matrix(frame = response)

  if (is.matrix(x = response)) {
    if (ncol(x = response) != 1L) stop("response must be a vector")
    response <- drop(x = response)
  }

  if (!is.vector(x = response)) stop("response must be a vector")

  if (is.array(x = response)) response <- as.vector(x = response)

  return( response )
}

.verifyDataFrame <- function(data){

  if (!is.data.frame(x = data)) stop("data must be a data.frame")

  return( data )
}

.verifyVerbose <- function(verbose){

  if (!is.logical(x = verbose)) stop("verbose must be a TRUE/FALSE")

  return( verbose )
}

.verifyResponseReward <- function(response, reward){

  # verify that a reward vector is provided.
  if (is.null(x = response) && is.null(x = reward)) {
    stop("either response or reward must be provided")
  }

  # if both response and reward are specified, ensure equivalent
  if (!is.null(x = response) && !is.null(x = reward)) {
    if (!isTRUE(x = all.equal(target = response, current = reward))) {
      stop("response/reward are equivalent - provide only one")
    }
  }

  # if only reward is provided, move to response variable
  if (is.null(x = response) && !is.null(x = reward)) response <- reward

  return( response )
}

.verifyFSet <- function(fSet) {

  # fSet can be a function with non-list regime and modelObj moPropen when
  # single tx options exist for some patients.
  if (!is.function(x = fSet) && !is.null(x = fSet)) {
    stop("fSet must be NULL or a function")
  }

  return( fSet )
}
