# October 24, 2018

.validity_TxSubset <- function(object) {

  # ensure that subsets identified for each patient are in the list of subsets
  ssNames <- names(x = object@subsets)
  tst <- object@ptsSubset %in% ssNames
  if (any(!tst)) return( "patient subset not found in the subset list" )

  # ptsSubset and singleton must be of equal length
  if (length(x = object@singleton) != length(x = object@ptsSubset)) {
    return( "ptsSubset/singleton not appropriately defined" )
  }

  return( TRUE )
}

#' Class \code{TxSubset}
#'
#' Class \code{TxSubset} stores subset information for tx
#'
#' @name TxSubset-class
#'
#' @slot ptsSubset A character vector. The name of the subset of which each 
#'   patient is a member
#' @slot subsetRule A function. The fSet function provided by user.
#' @slot subsets A list. The feasible treatments for each subset. The elements  
#'   must be named and contain tx subsets
#' @slot singleton A logical vector. TRUE indicates if 1 tx is available to  
#'   each patient
#'
#' @keywords internal
setClass(Class = "TxSubset",
         slots = c(ptsSubset  = "character",
                   subsetRule = "function",
                   subsets    = "list",
                   singleton  = "logical"),
         prototype = prototype(ptsSubset = "NA",
                               subsetRule = function(x){NA},
                               subsets = list(),
                               singleton = FALSE),
         validity = .validity_TxSubset)


##########
# GENERICS
##########

#' Create \code{TxSubset} Object
#'
#' Processes input to determine ptsSubset and singleton to create a
#'   \code{TxSubset} object
#'
#' @name newTxSubset
#'
#' @keywords internal
setGeneric(name = ".newTxSubset",
           def = function(fSet, superset, ...) { 
               standardGeneric(f = ".newTxSubset") 
             })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getPtsSubset",
           def = function(object) { standardGeneric(f = ".getPtsSubset") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getSingleton",
           def = function(object) { standardGeneric(f = ".getSingleton") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getSubsetRule",
           def = function(object) { standardGeneric(f = ".getSubsetRule") })

#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getSubsets",
           def = function(object) { standardGeneric(f = ".getSubsets") })

##########
# METHODS
##########
#' Methods Available for Objects of Class \code{TxSubset}
#'
#' @name TxSubset-methods
#'
#' @keywords internal
NULL

#' @rdname newTxSubset
setMethod(f = ".newTxSubset",
          signature = c(fSet = "function",
                        superset = "ANY"),
          definition = function(fSet, data, superset, verify, suppress, ...) {

              pro <- .feasibleTx(fSet = fSet, 
                                 superset = superset, 
                                 data = data, 
                                 suppress = suppress, 
                                 verify = verify)

              singleton <- logical(length(x = pro$ptsSubset))
              for (i in 1L:length(x = pro$subsets)) {
                if (length(x = pro$subsets[[ i ]]) == 1L) {
                  tst <- pro$ptsSubset == names(x = pro$subsets)[i]
                  singleton[tst] <- TRUE
                }
              }

              res <- new("TxSubset",
                         subsets = pro$subset,
                         ptsSubset = pro$ptsSubset,
                         subsetRule = fSet,
                         singleton = singleton)

              return( res )
            })

#' \code{.convertFromBinary(txObj, txVec)}
#'   converts a -1/1 Tx to user provided tx coding.
#'
#' @rdname TxSubset-methods
setMethod(f = ".convertFromBinary",
          signature = c("txObj" = "TxSubset"),
          definition = function(txObj, txVec, ...){

              subsets <- .getSubsets(object = txObj)

              ptsSubsets <- .getPtsSubset(object = txObj)

              optVec <- rep(x = NA, times = length(x = txVec))
              tst <- is.na(x = txVec)
              txVec[tst] <- 0.0

              for (i in 1L:length(x = subsets)) {
                levs <- subsets[[ i ]]
                usePts <- ptsSubsets == names(x = subsets)[i]

                if (length(x = levs) == 1L) {
                  optVec[usePts] <- levs[1L]
                } else if (length(x = levs) == 2L) {
                  optVec[usePts & txVec < -0.5] <- levs[1L]
                  optVec[usePts & txVec >  0.5] <- levs[2L]
                }
              }
              optVec[tst] <- NA
              return( optVec )
            })

#' \code{.convertToBinary(txObj, data)}
#'   converts user specified tx variable to binary -1/1.
#'
#' @rdname TxSubset-methods
setMethod(f = ".convertToBinary",
          signature = c("txObj" = "TxSubset"),
          definition = function(txObj, data, ...){ stop("not allowed") })

#' \code{.getPtsSubset(object)}
#'  retrieve subset name for which each pt is a member.
#'
#' @rdname TxSubset-methods
setMethod(f = ".getPtsSubset",
          signature = c(object = "TxSubset"),
          definition = function(object) { return( object@ptsSubset ) })

#' \code{.getSingleton(object)}
#'   retrieve T/F indicator of only 1 tx option available to each pt.
#'
#' @rdname TxSubset-methods
setMethod(f = ".getSingleton",
          signature = c(object = "TxSubset"),
          definition = function(object) { return( object@singleton ) })

#' @rdname DynTxRegime-internal-api
setMethod(f = ".getSubsetRule",
          signature = c(object = "ANY"),
          definition = function(object) { return( NULL ) })

#' \code{.getSubsetRule(object)}
#'   retrieve feasible set identification rule.
#'
#' @rdname TxSubset-methods
setMethod(f = ".getSubsetRule",
          signature = c(object = "TxSubset"),
          definition = function(object) { return( object@subsetRule ) })

#' \code{.getSubsets(object)}
#'   retrieve subset names and tx options.
#'
#' @rdname TxSubset-methods
setMethod(f = ".getSubsets",
          signature = c(object = "TxSubset"),
          definition = function(object) { return( object@subsets ) })

#' \code{.validTx(object, txVec)}
#'   ensures all elements in txVec are allowed by superset.
#'
#' @rdname TxSubset-methods
setMethod(f = ".validTx",
          signature = c(object = "TxSubset",
                        txVec = "ANY"),
          definition = function(object, txVec){

              if (length(x = txVec) != length(x = object@ptsSubset)) {
                stop("cannot evaluate validity of tx")
              }

              for (i in 1L:length(x = object@subsets)) {
                inss <- object@ptsSubset %in% names(x = object@subsets)[i]
                tst <- txVec[inss] %in% object@subsets[[ i ]]
                if (!all(tst)) stop("tx value not allowed by fSet")
              }

              return( NULL )

            })

##########
# LOCAL GENERICS
##########

#' @rdname DynTxRegime-internal-api

setGeneric(name = ".identifySubsets",
           def = function(fSetResult, input, ...) {
               standardGeneric(f = ".identifySubsets")
             })

##########
# LOCAL METHODS
##########

# new fSet format with data.frame as input
#' @rdname DynTxRegime-internal-api
setMethod(f = ".identifySubsets",
          signature = c(fSetResult = "list",
                        input = "data.frame"),
          definition = function(fSetResult, input, fSet) {

              subsets <- list()

              tt <- fSet(input)

              for (i in 1L:length(x = tt$subsets)) {
                if (!is.list(x = tt$subsets[[ i ]])) {
                  stop("verify fSet")
                }
                if (length(x = tt$subsets[[ i ]]) != 2L) stop("verify fSet")
                subsets[[ tt$subsets[[ i ]][[ 1L ]] ]] <- tt$subsets[[ i ]][[ 2L ]]
              }

              subsets <- subsets[sort(x = names(x = subsets))]

              return( list("subsets" = subsets, 
                           "ptsSubset" = tt$txOpts) )
            })

# new fSet format with covariates as input
#' @rdname DynTxRegime-internal-api
setMethod(f = ".identifySubsets",
          signature = c(fSetResult = "list",
                        input = "list"),
          definition = function(fSetResult, input, fSet) {

              subsets <- list()

              tt <- do.call(what = fSet, args = input)

              for (i in 1L:length(x = tt$subsets)) {
                if (!is.list(x = tt$subsets[[ i ]])) {
                  stop("verify fSet")
                }
                if (length(x = tt$subsets[[ i ]]) != 2L ) stop("verify fSet")
                subsets[[ tt$subsets[[ i ]][[ 1L ]] ]] <- tt$subsets[[ i ]][[ 2L ]]
              }

              subsets <- subsets[sort(x = names(x = subsets))]

              return( list("subsets" = subsets, 
                           "ptsSubset" = tt$txOpts) )
            })

# old fSet format with data.frame input
#' @rdname DynTxRegime-internal-api
setMethod(f = ".identifySubsets",
          signature = c(fSetResult = "ANY",
                        input = "data.frame"),
          definition = function(fSetResult, input, fSet) {
              subsets <- list()
              ptsSubset <- character(nrow(x = input))

              for (i in 1L:nrow(x = input)) {

                tt <- fSet(input[i,,drop=FALSE])

                if (length(x = tt) != 2L) stop("verify fSet")

                subsets[[ tt[[ 1L ]] ]] <- tt[[ 2L ]]

                ptsSubset[i] <- tt[[ 1L ]]
              }

              subsets <- subsets[sort(x = names(x = subsets))]

              return( list("subsets" = subsets, 
                           "ptsSubset" = ptsSubset) )
            })

# old fSet format with covariate input
#' @rdname DynTxRegime-internal-api
setMethod(f = ".identifySubsets",
          signature = c(fSetResult = "ANY",
                        input = "list"),
          definition = function(fSetResult, input, fSet) {
              subsets <- list()
              ptsSubset <- character(length(x = input[[ 1L ]]))

              for (i in 1L:length(x = input[[ 1L ]])) {

                args <- lapply(X = input, FUN = function(x) { x[i] })

                tt <- do.call(what = fSet, args = args)

                if (length(x = tt) != 2L) stop("verify fSet")

                subsets[[ tt[[ 1L ]] ]] <- tt[[ 2L ]]

                ptsSubset[i] <- tt[[ 1L ]]
              }

              subsets <- subsets[sort(x = names(x = subsets))]

              return( list("subsets" = subsets, 
                           "ptsSubset" = ptsSubset) )
            })

# old fSet format with covariate input
#' @rdname DynTxRegime-internal-api
setMethod(f = ".identifySubsets",
          signature = c(fSetResult = "ANY",
                        input = "ANY"),
          definition = function(fSetResult, input, fSet) {
              stop("unexpected returned value from fSet")
            })


.executeFset_dataFrame <- function(x, rule, verify) {

  txvec <- tryCatch(expr = rule(x),
                    condition = function(c){
                        stop("unable to execute fSet")
                      })
  if (verify) txvec <- .verifyTxvec(txvec = txvec)

  return( txvec )
}

.executeFset_covariates <- function(x, rule, verify){
  txvec <- tryCatch(expr = do.call(what = rule, 
                            args = as.list(x = x)),
                    condition = function(c){
                        stop("unable to execute fSet")
                      })

  if (verify) txvec <- .verifyTxvec(txvec = txvec)

  return( txvec )
}

# Determine feasible treatment information
#
# @param fSet A function.
# @param superset A character/integer vector of all treatment options
# @param data A data.frame
# @param suppress A logical indicating if print statements are executed
# @param verify A logical indicating of verification should be performed
#
.feasibleTx <- function(fSet, superset, data, suppress, verify) {

  fSetFormals <- names(x = formals(fun = fSet))

  if (length(x = fSetFormals) == 0L || is.null(x = fSetFormals)) {
    stop("formal arguments of fSet could not be identified")
  }

  # identify if formals match the column names of the dataset
  matchedColnames <- match(x = fSetFormals, table = colnames(x = data))

  unmatched <- which(x = is.na(x = matchedColnames))

  if (length(x = fSetFormals) == 1L && length(x = unmatched) == 1L) {

    # if one input and input does not match column names, assume
    # input to function is the data.frame object
    testFSet <- .executeFset_dataFrame(x = data[1L,,drop=FALSE], 
                                       rule = fSet,  
                                       verify = FALSE)

    res <- .identifySubsets(fSetResult = testFSet[[ 1L ]], 
                            input = data,
                            fSet = fSet)

  } else if (length(x = unmatched) == 0L) {

    dm <- as.list(x = data[,matchedColnames, drop=FALSE])

    testFSet <- .executeFset_covariates(x = data[1L, matchedColnames], 
                                        rule = fSet,  
                                        verify = FALSE)

    res <- .identifySubsets(fSetResult = testFSet[[ 1L ]], 
                            input = dm,
                            fSet = fSet)

  } else {

    stop(paste("fSet formal arguments", 
               paste(fSetFormals[unmatched], collapse = ", "), 
               "could not be found in dataset"))

  }

  if (verify) {
    ssOpts <- NULL
    for (i in 1L:length(x = res$subsets)) {
      if (!all(res$subsets[[ i ]] %in% superset)) {
        stop("subset has treatments not in data")
      }
      ssOpts <- c(ssOpts, res$subsets[[ i ]])
    }
    if (any(!{superset %in% ssOpts})) {
      stop("data has treatments not in subsets")
    }
  }

  if (!suppress) {
    cat("\nSubsets of treatment identified as:\n")
    print(x = res$subsets)
    counts <- sapply(X = names(x = res$subsets), 
                     FUN = function(x){sum(x == res$ptsSubset)})
    names(counts) <- names(x = res$subsets)
    cat("Number of patients in data for each subset:\n")
    print(x = counts)
  }

  return( res )

}

#  Returns a list object of the verified and appropriately typed result
#  of a call to fSet. The first element must be a character object
#  indicating the nickname of the subset. The second element will be
#  a character/integer representation of the subset.
#
# @param txvec An object returned by a call to user defined fSet.
#
.verifyTxvec <- function(txvec){

  # fSet must return a list object. 
  #
  # in old implementation, the list pertained to a single individual
  # the first element is a nickname for the subset to which the
  # individual belonged; the second element contained the tx options
  #
  # in new implementation, the list pertains to all individuals
  # the first element is 'subsets' and contains the tx subset options
  # the second is txOpts and contains the tx options for each indv.
  #
  if (is.list(x = txvec)) {
    if (length(x = txvec) != 2L) stop("fSet must return a list of length 2")
  } else {
    stop("fSet must return a list")
  }

  if (length(x = names(x = txvec)) > 0 && 
      all(names(x = txvec) %in% c("subsets", "txOpts"))) {
    nms <- NULL
    for (i in 1L:length(x = txvec[[ "subsets" ]])) {
      tmp <- txvec[[ "subsets" ]][[ i ]]
      if (length(x = tmp) != 2L) {
        stop("each subset should be provided as a list of length 2")
      }
      nms <- c(nms, tmp[[ 1L ]])
      if (length(x = tmp[[ 2L ]]) < 1L) {
        stop("subsets must contain at least one treatment option")
      }
      if (class(x = tmp[[ 2L ]]) == "factor") {
        # if provided as a factor vector, convert to character
        txvec[[ "subsets" ]][[ i ]][[ 2L ]] <- levels(x = tmp[[ 2L ]])[tmp[[ 2L ]]]
      } else if (class(x = tmp[[2L]]) == "numeric") {
        # if provided as a numeric vector, round and convert to integer
        txvec[[ "subsets" ]][[ i ]][[2L]] <- as.integer(round(tmp[[2L]],0L))
      } else if (class(x = tmp[[ 2L ]]) != "factor" && 
                 class(x = tmp[[ 2L ]]) != "integer" &&
                 class(x = tmp[[ 2L ]]) != "character") {
        stop("fSet defined treatment must be factor/character or integer")
      }

    }
    if (!all(txvec[[ "txOpts" ]] %in% nms)) {
      stop("txOpts returns a subset not identified in subsets")
    }
  } else if (!is.null(x = names(x = txvec))) {
    stop(paste("fSet must return a list with elements named 'subsets' and 'txOpts' or\n",
               "fSet must return an unnamed list with a subset name in the 1st element",
               "and a vector of treatment options in the 2nd element"))
  } else {
    # the first element must be a character description of subset
    if (!is.character(x = txvec[[ 1L ]])) {
      stop("first element of list returned by fSet must be a character")
    }

    # second must be a vector of treatment options available
    if (length(x = txvec[[ 2L ]]) == 0L) {
      stop(paste("at least one tx option must be available ",
                 "to every patient --- verify fSet", sep=""))
    }

    if (class(x = txvec[[ 2L ]]) == "factor") {
      # if provided as a factor vector, convert to character
      txvec[[ 2L ]] <- levels(x = txvec[[ 2L ]])[txvec[[ 2L ]]]
    } else if (class(x = txvec[[ 2L ]]) == "numeric") {
      # if provided as a numeric vector, round and convert to integer
      txvec[[2L]] <- as.integer(round(txvec[[2L]],0L))
    } else if (class(x = txvec[[ 2L ]]) != "factor" && 
               class(x = txvec[[ 2L ]]) != "integer" &&
               class(x = txvec[[ 2L ]]) != "character") {
      stop("fSet defined treatment must be factor/character or integer")
    }
  }

  return( txvec )

}
