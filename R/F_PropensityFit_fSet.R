# October 26, 2018

#' Class \code{PropensityFit_fSet}
#'
#' Class \code{PropensityFit_fSet} is a \code{TypedFit_fSet} identified as being
#'   for a propensity regression step.
#'
#' @name PropensityFit_fSet-class
#'
#' @slot small A logical TRUE indicates that the smallest valued tx is
#'   missing; FALSE indicates that the largest valued tx is missing
#' @slot levs A vector; the set of treatment options included in fit.
#'
#' @keywords internal
setClass("PropensityFit_fSet",
         slots = c(small = "logical",
                   levs = "vector"),
         contains = c("TypedFit_fSet"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{PropensityFit_fSet}
#'
#' Methods call equivalently named methods defined for \code{TypedFit_fSet}
#'
#' @name PropensityFit_fSet-methods
#'
#' @keywords internal
NULL

#' @rdname newPropensityFit
setMethod(f = ".newPropensityFit",
          signature = c(moPropen = "modelObj",
                        txObj = "TxInfoWithSubsets"),
          definition = function(moPropen, txObj, data, suppress) {

              txName <- .getTxName(object = txObj)

              fitResult <- try(expr = .newTypedFit(modelObj = moPropen,
                                                   data = data,
                                                   response = data[,txName],
                                                   txObj = txObj,
                                                   type = "moPropen",
                                                   suppress = suppress),
                               silent = TRUE)

              if (is(object = fitResult, class2 = "try-error")) {
                cat("converting response to factor and trying again\n")
                fitResult <- .newTypedFit(modelObj = moPropen,
                                          data = data,
                                          response = factor(x = data[,txName]),
                                          type = "moPropen",
                                          txObj = txObj,
                                          suppress = suppress)
              }

              subsets <- .getSubsets(object = txObj)
              superset <- .getSuperset(object = txObj)

              txOpts <- NULL
              for (i in 1L:length(x = subsets)) {
                if (length(x = subsets[[ i ]]) == 1L ) next
                txOpts <- c(txOpts, subsets[[ i ]])
              }
              levs <- superset[superset %in% txOpts]

              res <- new(Class = "PropensityFit_fSet",
                         "small" = moPropen@predictor@propenMissing == "smallest",
                         "levs" =  levs,
                         fitResult)

              return( res )

            })

#' @rdname PropensityFit_fSet-methods
setMethod(f = "coef",
          signature = c(object = "PropensityFit_fSet"),
          definition = function(object, ...) {
              return( callNextMethod()$moPropen )
            })

#' @rdname PropensityFit_fSet-methods
setMethod(f = "fitObject",
          signature = c(object = "PropensityFit_fSet"),
          definition = function(object, ...) {
              return( callNextMethod()$moPropen )
            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts propensity for all tx options.
#'   Returns a matrix of propensities predicted for all tx. 
#'   Tx options not available to a pt are coded as NA.
#'
#' @rdname PropensityFit_fSet-methods
setMethod(f = ".predictAll",
          signature = c(object = "PropensityFit_fSet",
                        newdata = "data.frame"),
          definition = function(object, 
                                newdata,  
                                suppress = TRUE) {


              txNew <- .newTxObj(fSet = .getSubsetRule(object = object@txInfo),
                                 txName = .getTxName(object = object@txInfo),
                                 data = newdata,
                                 suppress = TRUE,
                                 verify = FALSE)

              singles <- .getSingleton(object = txNew)

              res <- predict(object = as(object = object, Class = "TypedFit_fSet"), 
                             newdata = newdata[!singles,])

              if (is.null(x = ncol(x = res)) ) {
                res <- matrix(data = res, ncol = 1L)
              }

              if (is.character(x = res[1L])) {
                stop("propensities returned as characters")
              }

              if (any(res < -1.5e-8, na.rm = TRUE)) {
                stop("cannot have negative probabilities")
              }

              levs <- object@levs

              if (ncol(x = res) != length(x = levs)) {

                correction <- 1.0 - rowSums(x = res)

                if (object@small) {
                  if (!suppress ) {
                    cat("assumed missing prediction for", levs[1L],"\n")
                  }
                  res <- cbind(correction, res)
                } else {
                  if (!suppress ) {
                    cat("assumed missing prediction for", 
                        levs[length(x = levs)],"\n")
                  }
                  res <- cbind(res, correction)
                }

              }

              n <- nrow(x = newdata)

              superset <- .getSuperset(object = object@txInfo)

              mm <- matrix(data = 0.0,
                           nrow = n,
                           ncol = length(x = superset),
                           dimnames = list(NULL, superset))

              cols <- match(x = levs, table = superset)

              mm[!singles,cols] <- res

              if (any(.getSingleton(object = txNew))) {

                subsets <- .getSubsets(object = object@txInfo)
                ptsSubset <- .getPtsSubset(object = txNew)

                for (i in 1L:length(x = subsets)) {
                  if (length(x = subsets[[ i ]]) != 1L ) next
                  tst <- ptsSubset == names(x = subsets)[i]
                  cols <- superset %in% subsets[[ i ]]
                  mm[tst,cols] <- 1.0
                  mm[tst,!cols] <- 0.0
                }

              }

              return( mm )
            })

#' @rdname PropensityFit_fSet-methods
setMethod(f = "propen",
          signature = c(object = "PropensityFit_fSet"),
          definition = function(object, ...) {
              return( fitObject(object = object) )
            })

#' @rdname PropensityFit_fSet-methods
setMethod(f = "summary",
          signature = c(object = "PropensityFit_fSet"),
          definition = function(object, ...) {
              return( callNextMethod()$moPropen )
            })
