# October 26, 2018

#' Class \code{PropensityFit_SubsetList}
#'
#' Class \code{PropensityFit_SubsetList} is a \code{TypedFit_SubsetList} 
#'   identified as being for a propensity regression step.
#'
#' @name PropensityFit_SubsetList-class
#'
#' @slot small A logical vector TRUE indicates that the smallest valued tx is
#'   missing; FALSE indicates that the largest valued tx is missing
#' @slot levs A list; the set of treatment options included in each fit.
#'
#' @include F_PropensityFit.R F_PropensityFit_fSet.R
#'
#' @keywords internal
setClass(Class = "PropensityFit_SubsetList",
         slots = c(small = "vector",
                   levs = "list"),
         contains = c("TypedFit_SubsetList"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{PropensityFit_SubsetList}
#'
#' Most methods call equivalently named methods defined for 
#'   \code{TypedFit_SubsetList}
#'
#' @name PropensityFit_SubsetList-methods
#'
#' @keywords internal
NULL

#' @rdname newPropensityFit
setMethod(f = ".newPropensityFit",
          signature = c(moPropen = "ModelObj_SubsetList",
                        txObj   = "TxInfoWithSubsets"),
          definition = function(moPropen,
                                txObj,
                                data,
                                suppress) {

              txName <- .getTxName(object = txObj)

              res <- list()

              nms <- names(x = moPropen)

              type <- rep(x = "moPropen", times = length(x = moPropen))

              superset <- .getSuperset(object = txObj)
              subsets <- .getSubsets(object = txObj)
              ptsSubset <- .getPtsSubset(object = txObj)

              txReceived <- data[,txName]

              small <- NULL
              levs <- list()

              for (i in 1L:length(x = moPropen)) {

                # retrieve subset names associated with ith model
                modelSubset <- unlist(x = strsplit(x = nms[i], split = ","))

                # identify pts in the subset(s)
                use4fit <- ptsSubset %in% modelSubset

                if (!any(use4fit)) {
                  stop(paste("no observations match", modelSubset))
                 }

                # identify all tx options for the subset(s)
                txOpts <- unique(x = txReceived[use4fit])

                levs[[ i ]] <- superset[superset %in% txOpts]
                small[i] <- moPropen[[ i ]]@predictor@propenMissing == "smallest"

                if (!suppress) {
                  cat("Fitting models for ", paste(modelSubset, collapse=" ,"),
                      "using", sum(use4fit), "patient records.\n")
                }

                # create new tx object with subset of pts
                txNew <- .newTxObj(fSet = NULL,
                                   txName = .getTxName(object = txObj),
                                   data = data[use4fit,],
                                   suppress = TRUE,
                                   verify = FALSE)

                # try to obtain fit
                res[[ nms[i] ]] <- try(expr = .newTypedFit(modelObj = moPropen[[ i ]],
                                                           data = data[use4fit,],
                                                           response = data[use4fit,txName],
                                                           txObj = txNew,
                                                           type = "moPropen",
                                                           suppress = suppress),
                                        silent = TRUE)
  
                if (is(object = res[[ nms[i] ]], class2 = "try-error")) {
                  # convert tx to factor and try again
                  cat("converting response to factor and trying again\n")
                  res[[ nms[i] ]] <- .newTypedFit(modelObj = moPropen[[ i ]],
                                                  data = data[use4fit,],
                                                  response = factor(x = data[use4fit,txName]),
                                                  type = "moPropen",
                                                  txObj = txNew,
                                                  suppress = suppress)
                }

              }

              # store as a TypedFit_SubsetList
              result <- new(Class = "TypedFit_SubsetList",
                            txObj,
                            fits = new("SubsetList", res))

              return( new(Class = "PropensityFit_SubsetList",
                          "small" = small,
                          "levs" =  levs,
                          result) )
            })

#' @rdname PropensityFit_SubsetList-methods
setMethod(f = "coef",
          signature = c(object = "PropensityFit_SubsetList"),
          definition = function(object, ...) {
              res <- callNextMethod()

              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moPropen" ]]
              }
              return( res2 )
            })

#' @rdname PropensityFit_SubsetList-methods
setMethod(f = "fitObject",
          signature = c(object = "PropensityFit_SubsetList"),
          definition = function(object, ...) {
              res <- callNextMethod()

              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moPropen" ]]
              }
              return( res2 )
            })

#' @rdname PropensityFit_SubsetList-methods
setMethod(f = ".predictAll",
          signature = c(object = "PropensityFit_SubsetList",
                        newdata = "data.frame"),
          definition = function(object, 
                                newdata,  
                                suppress = TRUE) {

              # create tx object for new data
              txNew <- .newTxObj(fSet = .getSubsetRule(object = object@txInfo),
                                 txName = .getTxName(object = object@txInfo),
                                 data = newdata,
                                 suppress = TRUE,
                                 verify = FALSE)

              # extract subset identification for new data
              ptsSubset <- .getPtsSubset(object = txNew)

              # extract original subsets and superset
              subsets <- .getSubsets(object = object@txInfo)
              superset <- .getSuperset(object = object@txInfo)

              result <- matrix(data = 0.0,
                               nrow = nrow(x = newdata),
                               ncol = length(x = superset),
                               dimnames = list(NULL, superset))

              # track individuals not included in modeling
              # (i.e. singletons not included)
              notIncluded <- rep(x = TRUE, times = nrow(x = newdata))

              for (i in 1L:length(x = object@fits)) {

                # extract subsets included in regression
                nm <- names(x = object@fits)[i]
                nmSubsets <- unlist(x = strsplit(x = nm, split = ","))

                # identify any new pts in the subset(s)
                usePts <- ptsSubset %in% nmSubsets
                if (!any(usePts) ) next

                notIncluded[usePts] <- FALSE

                # call predict method
                mm <- predict(object = object@fits[[ i ]], 
                              newdata = newdata[usePts,])

                # ensure prediction is a matrix object
                if (!is.matrix(x = mm)) mm <- matrix(data = mm, ncol = 1L)

                # ensure they are positive numerics between 0 and 1
                if (is.character(x = mm[1L])) {
                  stop("propensities returned as characters")
                }

                if (any(mm < -1.5e-8, na.rm = TRUE)) {
                  stop("cannot have negative probabilities")
                }

                if (any(mm > {1.0 + 1.5e-8})) {
                  stop("cannot have probabilities > 1")
                }

                # retrieve levels contained in the subset(s)
                levs <- object@levs[[ i ]]

                # ensure that all levels are represented in prediction matrix
                if (ncol(x = mm) != length(x = levs)) {

                  correction <- 1.0 - rowSums(x = mm)

                  if (object@small[i]) {
                    if (!suppress ) {
                      cat("assumed missing prediction for", levs[1L],"\n")
                    }
                    mm <- cbind(correction, mm)
                  } else {
                    if (!suppress ) {
                      cat("assumed missing prediction for", 
                          levs[length(x = levs)],"\n")
                    }
                    mm <- cbind(mm, correction)
                  }

                }

                cols <- match(x = levs, table = superset)

                result[usePts, cols] <- mm
              }

              # set prediction to 1 for pts with only 1 tx option
              if (any(notIncluded)) {
                # if individuals not included in modeling, they have only
                # 1 tx options available and the subset received only 1 tx
                # identify the subsets and set P(A|X) = 1 for these
                for (i in 1L:length(x = subsets)) {
                  if (length(x = subsets[[ i ]]) != 1L ) next
                  reset <- {ptsSubset == names(x = subsets)[i]} & notIncluded
                  if (any(reset)) {
                    colMatch <- match(x = subsets[[ i ]][1L], table = superset)
                    result[reset, colMatch] <- 1.0
                    notIncluded[reset] <- FALSE
                  }
                }
                if (any(notIncluded)) stop("contact developer code 201")
              }

              return( result )

            })

#' @rdname PropensityFit_SubsetList-methods
setMethod(f = "propen",
          signature = c(object = "PropensityFit_SubsetList"),
          definition = function(object, ...) {
              return( fitObject(object = object) )
            })

#' @rdname PropensityFit_SubsetList-methods
setMethod(f = "summary",
          signature = c(object = "PropensityFit_SubsetList"),
          definition = function(object, ...) {
              res <- callNextMethod()

              res2 <- list()
              for (i in 1L:length(x = res)) {
                res2[[ names(x = res)[i] ]] <- res[[ i ]][[ "moPropen" ]]
              }
              return( res2 )
            })
