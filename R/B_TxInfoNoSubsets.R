# October 24, 2018

#' Class \code{TxInfoNoSubsets}
#'
#' Class \code{TxInfoNoSubsets} extends class \code{TxObj} to indicate that
#' \@txInfo is of class \code{TxInfoBasic} and thus no subsets were identified.
#'
#' @name TxInfoNoSubsets-class
#'
#' @slot txInfo A TxInfoBasic object
#'
#' @include B_TxObj.R B_TxInfoBasic.R
#'
#' @keywords internal
setClass(Class = "TxInfoNoSubsets",
         slots = c(txInfo = "TxInfoBasic"),
         contains = c("TxObj"))

##########
# METHODS
##########
#' Methods Available for Objects of Class \code{TxInfoNoSubsets}
#'
#' @name TxInfoNoSubsets-methods
#'
#' @keywords internal
NULL

#' @rdname newTxObj
setMethod(f = ".newTxObj",
          signature = c(fSet = "NULL",
                        txName = "character"),
          definition = function(fSet,
                                txName,
                                data,
                                suppress,
                                verify = TRUE) {

            if (length(x = txName) != 1L) stop("txName must be of length 1")

            txVec <- tryCatch(data[,txName],
                              error = function(e) {
                                        cat(e$message, "\n")
                                        stop(paste(txName, "not found in data"))
                                        return( e )
                                      })

            if (!is.integer(x = txVec) && !is.factor(x = txVec) && 
                !any(is.na(x = txVec))) {
              if (is.character(x = txVec)) {
                txVec <- factor(x = txVec)
              } else {
                if (!isTRUE(all.equal(target = txVec, 
                                      current = round(x = txVec)))) {
                  stop("treatment variable must be integer or factor")
                }
                txVec <- as.integer(x = round(x = txVec))
              }
            }

            if (is.factor(x = txVec)) {
              txInfo <- new(Class = "TxInfoFactor",
                            txName = txName,
                            superset = levels(x = txVec))
            } else {
              txInfo <- new(Class = "TxInfoInteger",
                            txName = txName,
                            superset = sort(x = unique(x = txVec)))
            }
            result <- new(Class = "TxObj", "txInfo" = txInfo)
            result <- new(Class = "TxInfoNoSubsets", result)

            return( result )
          })
