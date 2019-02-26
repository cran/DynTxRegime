# October 24, 2018

#' Class \code{TxInfoWithSubsets}
#'
#' Class \code{TxInfoWithSubsets} extends class \code{TxObj} to indicate that
#' \@txInfo is of class \code{TxInfoSubset} and thus subsets were identified.
#'
#' @name TxInfoWithSubsets-class
#'
#' @slot txInfo A TxSubset object
#'
#' @include B_TxObj.R B_TxSubset.R B_TxSubsetFactor.R B_TxSubsetInteger.R
#'
#' @keywords internal
setClass(Class = "TxInfoWithSubsets",
         slots = c(txInfo = "TxSubset"),
         contains = c("TxObj"),
         prototype = prototype(txInfo = new("TxSubset")))

##########
# METHODS
##########
#' Methods Available for Objects of Class \code{TxInfoWithSubsets}
#'
#' @name TxInfoWithSubsets-methods
#'
#' @keywords internal
NULL

#' @rdname newTxObj
setMethod(f = ".newTxObj",   
          signature = c(fSet = "function",
                        txName = "character"), 
          definition = function(fSet, 
                                  txName, 
                                  data, 
                                  suppress, 
                                  verify = TRUE) {

            if (length(x = txName) != 1L) stop("txName must be of length 1")
            if (nchar(x = txName) == 0L ) stop("txName must be provided")

            txVec <- tryCatch(expr = data[,txName],
                              error = function(e) {
                                        stop(paste(txName, "not found in data"))
                                        return( e )
                                      })

            if (!is.integer(x = txVec) && !is.factor(x = txVec) && 
                !any(is.na(x = txVec))) {
              if (is.character(x = txVec)) {
                data[,txName] <- factor(x = txVec)
                txVec <- data[,txName]
              } else {
                tmp <- round(x = txVec)
                if (!isTRUE(x = all.equal(target = txVec, current = tmp))) {
                  stop("treatment variable must be integer or factor")
                }
                data[,txName] <- as.integer(round(x = data[,txName]))
                txVec <- data[,txName]
              }
            }

            if (is.factor(x = txVec)) {
              ss <- levels(x = txVec)
            } else {
              ss <- sort(x = unique(x = txVec))
            }

            txInfo <- .newTxSubset(fSet = fSet,  
                                   superset = ss,  
                                   txName = txName,  
                                   data = data,  
                                   verify = verify,
                                   suppress = suppress)

            result <- new("TxInfoWithSubsets", "txInfo" = txInfo)

            return( result )
          })

#' \code{.getPtsSubset(object)}
#'   retrieves subset name to which each pt is a member. Method dispatched
#'   depends on class of @txInfo.
#'
#' @rdname TxInfoWithSubsets-methods
setMethod(f = ".getPtsSubset",
          signature = c(object = "TxInfoWithSubsets"), 
          definition = function(object) { 
              return( .getPtsSubset(object@txInfo) ) 
            })
          
#' \code{.getSingleton(object)}
#'   retrieves T/F indicating if >1 tx is available to each pt. Method dispatched
#'   depends on class of @txInfo.
#'
#' @rdname TxInfoWithSubsets-methods
setMethod(f = ".getSingleton",
          signature = c(object = "TxInfoWithSubsets"), 
          definition = function(object) { 
              return( .getSingleton(object@txInfo) ) 
            })

#' \code{.getSubsetRule(object)}
#'   retrieves feasible tx function. Method dispatched
#'   depends on class of @txInfo.
#'
#' @rdname TxInfoWithSubsets-methods
setMethod(f = ".getSubsetRule",
          signature = c(object = "TxInfoWithSubsets"), 
          definition = function(object) { 
              return( .getSubsetRule(object@txInfo) ) 
            })

#' \code{.getSubsets(object)}
#'   retrieves feasible tx information. Method dispatched
#'   depends on class of @txInfo.
#'
#' @rdname TxInfoWithSubsets-methods
setMethod(f = ".getSubsets", 
          signature = c(object = "TxInfoWithSubsets"), 
          definition = function(object) {
              return( .getSubsets(object@txInfo) ) 
            })
