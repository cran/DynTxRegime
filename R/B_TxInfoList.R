# October 24, 2018

.validity_TxInfoList <- function(object) {

  if (length(x = object@txInfo) <= 1L) {
    msg <- "list is of length 0 or 1"
    return( msg )
  }

  for (i in 1L:length(x = object@txInfo)) {
    if (!is(object = object@txInfo[[i]], class2 = "TxObj")) {
      msg <- "all elements of TxInfoList must be TxObj"
      return( msg )
    }
  }

  return( TRUE )

}

#' Class \code{TxInfoList}
#'
#' Class \code{TxInfoList} extends class \code{TxObj} to indicate that
#'   \@txInfo is of class \code{List}. Each element of that list corresponds
#'   to a decision point. All methods called with this object throw errors.
#'
#' @name TxInfoList
#'
#' @slot txInfo A List object
#'
#' @include B_TxObj.R B_TxInfoWithSubsets.R B_TxInfoNoSubsets.R
#'
#' @keywords internal
setClass(Class = "TxInfoList",
         slots = c("txInfo" = "List"),
         contains = c("TxObj"),
         prototype = prototype(txInfo = new("List",list("a"=0))),
         validity = .validity_TxInfoList)

#' @rdname DynTxRegime-internal-api
setMethod(f = "initialize", 
          signature = c(.Object = "TxInfoList"),
          definition = function(.Object, ...) {
              lst <- as.list(...)
              as(object = .Object@txInfo, Class = "List") <- new("List", lst)
              validObject(object = .Object)
              return( .Object )
            })


##########
# METHODS
##########
#' Methods Available for Objects of Class \code{TxInfoList}
#'
#' @name TxInfoList-methods
#'
#' @keywords internal
NULL
          
#' @rdname newTxObj
setMethod(f = ".newTxObj",   
          signature = c(fSet = "list", 
                        txName = "list"), 
          definition = function(fSet, txName, data, suppress, verify = TRUE) { 

               nDP <- length(x = txName)

               if (length(x = fSet) != nDP) {
                 stop("lengths of txName and fSet do not agree")
               }

               result <- list()

               for (i in 1L:nDP) {

                 if (!suppress && nDP > 1L) cat("Decision point", i, "\n")

                 result[[ i ]] <- .newTxObj(fSet = fSet[[ i ]], 
                                            txName = txName[[ i ]], 
                                            data = data,
                                            suppress = suppress,
                                            verify = verify)
               }

               if (nDP > 1L) {
                 result <- new("TxInfoList", result)
               } else {
                 result <- result[[ 1L ]]
               }

               return( result )
             })

#' @rdname newTxObj
setMethod(f = ".newTxObj",   
          signature = c(fSet = "NULL",
                        txName = "list"), 
          definition = function(fSet, txName, data, suppress, verify = TRUE) { 

               nDP <- length(x = txName)

               result <- list()

               for (i in 1L:nDP) {

                 if (!suppress && nDP > 1L) cat("Decision point", i, "\n")

                 result[[ i ]] <- .newTxObj(fSet = NULL, 
                                            txName = txName[[ i ]], 
                                            data = data,
                                            suppress = suppress,
                                            verify = verify)
               }

               if (length(x = result) == 1L) {
                 result <- result[[ 1L ]]
               } else {
                 result <- new("TxInfoList", result)
               }

               return( result )

               })

#' \code{.getPtsSubset(object)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getPtsSubset",
          signature = c(object = "TxInfoList"), 
          definition = function(object) { stop("not allowed") })
          
#' \code{.getSingleton(object)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getSingleton",
          signature = c(object = "TxInfoList"), 
          definition = function(object) { stop("not allowed") })

#' \code{.getSubsetRule(object)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getSubsetRule",
          signature = c(object = "TxInfoList"), 
          definition = function(object) { stop("not allowed") })

#' \code{.getSubsets(object)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getSubsets", 
          signature = c(object = "TxInfoList"), 
          definition = function(object) { stop("not allowed") })

#' \code{.getSuperset(object)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getSuperset",
          signature = c(object = "TxInfoList"),
          definition = function(object) { stop("not allowed") })

#' \code{.getTxName(object)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getTxName",
          signature = c(object = "TxInfoList"),
          definition = function(object) { stop("not allowed") })

#' \code{.validTx(object, txVec)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".validTx",
          signature = c(object = "TxInfoList",
                        txVec = "ANY"),
          definition = function(object) { stop("not allowed") })

#' \code{.compareTx(object, vec1, vec2)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".compareTx",
          signature = c(object = "TxInfoList",
                        vec1 = "ANY",
                        vec2 = "ANY"),
          definition = function(object) { stop("not allowed") })

#' \code{.convertTx(object, txVec)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".convertTx",
          signature = c(object = "TxInfoList",
                        txVec = "ANY"),
          definition = function(object) { stop("not allowed") })

#' \code{.getLevels(object, txVec)}
#'   not allowed.
#'
#' @rdname TxInfoList-methods
setMethod(f = ".getLevels",
          signature = c(object = "TxInfoList",
                        txVec = "ANY"),
          definition = function(object) { stop("not allowed") })
