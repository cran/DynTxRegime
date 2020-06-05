# October 24, 2018

.validity_TxInfoBasic <- function(object) {

  # the tx variable must be identified
  if (nchar(x = object@txName) == 0L) {
    return( "txName must be specified" )
  }

  # the superset must include at least one tx
  if (length(x = object@superset) == 0L) {
    return( "superset must be specified" )
  }

  return( TRUE )
}

#' Class \code{TxInfoBasic}
#'
#' Class \code{TxInfoBasic} stores basic treatment information.
#'
#' @name TxInfoBasic-class
#'
#' @slot superset A vector of all possible tx options.
#' @slot txName A character - column header of data.frame that contains tx 
#'       variable
#'
#' @keywords internal
setClass(Class = "TxInfoBasic",
         slots = c(superset = "ANY",
                   txName   = "character"),
         prototype = prototype(superset = NA, txName = NULL),
         validity = .validity_TxInfoBasic)

##########
# GENERICS
##########

#' Compare Equivalence of Provided Treatment Vectors
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".compareTx",
           def = function(object, vec1, vec2) { 
               standardGeneric(f = ".compareTx")
             })

#' Convert a -1/1 Tx to User Provided Tx
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".convertFromBinary",
           def = function(txObj, ...) {  
               standardGeneric(f = ".convertFromBinary")  
             })

#' Convert a User Provided Tx Variable to Binary -1/1
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".convertToBinary",
           def = function(txObj, ...) {  
               standardGeneric(f = ".convertToBinary")  
             })

#' Convert Provided Treatment Vector to Appropriate Class
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".convertTx",
           def = function(object, txVec) { standardGeneric(f = ".convertTx") })

#' Get Treatment Levels
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getLevels",
           def = function(object, txVec) { standardGeneric(f = ".getLevels") })

#' Retrieve Superset
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getSuperset",
           def = function(object) { standardGeneric(f = ".getSuperset") })

#' Retrieve Treatment Variable Name
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getTxName",
           def = function(object) { standardGeneric(f = ".getTxName") })

#' Ensure Validity of Provided Treatment Vector
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".validTx",
           def = function(object, txVec) { standardGeneric(f = ".validTx") })

##########
# METHODS
##########

#' Methods Available for Objects of Class \code{TxInfoBasic}
#'
#' @name TxInfoBasic-methods
#'
#' @keywords internal
NULL

#' \code{.compareTx(object, vec1, vec2)}
#'   not allowed
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".compareTx",
          signature = c(object = "TxInfoBasic",
                        vec1 = "ANY",
                        vec2 = "ANY"),
          definition = function(object, vec1, vec2) { stop("not allowed") })

#' \code{.convertFromBinary(txObj, txVec)}
#'   converts a -1/1 Tx to user provided tx coding
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".convertFromBinary",
          signature = c("txObj" = "TxInfoBasic"),
          definition = function(txObj, txVec, ...){

              superset <- .getSuperset(object = txObj)
              optVec <- rep(x = NA, times = length(x = txVec))

              txVec[is.na(x = txVec)] <- 0.0

              optVec[txVec >  0.5] <- superset[2L]
              optVec[txVec < -0.5] <- superset[1L]

              return( optVec )
            })

#' \code{.convertToBinary(txObj, data)}
#'   converts user specified tx variable to binary -1/1
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".convertToBinary",
          signature = c("txObj" = "TxInfoBasic"),
          definition = function(txObj, ..., txVec){

              superset <- .getSuperset(object = txObj)
              if (length(x = superset) != 2L) stop("non-binary tx")

              newTx <- rep(x = -1.0, times = length(x = txVec))
              nonBase <- txVec == superset[2L]
              newTx[nonBase] <- 1.0

              return( newTx )
            })

#' \code{.convertTx(object, txVec)}
#'   not allowed
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".convertTx",
          signature = c(object = "TxInfoBasic",
                        txVec = "ANY"),
          definition = function(object, txVec) { stop("not allowed") })

#' \code{.getLevels(object, txVec)}
#'   not allowed
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".getLevels",
          signature = c(object = "TxInfoBasic",
                        txVec = "ANY"),
          definition = function(object, txVec) { stop("not allowed") })

#' \code{.getSuperset(object)}
#'   retrieves superset information
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".getSuperset",
          signature = c(object = "TxInfoBasic"),
          definition = function(object) { return( object@superset ) })

#' \code{.getTxName(object)}
#'   retrieve tx variable name
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".getTxName",
          signature = c(object = "TxInfoBasic"),
          definition = function(object) { return( object@txName ) })

#' \code{.validTx(object, txVec)}
#'   ensures all elements in txVec are allowed by superset
#'
#' @rdname TxInfoBasic-methods
setMethod(f = ".validTx",
          signature = c(object = "TxInfoBasic",
                        txVec = "ANY"),
          definition = function(object, txVec) {
              if (!all(txVec %in% object@superset)) {
                stop("tx value not in superset")
              }
              return( )
            })
