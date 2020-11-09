# October 24, 2018

#' Class \code{TxObj}
#'
#' Storage Class to group tx information under a common name.
#'
#' @name TxObj-class
#'
#' @slot txInfo Any object -- expected to be of class TxInfoBasic,
#'   TxInfoSubset, or DecisionPointList
#'
#' @include B_TxInfoBasic.R B_TxInfoFactor.R B_TxInfoInteger.R
#'
#' @keywords internal
setClass(Class = "TxObj",
         slots = c("txInfo" = "ANY"))

##########
# GENERICS
##########

#' Create \code{TxObj} Object
#'
#' Creates appropriate \code{TxObj} based on class of fSet and txName.
#'
#' @name newTxObj
#' 
#' @keywords internal
setGeneric(name = ".newTxObj",
           def = function(fSet, txName, ...) { 
               standardGeneric(f = ".newTxObj") 
             })

##########
# METHODS
##########
#' Methods Available for Objects of Class \code{TxObj}
#'
#' Methods dispatched depend on class of @txInfo.
#'
#' @name TxObj-methods
#'
#' @keywords internal
NULL

#' \code{.compareTx(object, vec1, vec2)}
#'   compares vec1 and vec2 to identify equivalent elements.
#'
#' @rdname TxObj-methods
setMethod(f = ".compareTx",
          signature = c(object = "TxObj",
                        vec1 = "ANY",
                        vec2 = "ANY"),
          definition = function(object, vec1, vec2) {
              return( .compareTx(object = object@txInfo,
                                 vec1 = vec1,
                                 vec2 = vec2) )
             })

#' \code{.convertFromBinary(txObj, txVec)}
#'   converts a -1/1 Tx to user provided tx coding
#'
#' @rdname TxObj-methods
setMethod(f = ".convertFromBinary",
          signature = c("txObj" = "TxObj"),
          definition = function(txObj, ...){
              return( .convertFromBinary(txObj = txObj@txInfo, ...) )
            })

#' \code{.convertToBinary(txObj, data)}
#'   converts user specified tx variable to binary -1/1
#'
#' @rdname TxObj-methods
setMethod(f = ".convertToBinary",
          signature = c("txObj" = "TxObj"),
          definition = function(txObj, ...){
              return( .convertToBinary(txObj = txObj@txInfo, ...) )
            })

#' \code{.compareTx(object, vec1, vec2)}
#'   converts txVec to class of original tx.
#'
#' @rdname TxObj-methods
setMethod(f = ".convertTx",
          signature = c(object = "TxObj",
                        txVec = "ANY"),
          definition = function(object, txVec) {
              return( .convertTx(object = object@txInfo, txVec = txVec) ) })

#' \code{.getLevels(object, txVec)}
#'   determines tx levels contains in txVec.
#'
#' @rdname TxObj-methods
setMethod(f = ".getLevels",
          signature = c(object = "TxObj",
                        txVec = "ANY"),
          definition = function(object, txVec){
              return( .getLevels(object = object@txInfo, txVec = txVec) )
            })

#' \code{.getSuperset(object)}
#'   retrieves superset. Uses method defined for TxInfoBasic objects.
#'
#' @rdname TxObj-methods 
setMethod(f = ".getSuperset",
          signature = c(object = "TxObj"),
          definition = function(object) {
              return( .getSuperset(object = object@txInfo) )
            })

#' \code{.getTxName(object)}
#'   retrieves tx variable name. 
#
#' @rdname TxObj-methods 
setMethod(f = ".getTxName",
          signature = c(object = "TxObj"),
          definition = function(object) {
              return( .getTxName(object = object@txInfo) )
            })


#' @rdname newTxObj
setMethod(f = ".newTxObj",
          signature = c(fSet = "ANY",
                        txName = "character"),
          definition = function(fSet, txName, ...) { stop("not allowed") })

#' \code{.validTx(object, txVec)}
#'   ensures all elements in txVec are allowed by superset. 
#'
#' @rdname TxObj-methods
setMethod(f = ".validTx",
          signature = c(object = "TxObj",
                        txVec = "ANY"),
          definition = function(object, txVec){
              return( .validTx(object = object@txInfo, txVec = txVec) )
            })
