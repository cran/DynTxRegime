# October 24, 2018

#' Class TxInfoFactor
#'
#' Class \code{TxInfoFactor} extends \code{TxInfoBasic} to identify treatments 
#' as factor
#'
#' @name TxInfoFactor-class
#'
#' @slot superset character of all allowed tx options
#'
#' @include B_TxInfoBasic.R
#'
#' @keywords internal
setClass(Class = "TxInfoFactor",
         slots = c(superset = "character"),
         contains = c("TxInfoBasic"),
         prototype = prototype(superset = character(), txName = NULL))

##########
# METHODS
##########

#' Methods Available for Objects of Class \code{TxInfoFactor}
#'
#' @name TxInfoFactor-methods
#'
#' @keywords internal
NULL

#' \code{.compareTx(object, vec1, vec2)}
#'   compares vec1 and vec2 to identify equivalent elements.
#'
#' @rdname TxInfoFactor-methods
setMethod(f = ".compareTx",
          signature = c(object = "TxInfoFactor",
                        vec1 = "factor",
                        vec2 = "factor"),
          definition = function(object, vec1, vec2) {
              if (length(x = vec1) != length(x = vec2)) {
                stop("vec1 and vec2 must be of equivalent length")
              }
              lev1 <- levels(x = vec1)
              lev2 <- levels(x = vec2)
              levs <- sort(x = unique(x = c(lev1,lev2)))
              vec1 <- factor(x = vec1, levels = levs)
              vec2 <- factor(x = vec2, levels = levs)
              ind <- {vec1 == vec2} |
                     {is.na(x = vec1) & is.na(x = vec2)}
              return( ind )
            })

#' \code{.convertFromBinary(txObj, txVec)}
#'   converts a -1/1 Tx to user provided tx coding
#'
#' @rdname TxInfoFactor-methods
setMethod(f = ".convertFromBinary",
          signature = c("txObj" = "TxInfoFactor"),
          definition = function(txObj, txVec, ...){

              optVec <- callNextMethod()

              optVec <- .convertTx(object = txObj, txVec = optVec)

              return( optVec )
            })

#' \code{.compareTx(object, vec1, vec2)}
#'   converts txVec to factor.
#'
#' @rdname TxInfoFactor-methods
setMethod(f = ".convertTx",
          signature = c(object = "TxInfoFactor",
                        txVec = "ANY"),
          definition = function(object, txVec) {

              temp <- factor(x = txVec, levels = object@superset)
              temp2 <- factor(x = txVec)

              if (!all(levels(x = temp2) %in% object@superset)) {
                stop("inappropriate vector provided to convertTx")
              }
              return( temp )
            })

#' \code{.getLevels(object, txVec)}
#'   determines tx levels contains in txVec.
#'
#' @rdname TxInfoFactor-methods
setMethod(f = ".getLevels",
          signature = c(object = "TxInfoFactor",
                        txVec = "factor"),
          definition = function(object, txVec) {
              levs <- levels(x = txVec)
              return( levs[levs %in% txVec] )
            })
