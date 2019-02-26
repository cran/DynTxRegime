# October 24, 2018

#' Class TxInfoInteger
#'
#' Class \code{TxInfoInteger} extends \code{TxInfoBasic} to identify treatments 
#' as integer.
#'
#' @name TxInfoInteger-class
#'
#' @include B_TxInfoBasic.R
#'
#' @keywords internal
setClass(Class = "TxInfoInteger",
         slots = c(superset = "integer"),
         contains = c("TxInfoBasic"),
         prototype = prototype(superset = integer(), txName = NULL))

##########
# METHODS
##########

#' Methods Available for Objects of Class \code{TxInfoInteger}
#'
#' @name TxInfoInteger-methods
#'
#' @keywords internal
NULL

#' \code{.compareTx(object, vec1, vec2)}
#'   compares vec1 and vec2 to identify equivalent elements.
#'
#' @rdname TxInfoInteger-methods
setMethod(f = ".compareTx",
          signature = c(object = "TxInfoInteger",
                        vec1 = "integer",
                        vec2 = "integer"),
          definition = function(object, vec1, vec2) {
              if (length(x = vec1) != length(x = vec2)) {
                stop("vec1 and vec2 must be of equivalent length")
              }
              ind <- {vec1 == vec2} |
                     {is.na(x = vec1) & is.na(x = vec2)}
              return( ind )
            })

#' \code{.convertFromBinary(txObj, txVec)}
#'   converts a -1/1 Tx to user provided tx coding
#'
#' @rdname TxInfoInteger-methods
setMethod(f = ".convertFromBinary",
          signature = c("txObj" = "TxInfoInteger"),
          definition = function(txObj, txVec, ...){

              optVec <- callNextMethod()

              optVec <- .convertTx(object = txObj, txVec = optVec)

              return( optVec )
            })

#' \code{.compareTx(object, vec1, vec2)}
#'   converts txVec to factor.
#'
#' @rdname TxInfoInteger-methods
setMethod(f = ".convertTx",
          signature = c(object = "TxInfoInteger",
                        txVec = "ANY"),
          definition = function(object, txVec) {
              if (!is.numeric(x = txVec)) {
                stop("inappropriate vector provided to convertTx")
              }
              temp <- as.integer(x = round(x = txVec, digits = 0L))
              if (any(!(temp %in% c(object@superset, NA)))) {
                stop("inappropriate vector provided to convertTx")
              }
              return( temp )
            })

#' \code{.getLevels(object, txVec)}
#'   determines tx levels contains in txVec.
#'
#' @rdname TxInfoInteger-methods
setMethod(f = ".getLevels",
          signature = c(object = "TxInfoInteger",
                        txVec = "integer"),
          definition = function(object, txVec) {
              res <- unique(x = txVec)
              res <- sort(x = res[!is.na(x = res)])
              return( res )
            })
