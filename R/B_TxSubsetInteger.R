# September 26, 2018
#
# Class extends TxSubset and TxSubsetInteger

#' Class TxSubsetInteger
#'
#' Class \code{TxSubsetInteger} stores subset information for treatment
#'
#' @name TxSubsetInteger-class
#' @rdname TxSubsetInteger-class
#'
#' @include B_TxSubsetInteger.R B_TxSubset.R
#'
#' @keywords internal
setClass(Class = "TxSubsetInteger",
         contains = c("TxInfoInteger", "TxSubset"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{TxSubsetInteger}
#'
#' @name TxSubsetInteger-methods
#'
#' @keywords internal
NULL

#' @rdname newTxSubset
setMethod(f = ".newTxSubset",
          signature = c(fSet = "function",
                        superset = "integer"),
          definition = function(fSet, 
                                superset, 
                                txName, 
                                data,
                                verify, ...){

              obj2 <- callNextMethod()

              obj1 <- new(Class = "TxInfoInteger",
                          txName = txName, 
                          superset = superset)

              res <- new(Class = "TxSubsetInteger", obj1, obj2)

              return( res )
            })

#' \code{.convertFromBinary(txObj, txVec)}
#'   converts a -1/1 Tx to user provided tx coding.
#'   Call method defined for \code{TxSubset}.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".convertFromBinary",
          signature = c("txObj" = "TxSubsetInteger"),
          definition = function(txObj, txVec, ...){

              optVec <- .convertFromBinary(txObj = as(object  = txObj,
                                                      Class = "TxSubset"),
                                           txVec = txVec, ...)

              optVec <- .convertTx(object = txObj, txVec = optVec)

              return( optVec )
            })

#' \code{.convertToBinary(txObj, data)}
#'   converts user specified tx variable to binary -1/1.
#'   Call method defined for \code{TxSubset}.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".convertToBinary",
          signature = c("txObj" = "TxSubsetInteger"),
          definition = function(txObj, ..., txVec, data){

              # default all individuals to base level
              newTx <- rep(x = -1.0, times = nrow(x = data))

              # subsets identified through fSet
              subsets <- .getSubsets(object = txObj)

              # subset to which each individuals belongs
              ptsSubsets <- .getPtsSubset(object = txObj)

              for (i in 1L:length(x = subsets)) {

                if (length(x = subsets[[ i ]]) == 1L) {
                  # if the subset is a singlet determine if training data
                  # received tx in accordance with set
                  txInData <- sort(x = unique(x = data[,.getTxName(object = txObj)]))

                  # if there is only 1 tx in data, keep default -1 value
                  if (length(x = txInData) == 1L) next

                  # if there is > 1 tx in data, set appropriately
                  usePts <- ptsSubsets == names(x = subsets)[i]
                  nonBase <- txVec == txInData[2L]
                  newTx[usePts & nonBase] <- 1.0

                } else if (length(x = subsets[[ i ]]) == 2L) {
                  # if the subset is a binary set appropriately
                  usePts <- ptsSubsets == names(x = subsets)[i]
                  nonBase <- txVec == subsets[[ i ]][2L]
                  newTx[usePts & nonBase] <- 1.0
                } else { 
                  stop("non-binary tx", call. = FALSE)
                }
              }

              return( newTx )
            })

#' \code{.getPtsSubset(object)}
#'  retrieve subset name for which each pt is a member.
#'   Call method defined for \code{TxSubset}.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".getPtsSubset",
          signature = c(object = "TxSubsetInteger"),
          definition = function(object) { 
              return( .getPtsSubset(object = as(object  = object,
                                                Class = "TxSubset")) )
            })

#' \code{.getSingleton(object)}
#'   retrieve T/F indicator of only 1 tx option available to each pt.
#'   Call method defined for \code{TxSubset}.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".getSingleton",
          signature = c(object = "TxSubsetInteger"),
          definition = function(object) { 
              return( .getSingleton(object = as(object  = object,
                                                Class = "TxSubset")) )
            })

#' \code{.getSubsetRule(object)}
#'   retrieve feasible set identification rule.
#'   Call method defined for \code{TxSubset}.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".getSubsetRule",
          signature = c(object = "TxSubsetInteger"),
          definition = function(object) { 
              return( .getSubsetRule(object = as(object  = object,
                                                 Class = "TxSubset")) )
            })

#' \code{.getSubsets(object)}
#'   retrieve subset names and tx options.
#'   Call method defined for \code{TxSubset}.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".getSubsets",
          signature = c(object = "TxSubsetInteger"),
          definition = function(object) { 
               return( .getSubsets(object = as(object  = object,
                                               Class = "TxSubset")) )
            })

#' \code{.compareTx(object, vec1, vec2)}
#'   compares vec1 and vec2 to identify equivalent elements.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".compareTx",
          signature = c(object = "TxSubsetInteger",
                        vec1 = "integer",
                        vec2 = "integer"),
          definition = function(object, vec1, vec2) {
               return( .compareTx(object = as(object  = object,
                                              Class = "TxInfoInteger"), 
                                  vec1 = vec1,
                                  vec2 = vec2) )
            })

#' \code{.compareTx(object, vec1, vec2)}
#'   converts txVec to factor.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".convertTx",
          signature = c(object = "TxSubsetInteger",
                        txVec = "ANY"),
          definition = function(object, txVec) {
               return( .convertTx(object = as(object  = object,
                                              Class = "TxInfoInteger"), 
                                  txVec = txVec) )
            })

#' \code{.getLevels(object, txVec)}
#'   determines tx levels contains in txVec.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".getLevels",
          signature = c(object = "TxSubsetInteger",
                        txVec = "integer"),
          definition = function(object, txVec) {
               return( .getLevels(object = as(object  = object,
                                              Class = "TxInfoInteger"), 
                                  txVec = txVec) )
            })

#' \code{.getSuperset(object)}
#'   retrieves superset. Uses method defined for TxInfoInteger objects.
#'
#' @rdname TxSubsetInteger-methods 
setMethod(f = ".getSuperset",
          signature = c(object = "TxSubsetInteger"),
          definition = function(object) { 
              return( .getSuperset(object = as(object = object, 
                                               Class = "TxInfoInteger")) )
            })

#' \code{.getTxName(object)}
#'   retrieves tx variable name. Uses method defined for TxInfoInteger objects.
#
#' @rdname TxSubsetInteger-methods 
setMethod(f = ".getTxName",
          signature = c(object = "TxSubsetInteger"),
          definition = function(object) { 
              return( .getTxName(object = as(object = object, 
                                             Class = "TxInfoInteger")) )
            })

#' \code{.validTx(object, txVec)}
#'   ensures all elements in txVec are allowed by superset.
#'
#' @rdname TxSubsetInteger-methods
setMethod(f = ".validTx",
          signature = c(object = "TxSubsetInteger",
                        txVec = "ANY"),
          definition = function(object, txVec) {
              return( .validTx(object = as(object = object, Class = "TxSubset"),
                               txVec = txVec) )
            })
