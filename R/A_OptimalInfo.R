# October 23, 2018

setClassUnion("MatrixOrVector",
              members = c("matrix","vector"))

.validity_OptimalInfo <- function(object) {

  if (length(x = object@decisionFunc) == 1L) {
    if (!is.na(x = object@decisionFunc)) {
      return( "length 1, non-NA @decisionFunc" )
    }
  } else {
    if (!is.numeric(x = object@decisionFunc)) {
      return( "@decisionFunc is not numeric" )
    }
  }

  if (length(x = object@decisionFunc) != 1L) {
    if (is.matrix(x = object@decisionFunc)) {
      if (nrow(x = object@decisionFunc) != length(x = object@optimalTx)) {
        return( "dimensions of @decisionFunc and @optimalTx do not match" )
      }
    } else {
      if (length(x = object@decisionFunc) != length(x = object@optimalTx)) {
        return( "dimensions of @decisionFunc and @optimalTx do not match" )
      }
    }
  }

  if (length(x = object@estimatedValue) == 1L) {
    if (!is.na(x = object@estimatedValue) && 
        !is.numeric(x = object@estimatedValue)) {
      return( "@estimatedValue is not numeric" )
    }
  } else {
    if (!is.numeric(x = object@estimatedValue)) {
      return( "@estimatedValue is not numeric" )
    }
  }

  return( TRUE )
}

#' Class \code{OptimalInfo}
#'
#' Class \code{OptimalInfo} stores the estimated optimal tx, decision functions,
#' and estimated value.
#'
#' @slot optimalTx a vector of the estimated optimal tx
#' @slot estimatedValue a vector of the estimated value
#' @slot decisionFunc a vector or matrix containing the values used to determine
#'   @optimalTx (if applicable)
#'
#' @name OptimalInfo-class
setClass(Class = "OptimalInfo",
         slots = c(optimalTx      = "vector",
                   estimatedValue = "vector",
                   decisionFunc   = "MatrixOrVector"),
         prototype = list(optimalTx      = integer(),
                          estimatedValue = numeric(),
                          decisionFunc   = numeric()),
         validity = .validity_OptimalInfo)

##########
# GENERICS
##########

#' Retrieve the Estimated Value
#'
#' Retrieve the value as estimated by the statistical method.
#'
#' @name estimator
#' @exportMethod estimator
#'
#' @param x a DynTxRegime Object.
#' @param y If IQ-Learning, object of class IQLearnSS, IQLearnFS_C, 
#'    IQLearnFS_ME, or IQLearnFS_VHet
#' @param z If IQ-Learning, object of class IQLearnSS, IQLearnFS_C, 
#'    IQLearnFS_ME, or IQLearnFS_VHet
#' @param w If IQ-Learning, object of class IQLearnSS, IQLearnFS_C, 
#'    IQLearnFS_ME, or IQLearnFS_VHet
#' @param dens If IQ-Learning, one of {norm, nonpar} 
#' @param ...  Optional additional input. Ignored.
#'
#' @usage
#' estimator(x, ...)
#'
setGeneric(name = "estimator",
           def = function(x, ...) { standardGeneric(f = "estimator") })

#' Extract or Estimate the Optimal Tx and Decision Functions
#'
#' If newdata is provided, the results of the statistical method are used
#'   to estimate the decision functions and/or optimal tx. If
#'   newdata is missing, the estimated decision functions and/or optimal tx
#'   obtained for the original training data are returned.
#'
#' Methods are defined for all statistical methods implemented in DynTxRegime.
#'
#' @name optTx
#' @exportMethod optTx
#'
#' @param x a DynTxRegime Object.
#' @param newdata Optional data.frame if estimates for new patients are desired.
#' @param ...  Optional additional input.
#'
#'
#' @usage
#' optTx(x, newdata, ...)
#'
setGeneric(name = "optTx",
           def = function(x, newdata, ...) { standardGeneric(f = "optTx") })

##########
# METHODS
##########

#' Methods Available for Objects of Class \code{OptimalInfo}
#'
#' @name OptimalInfo-methods
#'
#' @keywords internal
NULL

#' \code{estimator(x)} 
#'   defines the estimated value to be the mean of the vector stored in 
#'   @estimatedValue
#'
#' @rdname OptimalInfo-methods
setMethod(f = "estimator",
          signature = c(x = "OptimalInfo"),
          definition = function(x) { 
              if (all(is.na(x = x@estimatedValue)) ) return( NA )
                return( sum(x@estimatedValue, na.rm = TRUE) / 
                                 length(x = x@estimatedValue) ) 
             })

#' \code{optTx(x)} 
#'   returns the contents of @optimalTx and @decisionFunc as a list
#'
#' @rdname OptimalInfo-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalInfo",
                        newdata = "missing"),
          definition = function(x, newdata, ...) {
              return( list("optimalTx"    = x@optimalTx,
                           "decisionFunc" = x@decisionFunc) )
            })

#' \code{optTx(x, newdata)} 
#'   returns an error
#'
#' @rdname OptimalInfo-methods
setMethod(f = "optTx",
          signature = c(x = "OptimalInfo",
                        newdata = "ANY"),
          definition = function(x, newdata, ...) { stop("not allowed") })

#' \code{print(x)}
#'   Prints a summary table of the recommended tx for the training data and the
#'   estimated value
#
#' @rdname OptimalInfo-methods 
setMethod(f = "print",
          signature = c(x = "OptimalInfo"),
          definition = function(x, ...) {
              cat("Recommended Treatments:\n")
              print(x = .table_OptimalInfo(object = x@optimalTx))
              if (!is.na(x = estimator(x = x))) {
                cat("\nEstimated value:", estimator(x = x), "\n")
              }
            })

#' \code{show(object)}
#'   Displays a summary table of the recommended tx for the training data and
#'   the estimated value
#
#' @rdname OptimalInfo-methods 
setMethod(f = "show",
          signature = c(object = "OptimalInfo"),
          definition = function(object) {
              cat("Recommended Treatments:\n")
              show(object = .table_OptimalInfo(object = object@optimalTx))
              if (!is.na(x = estimator(x = object))) {
                cat("\nEstimated value:", estimator(x = object), "\n")
              }
            })

#' \code{summary(object)}
#'   Returns a list containing a summary table of the recommended tx for the 
#'   training data and the estimated value
#
#' @rdname OptimalInfo-methods 
setMethod(f = "summary",
          signature = c(object = "OptimalInfo"),
          definition = function(object, ...) {
              res <- list()
              res[[ "optTx" ]] <- .table_OptimalInfo(object = object@optimalTx)
              if (!is.na(x = estimator(x = object))) {
                res[[ "value" ]] <- estimator(x = object)
              }
              return( res )
            })

# if optTx is a vector, assume that recommended treatment is of class
# factor and attempt to table recommended treatments. if table fails,
# recommended treatment is non-factor. Convert and table. not
# immediately converting to factor to allow for treatment options that
# are allowed but may not ever be recommended. convert value object
# returned by table() to a named vector to clean up print/show results
.table_OptimalInfo <- function(object) {

  tbl <- tryCatch(expr = table(object, useNA = "ifany"),
                  error = function(e) {
                            table(factor(object), useNA = "ifany")
                          })
  nm <- names(x = tbl)
  tbl <- as.vector(x = tbl)
  names(x = tbl) <- nm

  return( tbl )
}
