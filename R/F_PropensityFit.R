# October 26, 2018

#' Class \code{PropensityFit}
#'
#' Class \code{PropensityFit} is a \code{TypedFit} identified as being
#'   for a propensity regression step.
#'
#' @name PropensityFit-class
#'
#' @slot small A logical TRUE indicates that the smallest valued tx is
#'   missing; FALSE indicates that the largest valued tx is missing
#' @slot levs A vector; the set of treatment options included in fit.
#'
#' @keywords internal
setClass("PropensityFit",
         slots = c(small = "logical",
                   levs = "vector"),
         contains = c("TypedFit", "TxInfoNoSubsets"))

##########
## GENERICS
##########

#' Complete a Propensity Regression Step
#'
#' Dispatches appropriate method for completing propensity regressions.
#'
#' @rdname newPropensityFit
#'
#' @param moPropen A modeling object
#' @param txObj A TxObj object
#' @param ...  Any optional additional input.
#' 
#' @keywords internal
setGeneric(name = ".newPropensityFit",
           def = function(moPropen, txObj, ...) {
               standardGeneric(f = ".newPropensityFit")
             } )

#' Retrieve Propensity Regression Analysis
#'
#' For statistical methods that require a propensity regression analysis,
#'   the value object returned by the modeling function(s) is retrieved.
#'
#' Methods are defined for all statistical methods implemented in DynTxRegime
#'   that use propensity regression. 
#' 
#' @name propen
#'
#' @param object A value object returned by a statistical method of DynTxRegime.
#' @param ... Ignored.
#'
#' @usage
#' propen(object, ...)
#'
#' @exportMethod propen
setGeneric(name = "propen",
           def = function(object, ...) { standardGeneric(f = "propen") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{PropensityFit}
#'
#' Methods call equivalently named methods defined for \code{TypedFit}
#'
#' @name PropensityFit-methods
#'
#' @keywords internal
NULL

#' @rdname newPropensityFit
setMethod(f = ".newPropensityFit",
          signature = c(moPropen = "modelObj",
                        txObj = "TxInfoNoSubsets"),
          definition = function(moPropen, txObj, data, suppress) {

              txName <- .getTxName(object = txObj)

              fitResult <- try(expr = .newTypedFit(modelObj = moPropen,
                                                   data = data,
                                                   response = data[,txName],
                                                   type = "moPropen",
                                                   txObj = txObj,
                                                   suppress = suppress),
                               silent = TRUE)

              if (is(object = fitResult, class2 = "try-error")) {
                cat("converting response to factor and trying again\n")
                fitResult <- tryCatch(expr = .newTypedFit(modelObj = moPropen,
                                                          data = data,
                                                          response = factor(x = data[,txName]),
                                                          type = "moPropen",
                                                          txObj = txObj,
                                                          suppress = suppress),
                                      error = function(x){
                                                print(x = x$message)
                                                stop('unable to obtain propensity fit')
                                              })
              }

              res <- new(Class = "PropensityFit",
                         "small" = moPropen@predictor@propenMissing == "smallest",
                         "levs"  = as.character(x = .getSuperset(txObj)),
                         txObj,
                         fitResult)

              return( res )

            })

#' @rdname PropensityFit-methods
setMethod(f = "coef",
          signature = c(object = "PropensityFit"),
          definition = function(object, ...) {
              return( coef(object = as(object = object, 
                                       Class = "TypedFit"), ...)$moPropen )
            })

#' @rdname PropensityFit-methods
setMethod(f = "fitObject",
          signature = c(object = "PropensityFit"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object,  
                                            Class = "TypedFit"), ...)$moPropen )
            })

#' @rdname PropensityFit-methods
setMethod(f = "plot",
          signature = c(x = "PropensityFit"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = as(object = x, Class = "TypedFit"),
                   suppress = suppress, ...)
            })

#' @rdname PropensityFit-methods
setMethod(f = "predict",
          signature = c(object = "PropensityFit"),
          definition = function(object, ...) {
              return( predict(object = as(object = object, 
                                          Class = "TypedFit"), ...)) 
            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts propensity for all tx options.
#'   Returns a matrix of propensities predicted for all tx. 
#'
#' @rdname PropensityFit-methods
setMethod(f = ".predictAll",
          signature = c(object = "PropensityFit",
                        newdata = "data.frame"),
          definition = function(object, 
                                newdata,  
                                suppress = TRUE) {

              mm <- predict(object = as(object = object, Class = "TypedFit"), 
                            newdata = newdata)

              if (is.character(x = mm[1L])) {
                stop("propensities returned as characters")
              }

              if (any(mm < -1.5e-8)) {
                stop("cannot have negative probabilities")
              }

              if (any(mm > {1.0 + 1.5e-8})) {
                stop("cannot have probabilities > 1")
              }

              if (!is.matrix(x = mm)) mm <- matrix(data = mm, ncol = 1L)

              levs <- object@levs

              if (ncol(x = mm) != length(x = levs)) {

                correction <- 1.0 - rowSums(x = mm)

                if (object@small) {
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

              colnames(x = mm) <- levs

              return( mm )

            })

#' @rdname PropensityFit-methods
setMethod(f = "print",
          signature = c(x = "PropensityFit"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "TypedFit"))
            })

#' @rdname PropensityFit-methods
setMethod(f = "propen",
          signature = c(object = "PropensityFit"),
          definition = function(object, ...) {
              return( fitObject(object = object) )
            })

#' @rdname PropensityFit-methods
setMethod(f = "show",
          signature = c(object = "PropensityFit"),
          definition = function(object) {
              show(object = as(object = object, Class = "TypedFit"))
            })

#' @rdname PropensityFit-methods
setMethod(f = "summary",
          signature = c(object = "PropensityFit"),
          definition = function(object, ...) {
              return( summary(object = as(object = object,  
                                          Class = "TypedFit"), ...)$moPropen )
            })
