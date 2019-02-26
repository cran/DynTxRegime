# October 26, 2018

#' Class \code{ClassificationFit}
#'
#' Class \code{ClassificationFit} combines a \code{TypedFit} object and a 
#'   \code{TxInfoNoSubsets} object to define a classification regression result
#'   when subsets are not identified.
#'
#' @name ClassificationFit-class
#' @docType class
#'
#' @keywords internal
setClass(Class = 'ClassificationFit',
         contains = c("TypedFit", "TxInfoNoSubsets"))

##########
## GENERICS
##########

#' Complete a Classification Regression Step
#'
#' Methods dispatch appropriate typed fit methods based on the modeling
#'   object specified by the user and the feasible tx definitions.
#'   The value object returned depends on the underlying typed fit method.
#'
#' @name newClassificationFit
#' @docType methods
setGeneric(name = ".newClassificationFit",
           def = function(moClass, txObj, ...) {
               standardGeneric(f = ".newClassificationFit")
             })

#' Retrieve Classification Regression Analysis
#'
#' Method retrieves the value object returned by the user specified
#'   classification regression modeling object(s). Exact structure of the 
#'   returned object will vary.
#'
#' @name classif
#' @docType methods
#' @export classif
#'
#' @param object Value object returned from a method that uses classification
#'   regression
#' @param ... Ignored.
#'
setGeneric(name = "classif",
           def = function(object, ...) { standardGeneric(f = "classif") })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{ClassificationFit}
#'
#' @name ClassificationFit-methods
#'
#' @keywords internal
NULL

#' Complete a Classification Regression Step when Tx Subsets are not
#'  Indicated
#'
#' @param moClass modeling object(s) defining the classification regression
#' @param txObj TxObj defining the tx feasible sets
#' @param contrast vector of contrasts
#' @param data data.frame of covariates and tx received
#' @param suppress logical indicating user's screen printing preference
#' @param ... additional arguments. Ignored.
#'
#' @rdname newClassificationFit
#'
#' @importFrom modelObj solverArgs solverArgs<-
#'
#' @keywords internal
setMethod(f = ".newClassificationFit",
          signature = c(moClass = "modelObj",
                        txObj   = "TxInfoNoSubsets"),
          definition = function(moClass, 
                                txObj,  
                                response,  
                                data,  
                                suppress, ...) {

              if (!suppress) cat("\nClassification Analysis\n")

              # normalize weights
              data$wgt <- data$wgt / sum(data$wgt)

              typedFit <- .newTypedFit(modelObj = moClass,
                                       txObj = txObj,
                                       response = response,
                                       data = data,
                                       type = "moClass",
                                       suppress = suppress)

              return( new(Class = "ClassificationFit",
                          typedFit,
                          txObj) )
            })

#' \code{classif(object)}
#'   retrieves the object returned by the classification regression 
#'   analysis. Calls method defined for \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "classif",
          signature = c(object = "ClassificationFit"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object, Class = "TypedFit"))$moClass )
            })

#' \code{coef(object)} 
#'   calls coef method defined for the object returned by the
#'   classification regression analysis. Calls method defined for 
#'   \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "coef",
          signature = c(object = "ClassificationFit"),
          definition = function(object, ...) {
              return( coef(object = as(object = object, 
                                       Class = "TypedFit"), ...)$moClass )
            })

#' \code{fitObject(object)}
#'   retrieves the object returned by the classification regression 
#'   analysis.  Calls method defined for \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "fitObject",
          signature = c(object = "ClassificationFit"),
          definition = function(object, ...) {
              return( fitObject(object = as(object = object,  
                                            Class = "TypedFit"), ...)$moClass )
            })

#' \code{plot(x, ...)}
#'   calls plot method defined for the object returned by the
#'   classification regression analysis.  Calls method defined for 
#'   \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "plot",
          signature = c(x = "ClassificationFit"),
          definition = function(x, suppress=FALSE, ...) {
              plot(x = as(object = x, Class = "TypedFit"),
                   suppress = suppress, ...)
            })

#' \code{predict(object, ...)}
#'   calls predict method defined for the object returned by the
#'   classification regression analysis.  Calls method defined for 
#'   \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "predict",
          signature = c(object = "ClassificationFit"),
          definition = function(object, ...) {

              return( predict(object = as(object = object, 
                                          Class = "TypedFit"), ...) )

            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts optimal treatment
#'
#' @rdname ClassificationFit-methods
setMethod(f = ".predictAll",
          signature = c(object = "ClassificationFit",
                        newdata = "data.frame"),
          definition = function(object, newdata, ...) {

              superset <- .getSuperset(object = object@txInfo)

              pred <- predict(object = object, newdata = newdata)

              isBase <- pred %in% c(0,"0")

              dFunc <- NA

              optimalTx <- rep(x = NA, times = nrow(x = newdata))

              optimalTx[ isBase] <- superset[1L]
              optimalTx[!isBase] <- superset[2L]

              optimalTx <- .convertTx(object = object@txInfo, txVec = optimalTx)

              return( list("optimalTx"    = optimalTx,
                           "decisionFunc" = dFunc) )
            })

#' \code{print(x)} 
#'   calls the print method defined for the object returned by the
#'   classification regression analysis.  Calls method defined for 
#'   \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "print",
          signature = c(x = "ClassificationFit"),
          definition = function(x, ...) {
              print(x = as(object = x, Class = "TypedFit"))
            })

#' \code{show(object)}
#'   calls the show method defined for the object returned by the
#'   classification regression analysis.  Calls method defined for 
#'   \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "show",
          signature = c(object = "ClassificationFit"),
          definition = function(object) {
              show(object = as(object = object, Class = "TypedFit"))
            })

#' \code{summary(object)}
#'   calls the summary method defined for the object returned by the
#'   classification regression analysis.  Calls method defined for 
#'   \code{TypedFit}.
#'
#' @rdname ClassificationFit-methods
setMethod(f = "summary",
          signature = c(object = "ClassificationFit"),
          definition = function(object, ...) {
              return( summary(object = as(object = object,  
                                          Class = "TypedFit"), ...)$moClass )
            })
