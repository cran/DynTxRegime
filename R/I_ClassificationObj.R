# October 23, 2018

.validity_ClassificationObj <- function(object) {

  # @classif must be NA, ClassificationFit, ClassificationFit_fSet, or
  # ClassificationFit_SubsetList
  if (!is(object = object@classif, class2 = "ClassificationFit") &&
      !is(object = object@classif, class2 = "ClassificationFit_fSet") &&
      !is(object = object@classif, class2 = "ClassificationFit_SubsetList") &&
      !is.na(x = object@classif)) {
    return( "incorrect object for @classif" )
  }

  return( TRUE )
}

#' Class \code{ClassificationObj}
#'
#' Stores classification regression results under a common name.
#'
#' @name ClassificationObj-class
#' @docType class
#'
#' @slot classif ANY - required to be NA, \code{ClassificationFit}, 
#'   \code{ClassificationFit_fSet}, or \cr \code{ClassificationFit_SubsetList}.
#'
#' @include I_ClassificationFit.R I_ClassificationFit_fSet.R
#' @include I_ClassificationFit_SubsetList.R
#'
#' @keywords internal
setClass(Class = "ClassificationObj",
         slots = c(classif = "ANY"),
         prototype = list(classif = NA),
         validity = .validity_ClassificationObj)

##########
## GENERICS
##########

#' Create an Object of Class \code{ClassificationFitObj}
#'
#' Method calls .newClassificationFit() and stores the result in @classif.
#'
#' @rdname newClassificationObj
#' @docType methods
#'
#' @keywords internal
setGeneric(name = ".newClassificationObj",
           def = function(moClass, txObj, ...) {
               standardGeneric(f = ".newClassificationObj")
             })

#' Create an Object of Class \code{ClassificationFitObj}
#'
#' @inheritParams .newClassificationFit
#'
#' @rdname newClassificationObj
setMethod(".newClassificationObj",
          signature = c("moClass" = "ANY",
                        "txObj" = "ANY"),
          definition = function(moClass,
                                txObj,
                                data,
                                response,
                                suppress, ...) {
              return( new(Class = "ClassificationObj",
                          "classif" = .newClassificationFit(moClass = moClass,
                                                            txObj = txObj,
                                                            response = response,
                                                            data = data,
                                                            suppress = suppress, ...)) )
            })

#' Methods Available for Objects of Class \code{ClassificationObj}
#'
#' @name ClassificationObj-methods
#'
#' @keywords internal
NULL

#' \code{classif(object)}
#'   retrieves the regression object(s) used for a classification step.
#'   Method called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
#'
setMethod(f = "classif",
          signature = c(object = "ClassificationObj"),
          definition = function(object, ...) {
              return( classif(object = object@classif) )
            })

#' \code{coef(object)} 
#'   retrieves the coefficients estimated in a classification step. 
#'   Method called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
setMethod(f = "coef",
          signature = c(object = "ClassificationObj"),
          definition = function(object, ...) {
              return( list("classif" = coef(object = object@classif)) )
            })

#' \code{fitObject(object)}
#'   retrieves the regression objects obtained for a classification step.  
#'   Returns result as a single element list with element name
#'   'classification'. Method called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
setMethod(f = "fitObject",
          signature = c(object = "ClassificationObj"),
          definition = function(object, ...) {
              return( list("classif" = fitObject(object = object@classif)) )
            })

#' \code{plot(x, ...)}
#'   calls plot method(s) for objects obtained for a classification step.
#'   Title is concatenated with 'Classification' if suppress = FALSE. Method 
#'   called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
setMethod(f = "plot",
          signature = c(x = "ClassificationObj"),
          definition = function(x, suppress = FALSE, ...) {

              argList <- list(...)

              if (!suppress) {
                argList <- .titleIt(argList = argList, nm = "Classification")
              }

              argList[[ "x" ]] <- x@classif
              argList[[ "suppress" ]] <- suppress

              do.call(what = plot, args = argList)

            })

#' \code{predict(object, ...)}
#'   calls predict method for the object(s) obtained for a classification step.
#'   Method called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
setMethod(f = "predict",
          signature = c(object = "ClassificationObj"),
          definition = function(object, ...) {
              return( predict(object = object@classif, ...) ) 
            })

#' Make Predictions for All Tx
#'
#' \code{.predictAll(object, newdata)}
#'   predicts optimal treatment
#'
#' @rdname ClassificationObj-methods
setMethod(f = ".predictAll",
          signature = c(object = "ClassificationObj",
                        newdata = "data.frame"),
          definition = function(object, newdata, ...) {
              return( .predictAll(object = object@classif, newdata = newdata, ...) ) 
            })

#' print() extends the print method to include "Classification" header. 
#'   Method called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
setMethod(f = "print",
          signature = c(x = "ClassificationObj"),
          definition = function(x, ...) {
              cat("\nClassification\n")
              print(x = x@classif, ...)
            })

#' show() extends the show method to include "Classification" header.
#'   Method called determined by class of @classif.
#'
#' @rdname ClassificationObj-methods
setMethod(f = "show",
          signature = c(object = "ClassificationObj"),
          definition = function(object) {
              cat("\nClassification\n")
              show(object = object@classif)
            })

#' summary() calls summary method(s) for regression object(s) obtained in
#'   a classification step. Returns result as a single element list with
#'   element name 'classification'. Method called determined by class of 
#'   @classif
#'
#' @rdname ClassificationObj-methods
setMethod(f = "summary",
          signature = c(object = "ClassificationObj"),
          definition = function(object, ...) {
              return( list("classif" = summary(object = object@classif, ...)) )
            })
