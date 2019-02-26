# October 25, 2018

#' Class \code{TypedFit}
#'
#' Class \code{TypedFit} is a \code{modelObjFit} combined with a character
#'   to identify its purpose.
#'
#' @name TypedFit-class
#'
#' @keywords internal
setClass(Class = "TypedFit",
         slots = c(type = "character"),
         contains = c("modelObjFit"))

##########
## GENERICS
##########

#' Complete a Regression Step
#'
#' This function completes a regression step and stores a character object
#'   used to identify the purpose of the step, such as a propensity or
#'   outcome regression.
#'
#' @name newTypedFit
#'
#' @param modelObj A modeling object
#' @param txObj A TxObj object
#' @param ...  Any optional additional input.
#' 
#' @keywords internal
setGeneric(name = ".newTypedFit", 
           def = function(modelObj, txObj, ...) {
               standardGeneric(f = ".newTypedFit")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{TypedFit}
#'
#' Methods call equivalently named methods defined for \code{modelObjFit}
#'   objects.
#'
#' @name TypedFit-methods
#'
#' @keywords internal
NULL

#' @rdname newTypedFit
#'
#' @importFrom modelObj fit coef
setMethod(f = ".newTypedFit", 
          signature = c(modelObj = "modelObj",
                        txObj = "TxInfoNoSubsets"), 
          definition = function(modelObj, 
                                txObj,
                                response,  
                                data,
                                type,
                                suppress) {

              if (nchar(x = type) == 0L ) stop("must provide type")

              if (!suppress ) cat("Regression analysis for ", type, ":\n", sep="")

              fitObj <- tryCatch(expr = modelObj::fit(object = modelObj, 
                                                      data = data, 
                                                      response = response),
                                 error = function(e) {
                                           cat(e$message, "\n")
                                           stop("unable to obtain fit\n")
                                           return( e )
                                         })

              if (!is.null(x = modelObj::coef(object = fitObj))) {
                if (any(is.na(x = modelObj::coef(object = fitObj)))) {
                  stop("fit results in NA parameter estimates")
                }
              }

              if (!suppress) print(x = fitObj)

              result <- new(Class = "TypedFit",
                            type = type,
                            fitObj)

              return( result )
            })

#' \code{coef(object)} 
#'   retrieves the estimated coefficients. 
#'
#' @rdname TypedFit-methods
#' @importFrom modelObj coef
setMethod(f = "coef",
          signature = c(object = "TypedFit"),
          definition = function(object, ...) {
              res <- list()
              res[[ object@type ]] <- callNextMethod()
              return( res )
            })

#' \code{fitObject(object)}
#'   retrieves the regression objects.  
#'
#' @rdname TypedFit-methods
#' @importFrom modelObj fitObject
setMethod(f = "fitObject",
          signature = c(object = "TypedFit"),
          definition = function(object, ...) {
              res <- list()
              res[[ object@type ]] <- callNextMethod()
              return( res )
            })

#' \code{plot(x, ...)}
#'   calls plot method(s) for a regression object.
#'   Title is concatenated with @type if suppress = FALSE. 
#'
#' @rdname TypedFit-methods
#' @importFrom modelObj fitObject plot
setMethod(f = "plot",
          signature = c(x = "TypedFit"),
          definition = function(x, suppress=FALSE, ...) {

              argList <- list(...)
              if (!suppress) argList <- .titleIt(argList = argList, nm = x@type)

              argList[[ "x" ]] <- modelObj::fitObject(object = as(object = x,
                                                                  Class = "modelObjFit"))

              try(do.call(what = modelObj::plot, args = argList))

            })

#' \code{print(x)}
#'   extends the print method to include @type header. 
#'
#' @rdname TypedFit-methods
#' @importFrom modelObj predict print
setMethod(f = "print",
          signature = c(x = "TypedFit"),
          definition = function(x, ...) {
              cat(x@type, "\n")
              callNextMethod()
            })

#' \code{show(object)}
#'   extends the show method to include @type header. 
#'
#' @rdname TypedFit-methods
#' @importFrom modelObj show
setMethod(f = "show",
          signature = c(object = "TypedFit"),
          definition = function(object) {
              cat(object@type, "\n")
              callNextMethod()
            })

#' \code{summary(object)}
#'   calls summary method(s) for regression object. 
#'   Returns result as a single element list with element name in @type. 
#'
#' @rdname TypedFit-methods
#' @importFrom modelObj summary
setMethod(f = "summary",
          signature = c(object = "TypedFit"),
          definition = function(object, ...) {
              res <- list()
              res[[ object@type ]] <- callNextMethod()
              return( res )
            })
