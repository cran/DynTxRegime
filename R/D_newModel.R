# October 25, 2018

# Create main effects component of model formula
# @importFrom stats terms
.main <- function(model) {

  tempMain <- stats::terms(x = model)
  mainPart <- paste(attr(x = tempMain, which = "term.labels"), collapse="+")

  if (attr(x = tempMain, which = "intercept") < 0.5) {
    mainPart <- paste("0+", mainPart, sep="")
  }

  return( mainPart )

}

# Create contrast component of model formula
# @importFrom stats terms
.contrast <- function(model, txName) {

  tempCont <- stats::terms(x = model)
  contPart <- paste(attr(x = tempCont, which = "term.labels"), collapse="+")

  if (attr(x = tempCont, which = "intercept") > 0.5 & nchar(x = contPart) > 0L) {
    contPart <- paste0(txName, " + ", txName, ":(", contPart, ")")

  } else if (attr(x = tempCont, which = "intercept") > 0.5) {
    contPart <- txName

  } else {
    contPart <- paste0(txName, ":(", contPart, ")")
  }

  return( contPart )

}

#' Combine model object models
#'
#' Combines moMain and moCont into a single modeling object.
#'
#' @name newModel
#'
#' @keywords internal
setGeneric(name = ".newModel",
           def = function(moMain, moCont, ...) {
                   standardGeneric(".newModel")
                 })

#' @rdname newModel
#' @importFrom stats as.formula
#' @importFrom modelObj model
setMethod(f = ".newModel",
          signature = c(moMain = "modelObj",
                        moCont = "modelObj"),
          definition = function(moMain, moCont, txName, suppress) {

              mainPart <- .main(model = model(object = moMain))
              contPart <- .contrast(model = model(object = moCont),
                                    txName = txName)

              newForm <- paste("~", mainPart, "+", contPart)

              if (!suppress) {
                cat("Combined outcome regression model:", newForm, ".\n")
              }

              moMain@model <- stats::as.formula(object = newForm)

              return( moMain )

            })

#' @rdname newModel
setMethod(f = ".newModel",
          signature = c(moMain = "modelObj",
                        moCont = "NULL"),
          definition = function(moMain, moCont, txName, suppress) {

              if (!suppress) {
                cat("moMain only outcome regression model.", 
                    paste(as.character(x = moMain@model), collapse=""), "\n")
              }

              return( moMain )

            })


#' @rdname newModel
#' @importFrom modelObj model
setMethod(f = ".newModel",
          signature = c(moMain = "NULL",
                        moCont = "modelObj"),
          definition = function(moMain, moCont, txName, suppress) {


              contPart <- .contrast(model = modelObj::model(object = moCont),
                                    txName = txName)

              newForm <- paste("~ -1 +", contPart)

              moCont@model <- as.formula(object = newForm)

              if (!suppress) {
                cat("moCont only outcome regression model.", newForm, "\n")
              }

              return( moCont )

            })
