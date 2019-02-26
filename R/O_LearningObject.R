# December 3, 2018

#' Class \code{LearningObject}
#'
#' Class \code{LearningObject} contains stores parameters required for 
#'  weighted learning optimization step
#'
#' @name LearningObject-class
#' @slot mu Matrix of outcome regression
#' @slot txVec Vector of treatment coded as -1/1
#' @slot invPi Vector of inverse propensity for treatment received
#' @slot response Vector of the response
#'
#' @keywords internal
setClass(Class = 'LearningObject',
         slots = c(mu = "matrix",
                   txVec = "vector",
                   invPi = "vector",
                   response = "vector"),
         contains = c("MethodObject"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{LearningObject}
#'
#' @name LearningObject-methods
#'
#' @keywords internal
NULL


#' @rdname LearningObject-methods
setMethod(f = ".subsetObject",
          signature = c(methodObject = "LearningObject"),
          definition = function(methodObject, subset) {

              newMO <- .subsetObject(methodObject = as(object = methodObject, 
                                                       Class = "MethodObject"),
                                     subset = subset)

              return( new("LearningObject",
                          "mu"       = methodObject@mu[subset,,drop=FALSE],
                          "txVec"    = methodObject@txVec[subset],
                          "invPi"    = methodObject@invPi[subset],
                          "response" = methodObject@response[subset],
                          newMO) )
            })


#' @rdname LearningObject-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = "LearningObject",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' @rdname LearningObject-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = "LearningObject",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' @rdname LearningObject-methods
setMethod(f = ".valueFunc",
          signature = c("methodObject" = "LearningObject"),
          definition = function(methodObject, optTx, ...) { stop("not allowed") })

#' Create LearningObject
#'
#' @param kernel Kernel object
#' @param surrogate Surrogate object indicating surrogate loss-function
#' @param mu Matrix of predicted outcome on binary tx coding
#' @param txVec Tx vector coded as -1/1
#' @param prWgt propensity wgt for tx received
#' @param response vector of response
#' @param guess Vector of estimated regime parameters
#'
#' @return A LearningObject object
#'
#' @rdname LearningObject-methods
#'
#' @keywords internal
.createLearningObject <- function(kernel, 
                                  surrogate,  
                                  mu,
                                  txVec,
                                  response,
                                  prWgt,
                                  guess = NULL, ...) {

  newMO <- .createMethodObject(kernel = kernel,
                               surrogate = surrogate,
                               guess = guess) 

  return( new("LearningObject",
              "mu"       = mu,
              "txVec"    = txVec,
              "invPi"    = 1.0/prWgt,
              "response" = response,
              newMO) )

}
