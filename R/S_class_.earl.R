# October 26, 2018

#' Class \code{.earl}
#'
#' Class \code{.earl} stores parameters required for EARL optimization step.
#'
#' @name internal-earl-class
#'
#' @keywords internal
#'
#' @slot x Matrix of covariates for kernel
#' @slot wp Vector of positive weights
#' @slot wn Vector of negative weights
#' @slot mu Matrix of outcome regression
#' @slot txVec Vector of treatment coded as -1/1
#' @slot invPi Vector of inverse propensity for treatment received
#' @slot response Vector of the response
#' @slot surrogate The Surrogate for the loss-function
#' @slot par Vector of regime parameters
#' @slot kernel The Kernel defining the decision function
setClass(Class = '.earl',
         slots = c(wp = "vector",
                   wn = "vector"),
         contains = c("LearningObject"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{.earl}
#'
#' @name internal-earl-methods
#'
#' @keywords internal
NULL

#' @rdname internal-earl-methods
setMethod(f = ".subsetObject",
          signature = c(methodObject = ".earl"),
          definition = function(methodObject, subset) {

              newLO <- .subsetObject(methodObject = as(object = methodObject,
                                                       Class = "LearningObject"),
                                     subset = subset)

              return( new(".earl",
                          "wp" = methodObject@wp[subset],
                          "wn" = methodObject@wn[subset],
                          newLO) )
            })

#' @rdname internal-earl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".earl",
                        "kernel" = "LinearKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- methodObject@x %*% vee

              lb <- drop(x = vee %*% vee)*0.5*lambda
              u <- drop(x = temp) + bee

              res <- mean(x = methodObject@wp*
                              .phiFunc(surrogate = methodObject@surrogate, 
                                       u = u)) +
                     mean(x = methodObject@wn*
                              .phiFunc(surrogate = methodObject@surrogate, 
                                       u = -u)) + lb

              if (is.nan(x = res)) res <- 1e10

              return( res )

            })

#' @rdname internal-earl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".earl",
                        "kernel" = "LinearKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              u <- drop(x = methodObject@x %*% vee) + bee
              du <- cbind(1.0, methodObject@x)

              dlb <- c(0,lambda * vee)

              res <- colMeans(x = methodObject@wp * 
                                  .dPhiFunc(surrogate = methodObject@surrogate, 
                                            u = u,
                                            du = du)) +
                     colMeans(x = methodObject@wn * 
                                  .dPhiFunc(surrogate = methodObject@surrogate,  
                                            u = -u, 
                                            du = -du)) + dlb

              if (any(is.nan(x = res))) res[] <- 1e10

              return( drop(x = res) )

            })

#' @rdname internal-earl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".earl",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- methodObject@x %*% vee

              lb <- drop(x = vee %*% temp)*0.5*lambda
              u <- drop(x = temp) + bee

              res <- mean(x = methodObject@wp *
                              .phiFunc(surrogate = methodObject@surrogate, 
                                       u = u)) +
                     mean(x = methodObject@wn *
                              .phiFunc(surrogate = methodObject@surrogate, 
                                       u = -u)) + lb

              if (is.nan(x = res)) res <- 1e10

              return( res )

            })

#' @rdname internal-earl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".earl",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- drop(x = methodObject@x %*% vee)

              u <- temp + bee
              du <- cbind(1.0, methodObject@x)
              dlb <- c(0.0,lambda * temp)

              res <- colMeans(x = methodObject@wp * 
                                  .dPhiFunc(surrogate = methodObject@surrogate, 
                                            u = u, 
                                            du = du)) +
                     colMeans(x = methodObject@wn * 
                                  .dPhiFunc(surrogate = methodObject@surrogate,  
                                            u = -u, 
                                            du = -du)) + dlb

              if (any(is.nan(x = res))) res[] <- 1e10
              return( drop(res) )

            })

#' \code{.objFn} not allowed for EARL with multiple radial kernels
#'
#' @rdname internal-earl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".earl",
                        "kernel" = "MultiRadialKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' \code{.dobjFn} not allowed for EARL with multiple radial kernels
#'
#' @rdname internal-earl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".earl",
                        "kernel" = "MultiRadialKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' @rdname internal-earl-methods
setMethod(f = ".valueFunc",
          signature = c("methodObject" = ".earl"),
          definition = function(methodObject, optTx, ...) {

              posOpt <- optTx > 0.5
              posTx <- methodObject@txVec > 0.5

              ind <- {!posTx & !posOpt} | {posTx & posOpt}

              mec <- methodObject@mu[,1L]
              mec[posOpt] <- methodObject@mu[posOpt, 2L]

              val <- {methodObject@response - mec} * ind * methodObject@invPi + mec
              badVal <- is.infinite(x = val) | is.na(x = val) | is.nan(x = val)

              val[badVal] <- 0.0

              value <- mean(x = val)

              return( value )
            })

#' Create method object for EARL
#'
#' @param kernel Kernel object
#' @param txVec Vector of tx coded as -1/1
#' @param response Vector of responses
#' @param prWgt Vector of propensity for tx received
#' @param surrogate Surrogate object indicating surrogate loss-function
#' @param guess Vector of estimated regime parameters
#' @param mu Matrix of outcome regression (zero/ignored)
#'
#' @return An .earl object
#'
#' @name createearl
#'
#' @keywords internal
.createearl <- function(kernel, 
                        txVec,  
                        response,  
                        prWgt,  
                        surrogate,  
                        guess = NULL,  
                        mu, ...) {

  newLO <- .createLearningObject(kernel = kernel,
                                 surrogate = surrogate,
                                 guess = guess,
                                 mu = mu,
                                 txVec = txVec,
                                 prWgt = prWgt,
                                 response = response) 

  wNeg <- {txVec < 0.0} * {response - mu[,1L]} / prWgt + mu[,1L]
  wPos <- {txVec > 0.0} * {response - mu[,2L]} / prWgt + mu[,2L]

  wp <-  wPos * {wPos > 0.0} - wNeg * {wNeg < 0.0}
  wn <- -wPos * {wPos < 0.0} + wNeg * {wNeg > 0.0}

  return( new(".earl",
              "wp" = wp,
              "wn" = wn,
              newLO) )

}
