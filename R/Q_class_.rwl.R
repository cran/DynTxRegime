# October 26, 2018

#' Class \code{.rwl}
#'
#' Class \code{.rwl} stores parameters required for an RWL optimization step
#'
#' @name internal-rwl-class
#'
#' @keywords internal
#'
#' @slot x Matrix of covariates for kernel
#' @slot txVec Vector of treatment coded as -1/1
#' @slot absRinvPi Vector of the absolute value of the residual weighted by
#'   the propensity for the treatment received
#' @slot residual Vector of the residuals
#' @slot response Vector of the response
#' @slot beta Vector of beta parameters
#' @slot surrogate The Surrogate for the loss-function
#' @slot pars Vector of regime parameters
#' @slot kernel The Kernel defining the decision function
setClass(Class = ".rwl",
         slots = c(absRinvPi = "vector",
                   residual = "vector",
                   beta = "vector"),
         contains = c("LearningObject"))


##########
## METHODS
##########
#' Methods Available for Objects of Class \code{.rwl}
#'
#' @name internal-rwl-methods
#'
#' @keywords internal
NULL

#' @rdname internal-rwl-methods
setMethod(f = ".subsetObject",
          signature = c(methodObject = ".rwl"),
          definition = function(methodObject, subset) {

              newLO <- .subsetObject(methodObject = as(object = methodObject,
                                                       Class = "LearningObject"),
                                     subset = subset)

              return( new(".rwl",
                          "absRinvPi" = methodObject@absRinvPi[subset],
                          "residual"  = methodObject@residual[subset],
                          "beta"      = methodObject@beta[subset],
                          newLO) )
            })

#' Redefines general newOptimObj method for RWL to include beta parameters
#' Returns NULL if optimization is not successful. Returns an object of
#' class OptimObj if successful.
#'
#' @rdname newOptimObj
setMethod(f = ".newOptimObj",
          signature = c(methodObject = ".rwl"),
          definition = function(methodObject, 
          	                lambda,
          	                suppress, ...) {

              if (is(object = methodObject@kernel, class2 = "LinearKernel")) {
                # if linear kernel, use covariate matrix
                kern <- methodObject@kernel@X
              } else {
                # if non-linear kernel, create kernel matrix using internal
                # covariate matrix
                kern <- .kernel(object = methodObject@kernel)
              }

              n <- nrow(x = kern)

              maxIter <- 1000L
              iter <- 0L
              betaOld <-  2.0 * methodObject@absRinvPi / n * 
                          {methodObject@residual < 0.0}
              betaOld <- unname(obj = betaOld)

              while (iter < maxIter) {

                if (suppress == 2L) cat("beta iteration", iter+1L, "\n")

                optimResult <- .newOptim(kernel = methodObject@kernel,
                                         lambda = lambda,
                                         methodObject = methodObject,
                                         suppress = suppress, ...)

                if (is.null(x = optimResult)) {
                  iter <- iter + 1L
                  next
                }

                # calculate new beta values
                betaN <- .calculateBeta(par = regimeCoef(object = optimResult), 
                                        kern = kern,  
                                        methodObject = methodObject)

                if (suppress != 0L & {{iter %% 10L} == 0L}) {
                  cat("current max beta diff: ", max(abs(x = betaN-betaOld)),"\n")
                }

                # if new beta values are close to prior iteration break loop
                if (all(abs(x = {betaN - betaOld}) < 1.e-6)) {
                  betaOld <- betaN
                  break
                }

                tst <- abs(x = {betaN - betaOld})/{{betaN+betaOld}/2.0}*1e2
                tst[is.nan(x=tst)] <- 0.0

                if (all(tst < 0.1)) {
                  betaOld <- betaN
                  break
                }

                # reset beta in methodObj and optimize again
                betaOld <- unname(obj = betaN)
                methodObject@beta <- betaOld

                iter <- iter + 1L
              }

              if ({iter > maxIter && is.null(x = optimResult)}) {
                cat("optimization did not converge.\n")
                return( NULL )
              }

              if (suppress == 2L) {
                cat("\nBetas\n")
                print(x = betaOld)
              }

              if (suppress == 1L) {
                print(x = optimResult)
              }

              return( new("OptimObj", "optim" = optimResult) )
            })

.calculateBeta <- function(par, methodObject, kern) {

  bee <- par[ 1L]
  vee <- par[-1L]

  u <- methodObject@txVec * {drop(x = kern %*% vee) + bee}

  beta <- -.dPhiFunc(surrogate = methodObject@surrogate,
                     u = u,
                     du = methodObject@absRinvPi / nrow(x = kern),
                     res = -methodObject@residual) 

  return( beta )
}

#' @rdname internal-rwl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".rwl",
                        "kernel" = "LinearKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- methodObject@x %*% vee

              lb <- drop(x = vee %*% vee)*0.5*lambda
              u <- methodObject@txVec*{drop(x = temp) + bee}

              res <- mean(x = methodObject@absRinvPi *
                              .phiFunc(surrogate = methodObject@surrogate,
                                       u = u,
                                       res = methodObject@residual, ...)) +
                     lb + sum(methodObject@beta * u)

              if (is.nan(x = res)) res <- 1e10

              return( res )

            })

#' @rdname internal-rwl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".rwl",
                        "kernel" = "LinearKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- drop(methodObject@x %*% vee)

              u <- methodObject@txVec*{temp + bee}
              du <- cbind(methodObject@txVec, methodObject@x*methodObject@txVec)

              dlb <- c(0,lambda * vee)

              res <- colMeans(x = methodObject@absRinvPi *
                                  .dPhiFunc(surrogate = methodObject@surrogate,
                                            u = u,
                                            du = du,
                                            res = methodObject@residual, ...)) + 
                     dlb + colSums(x = methodObject@beta*du)

              if (any(is.nan(x = res))) res[] <- 1e10

              return( drop(x = res) )

            })


#' @rdname internal-rwl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".rwl",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- methodObject@x %*% vee

              lb <- drop(x = vee %*% temp)*0.5*lambda
              u <- methodObject@txVec*{x = drop(temp) + bee}

              res <- mean(x = methodObject@absRinvPi *
                              .phiFunc(surrogate = methodObject@surrogate,
                                       u = u,
                                       res = methodObject@residual, ...)) + 
                     lb + sum(methodObject@beta * u)

              if (is.nan(x = res)) res <- 1e10

              return( res )

            })

#' @rdname internal-rwl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".rwl",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- drop(x = methodObject@x %*% vee)

              u <- methodObject@txVec*{temp + bee}
              du <- cbind(methodObject@txVec, methodObject@x*methodObject@txVec)
              dlb <- c(0.0,lambda * temp)

              res <- colMeans(x = methodObject@absRinvPi *
                                  .dPhiFunc(surrogate = methodObject@surrogate,
                                            u = u,
                                            du = du,
                                            res = methodObject@residual, ...)) + 
                     dlb + colSums(x = methodObject@beta*du)

              if (any(is.nan(x = res))) res[] <- 1e10

              return( drop(x = res) )

            })

#' \code{.objFn}
#'   not allowed for RWL With multiple radial kernels
#'
#' @rdname internal-rwl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".rwl",
                        "kernel" = "MultiRadialKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' \code{.dobjFn}
#'   not allowed for RWL With multiple radial kernels
#'
#' @rdname internal-rwl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".rwl",
                        "kernel" = "MultiRadialKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                lambda, ...) { stop("not allowed") })

#' @rdname internal-rwl-methods
setMethod(f = ".valueFunc",
          signature = c("methodObject" = ".rwl"),
          definition = function(methodObject, optTx, ...) {

              val <- methodObject@response * methodObject@invPi *
                     {{sign(x = optTx) * sign(x = methodObject@txVec)} > 0.0}

              badV <- is.infinite(x = val) | is.na(x = val) | is.nan(x = val)

              val[ badV ] <- 0.0

              value <- mean(x = val)

              return( value )
            })

#' Create method object for Residual Weighted Learning
#'
#' @param kernel Kernel object
#' @param txVec Vector of tx coded as -1/1
#' @param response Vector of responses
#' @param prWgt Vector of propensity for tx received
#' @param surrogate Surrogate object indicating surrogate loss-function
#' @param guess Vector of estimated regime parameters
#' @param mu Matrix of outcome regression
#'
#' @return An \code{.rwl} object
#'
#' @name createrwl
#'
#' @keywords internal
.createrwl <- function(kernel, 
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

  residual <- response - mu[,2L]


  return( new(".rwl",
              "absRinvPi" = abs(x = residual)/prWgt,
              "residual"  = residual,
              "beta"      = rep(x = 0.0, times = nrow(x = mu)),
              newLO) )

}

#' Create method object for Residual Weighted Learning
#'
#' @param kernel Kernel object
#' @param txVec Vector of tx coded as -1/1
#' @param response Vector of responses
#' @param prWgt Vector of propensity for tx received
#' @param surrogate Surrogate object indicating surrogate loss-function
#' @param guess Vector of estimated regime parameters
#' @param mu Matrix of outcome regression
#'
#' @return An \code{.rwl} object
#'
#' @name createrwl
#'
#' @keywords internal
.createrwlcount <- function(kernel, 
                            txVec,  
                            response,  
                            prWgt,  
                            surrogate,  
                            guess = NULL, 
                            mu, ...) {

  newLO <- .createrwl(kernel = kernel,
                      surrogate = surrogate,
                      guess = guess,
                      mu = mu,
                      txVec = txVec,
                      prWgt = prWgt,
                      response = response) 

  newLO@residual <- -newLO@residual

  return( newLO )

}
