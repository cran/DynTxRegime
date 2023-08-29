#' Class \code{.owl}
#'
#' Class \code{.owl} stores parameters required for OWL optimization step
#'
#' @name internal-owl-class
#'
#' @keywords internal
#'
#' @slot x Matrix of covariates for kernel
#' @slot txSignR Vector of tx multiplied by the sign of the response
#' @slot txVec Vector of tx coded as -1/1
#' @slot absRinvPi Vector of the absolute value of the response weighted by
#'   the propensity for the tx received
#' @slot response Vector of the response
#' @slot invPi Vector of the inverse of the propensity for the tx received
#' @slot surrogate The Surrogate for the loss-function
#' @slot pars Vector of regime parameters
#' @slot kernel The Kernel defining the decision function
setClass(Class = '.owl',
         slots = c(txSignR = "vector",
                   absRinvPi = "vector"),
         contains = c("LearningObject"))

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{.owl}
#'
#' @name internal-owl-methods
#'
#' @keywords internal
NULL


#' @rdname internal-owl-methods
setMethod(f = ".subsetObject",
          signature = c(methodObject = ".owl"),
          definition = function(methodObject, subset) {

              newLO <- .subsetObject(methodObject = as(object = methodObject,
                                                       Class = "LearningObject"),
                                     subset = subset)

              return( new(Class = ".owl",
                          "txSignR"   = methodObject@txSignR[subset],
                          "absRinvPi" = methodObject@absRinvPi[subset],
                          newLO) )
            })


#' @rdname internal-owl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".owl",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                ..., 
                                lambda) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- methodObject@x %*% vee

              lb <- drop(x = vee %*% temp)*0.5*lambda
              u <- methodObject@txSignR*{drop(x = temp) + bee}

              res <- mean(x = methodObject@absRinvPi*
                             .phiFunc(surrogate = methodObject@surrogate, 
                                      u = u, ...)) + lb

              return( res )
            })

#' @rdname internal-owl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObj" = ".owl",
                        "kernel" = "Kernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                ...,
                                lambda) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- drop(methodObject@x %*% vee)

              dlb <- c(0,lambda * temp)
              u <- methodObject@txSignR*{temp + bee}
              du <- methodObject@txSignR * cbind(1,methodObject@x)

              res <- colMeans(x = methodObject@absRinvPi*
                                  .dPhiFunc(surrogate = methodObject@surrogate,
                                            u = u,
                                            du = du, ...)) + dlb

              return( drop(x = res) )
            })

#' @rdname internal-owl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".owl",
                        "kernel" = "LinearKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                ...,
                                lambda) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- methodObject@x %*% vee

              lb <- drop(x = vee %*% vee)*0.5*lambda
              u <- methodObject@txSignR*{drop(x = temp) + bee}
              res <- mean(x = methodObject@absRinvPi*
                              .phiFunc(surrogate = methodObject@surrogate,
                                       u = u, ...)) + lb

              return( res )

            })

#' @rdname internal-owl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".owl",
                        "kernel" = "LinearKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                ...,
                                lambda) {

              bee <- par[ 1L]
              vee <- par[-1L]

              temp <- drop(x = methodObject@x %*% vee)

              dlb <- c(0,lambda*vee)
              u <- methodObject@txSignR*{temp + bee}
              du <- methodObject@txSignR*cbind(1,methodObject@x)

              res <- colMeans(x = methodObject@absRinvPi*
                                  .dPhiFunc(surrogate = methodObject@surrogate,
                                            u = u,
                                            du = du, ...)) + dlb

              return( drop(x = res) )

            })

#' code{.objFn} is not allowed for OWL with multiple radial kernels
#'
#' @rdname internal-owl-methods
setMethod(f = ".objFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".owl",
                        "kernel" = "MultiRadialKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                ...,
                                lambda) { stop("not allowed") })

#' code{.dobjFn} is not allowed for OWL with multiple radial kernels
#'
#' @rdname internal-owl-methods
setMethod(f = ".dobjFn",
          signature = c("par" = "numeric",
                        "methodObject" = ".owl",
                        "kernel" = "MultiRadialKernel"),
          definition = function(par,
                                methodObject, 
                                kernel,
                                ...,
                                lambda) { stop("not allowed") })

#' @rdname internal-owl-methods
setMethod(f = ".valueFunc",
          signature = c("methodObject" = ".owl"),
          definition = function(methodObject, ..., optTx) {

              val <- methodObject@response * methodObject@invPi *
                     {{sign(x = optTx) * sign(x = methodObject@txVec)} > 0.0}

              badV <- is.infinite(x = val) | is.na(x = val) | is.nan(x = val)

              val[ badV ] <- 0.0

              value <- mean(x = val)

              return( value )
            })

#' Create method object for Outcome Weighted Learning
#'
#' @param kernel Kernel object
#' @param txVec Vector of tx coded as -1/1
#' @param response Vector of responses
#' @param prWgt Vector of propensity for tx received
#' @param surrogate Surrogate object indicating surrogate loss-function
#' @param guess Vector of estimated regime parameters
#' @param mu Matrix of outcome regression (zero/ignored)
#'
#' @return An .owl object
#'
#' @name createowl
#'
#' @keywords internal
.createowl <- function(...,
                       kernel, 
                       txVec,  
                       response,  
                       prWgt,  
                       surrogate,  
                       guess = NULL, 
                       mu) {

  newLO <- .createLearningObject(kernel = kernel,
                                 surrogate = surrogate,
                                 guess = guess,
                                 mu = mu,
                                 txVec = txVec,
                                 prWgt = prWgt,
                                 response = response) 
  
  tsr <- txVec
  tsr[response <= 0] <- -tsr[response <= 0]

  return( new(Class = ".owl",
              "txSignR"   = tsr,
              "absRinvPi" = abs(x = response) / prWgt,
              newLO) )

}

#' @importFrom kernlab ipop
setMethod(f = ".optimFunc",
          signature = c("methodObject" = ".owl"),
          definition = function(methodObject, 
                                ...,
                                par,
                                lambda,
                                suppress) {

            if (is(object = methodObject@surrogate, 
                   class2 = "HingeSurrogate")) {

              argList <- list(...)
              ipopNames <- names(x = formals(fun = kernlab::ipop))
              use <- names(x = argList) %in% ipopNames
              argList <- argList[ use ]

              # objective function
              # kernlab::ipop uses the following notation:
              # min( c'x + 1/2 x' H x)
              # s.t. b <= A x <= b + r; l <= x <= u
              n <- nrow(x = methodObject@x)

              # calculate weights
              argList[[ "u" ]] <- lambda * methodObject@absRinvPi

              # c is a negative sign
              argList[[ "c" ]] <- matrix(-1.0, nrow = n, ncol = 1L)

              # equality condition: sum of sign(Y) treatments * parameters = 0.0
              argList[[ "A" ]] <- matrix(methodObject@txSignR, nrow = 1L, ncol = n)

              # H = sign(Y) txVec * kernel matrix * txVec sign(Y)
              h1 <- methodObject@txSignR %o% methodObject@txSignR

              if (is(object = methodObject@kernel, class2 = "LinearKernel")) {
                argList[[ "H" ]] <- h1 * {methodObject@x %*% t(methodObject@x)}
              } else {
                argList[[ "H" ]] <- h1 * methodObject@x
              }

              argList[[ "b" ]] <- 0.0
              argList[[ "l" ]] <- rep(0.0,n)
              argList[[ "r" ]] <- 0.0
              argList[[ "verb" ]] <- as.numeric(!suppress)

              # optimization
              optimResults <- tryCatch(expr = do.call(what = kernlab::ipop,
                                                      args = argList),
                                       error = function(e){
                                                   stop("kernlab::ipop() encountered errors\n", 
                                                        e$message, call. = FALSE)
                                                 })

              # stop if method did not converge
              if (optimResults@how != "converged") {
                stop("kernlab::ipop() did not converge", call. = FALSE)
              }

              # create list return object to match expectations of optim classes
              res <- list()
              if (is(object = methodObject@kernel, class2 = "LinearKernel")) {
                pars <- {optimResults@primal * methodObject@txSignR} %*% 
                        methodObject@x
                res[[ "par" ]] <- c(-optimResults@dual, pars)
              } else {
                res[[ "par" ]] <- c(-optimResults@dual, optimResults@primal)
              }

              if (optimResults@how == "converged") {
                res[[ "convergence" ]] <- 0
              } else {
                res[[ "convergence" ]] <- 2
              }

              res[[ "primal" ]] <- optimResults@primal
              res[[ "dual" ]] <- optimResults@dual
              res[[ "how" ]] <- optimResults@how

              return( res )
            } else {

              return( .optim(surrogate = methodObject@surrogate,
                             par = par,
                             lambda = lambda,
                             fn = .objFn,
                             gr = .dobjFn,
                             suppress = suppress, 
                             methodObject = methodObject, 
                             kernel = methodObject@kernel, ...) )
            }

          })
