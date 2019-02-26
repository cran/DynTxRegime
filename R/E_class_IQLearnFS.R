# October 25, 2018

#' Class \code{IQLearnFS}
#'
#' Class \code{IQLearnFS} contains results for a component of the first stage
#'   analysis of the interactive Q-Learning algorithm.
#'
#' @name IQLearnFS-class
#'
#' @include E_class_IQLearnSS.R
#'
#' @keywords internal
setClass(Class = "IQLearnFS", 
         contains = c("QLearn"))

#' Methods Available for Objects of Class \code{IQLearnFS}
#'
#' Employs methods defined for \code{QLearn}
#'
#' @name IQLearnFS-methods
#'
#' @keywords internal
NULL

#' @param y Object of class IQLearnFS
#' @param z Object of class IQLearnFS
#' @param dens one of \{norm, nonpar\}
#' @rdname optTx
setMethod(f = "optTx", 
          signature = c(x = "IQLearnFS",
                        newdata = "data.frame"), 
          definition =  function(x, 
                                 newdata, ...,  
                                 y = NULL,  
                                 z = NULL,  
                                 dens = NULL) {

              if ({is.null(x = y) & is.null(x = z)} || is.null(x = dens)) {
                stop("must provide at least two first stage objects and dens")
              }  

              obj <- .matchObjects(x = x, y = y, z = z)    
              me <- obj$me
              co <- obj$co
              so <- obj$so                   

              lhat <- optTx(x = as(object = me, Class = "QLearn"), 
                            newdata = newdata)$decisionFunc

              mu <- optTx(x = as(object = co, Class = "QLearn"), 
                            newdata = newdata)$decisionFunc

              if (!is.null(x = so)) {
                sig <- optTx(x = as(object = so, Class = "QLearn"), 
                             newdata = newdata)$decisionFunc
                sig <- exp(x = sig / 2.0) * exp(x = so@scale / 2.0)

                if (dens == "norm") {
                  rso <- NULL
                } else {
                  rso <- residuals(object = so)
                }
              } else {
                sig <- matrix(data = sd(x = co), ncol = 2L)

                if (dens == "norm") {
                  rso <- NULL
                } else {
#                  rso <- scale(x = residuals(object = co), center = FALSE)
                  rso <- residuals(object = so)
                }
              }

              qf <- .qfIQLearnFS(lhat = lhat, 
                                 mu = mu, 
                                 sig = sig, 
                                 dens = dens, 
                                 txVec = co@txVec, 
                                 rso = rso)
 
              # optimal tx is that with the largest value function
              optTx <- apply(X = qf, MARGIN = 1L, FUN = which.max)
              optTx <- c(-1L,1L)[optTx]
  
              return( list("optimalTx"    = optTx,
                           "decisionFunc" = qf) )
            })

#' @rdname optTx
setMethod(f = "optTx", 
          signature = c(x = "IQLearnFS",
                        newdata = "missing"), 
          definition =  function(x, 
                                 newdata, ...,  
                                 y = NULL,  
                                 z = NULL,  
                                 dens = NULL) {

              if ({is.null(x = y) & is.null(x = z)} || is.null(x = dens)) {
                stop("must provide at least two first stage objects and dens")
              }  

              obj <- .matchObjects(x = x, y = y, z = z)    
              me <- obj$me
              co <- obj$co
              so <- obj$so                   

              lhat <- optTx(x = as(object = me, Class = "QLearn"))$decisionFunc

              mu <- optTx(x = as(object = co, Class = "QLearn"))$decisionFunc

              if (!is.null(x = so)) {

                sig <- optTx(x = as(object = so, Class = "QLearn"))$decisionFunc
                sig <- exp(x = sig / 2.0) * exp(x = so@scale / 2.0)

                if (dens == "norm") {
                  rso <- NULL
                } else {
                  rso <- residuals(object = so)
                }
              } else {

                sig <- matrix(data = sd(x = co), ncol = 2L)

                if (dens == "norm") {
                  rso <- NULL
                } else {
#                  rso <- scale(x = residuals(object = co), center = FALSE)
                  rso <- residuals(object = so)
                }
              }

              qf <- .qfIQLearnFS(lhat = lhat, 
                                 mu = mu, 
                                 sig = sig, 
                                 dens = dens, 
                                 txVec = co@txVec, 
                                 rso = rso)
 
              # optimal tx is that with the largest value function
              optTx <- apply(X = qf, MARGIN = 1L, FUN = which.max)
              optTx <- c(-1L,1L)[optTx]
  
              return( list("optimalTx"    = optTx,
                           "decisionFunc" = qf) )
            })


.estimator_IQLearnFS <- function(x,  
                                 w = NULL,  
                                 y = NULL,  
                                 z = NULL,  
                                 dens = NULL) { 

  tst <- c(is.null(x = w), is.null(x = y), is.null(x = z))
  if (sum(tst) > 1L || is.null(x = dens)) {
    stop("must provide at least 2 IQLearnFS results, 1 IQLearnSS result, and dens")
  }

  obj <- .matchObjects(x = x, y = y, z = z, w = w)    

  me <- obj$me
  co <- obj$co
  so <- obj$so
  ss <- obj$ss                 

  dens <- tolower(x = dens)

  if (!(dens %in% c("norm", "nonpar"))) {
    stop("dens must be one of {norm, nonpar}")
  }

  q1res <- optTx(x = me, y = co, z = so, dens = dens)

  q2res <- ss@delta * {co@txVec == q1res$optimalTx}

  return( sum(q2res)/sum(abs(x = q2res)>0.0) )

}

#' @rdname estimator
setMethod(f = "estimator",    
          signature = c(x = "IQLearnFS"), 
          definition =  .estimator_IQLearnFS)


#' @rdname estimator
setMethod(f = "estimator",    
          signature = c(x = "IQLearnSS"), 
          definition = .estimator_IQLearnFS )

# @param lhat Qhat for fitted main effects component of final outcome
# @param mu Qhat for fitted contrast component of final outcome
# @param sig Qhat for fitted variance
# @param dens Density model
# @param txVec vector of treatments coded as +/- 1
# @param rso residuals
#
# @returns matrix of Q-functions for first stage
#' @importFrom stats pnorm
.qfIQLearnFS <- function(lhat, mu, sig, dens, txVec, rso) {

  dens <- tolower(x = dens)

  # estimate Q1 using either normal density or empirical estimate
  q1Hat <- matrix(data = 0.0,  
                  nrow = nrow(x = lhat),  
                  ncol = 2L,  
                  dimnames=list(NULL, c(-1,1)))

  if (dens == "norm") {

    q1Hat[,2L] <- mu[,2L] * (1.0 - 2.0 * stats::pnorm(q = -mu[,2L] / sig[,2L])) +
                  sqrt(x = 2.0 / pi) * sig[,2L] * 
                  exp(x = -mu[,2L]^2 / (2.0 * sig[,2L]^2)) 

    q1Hat[,1L] <- mu[,1L] * (1.0 - 2.0 * stats::pnorm(q = -mu[,1L] / sig[,1L])) +
                  sqrt(x = 2.0 / pi) * sig[,1L] * 
                    exp(x = -mu[,1L]^2 / (2.0 * sig[,1L]^2)) 

  } else {
    func <- function(x,y,r,t,a) {
              tma <- as.numeric(x = t == a)
              if (sum(tma) > 0.0) {
                return( sum(abs(x = x + y*r)) )
              } else {
                return( 0.0 )
              }
            }

    q1Hat[,2L] <- mapply(FUN = func, 
                         mu[,2L], sig[,2L], 
                         MoreArgs = list(r = rso, 
                                         t = as.integer(x = round(x = txVec)),  
                                         a = 1L))
    q1Hat[,1L] <- mapply(FUN = func, 
                         mu[,1L], sig[,1L], 
                         MoreArgs = list(r = rso, 
                                         t = as.integer(x = round(x = txVec)),  
                                         a = -1L))
  }
  q1Hat <- lhat + q1Hat

  return( q1Hat )

}

.matchObjects <- function(x, y, z, w = NULL) {

  me <- NULL
  co <- NULL
  so <- NULL
  ss <- NULL

  if (is.null(x = y)) {
  } else if (is(object = y, class2 = "IQLearnFS_ME")) {
    me <- y
  } else if (is(object = y, class2 = "IQLearnFS_C")) {
    co <- y
  } else if (is(object = y, class2 = "IQLearnFS_VHet")) {
    so <- y
  } else if (is(object = y, class2 = "IQLearnSS")) {
    ss <- y
  }

  if (is.null(x = z)) {
  } else if (is(object = z, class2 = "IQLearnFS_ME")) {
    me <- z
  } else if (is(object = z, class2 = "IQLearnFS_C")) {
    co <- z
  } else if (is(object = z, class2 = "IQLearnFS_VHet")) {
    so <- z
  } else if (is(object = z, class2 = "IQLearnSS")) {
    ss <- z
  }

  if (is.null(x = x)) {
  } else if (is(object = x, class2 = "IQLearnFS_ME")) {
    me <- x
  } else if (is(object = x, class2 = "IQLearnFS_C")) {
    co <- x
  } else if (is(object = x, class2 = "IQLearnFS_VHet")) {
    so <- x
  } else if (is(object = x, class2 = "IQLearnSS")) {
    ss <- x
  }

  if (is.null(x = w)) {
  } else if (is(object = w, class2 = "IQLearnFS_ME")) {
    me <- w
  } else if (is(object = w, class2 = "IQLearnFS_C")) {
    co <- w
  } else if (is(object = w, class2 = "IQLearnFS_VHet")) {
    so <- w
  } else if (is(object = w, class2 = "IQLearnSS")) {
    ss <- w
  }
  
  if (is.null(x = me)) {
    stop("no IQLearnFS_ME object provided")
  }

  if (is.null(x = co)) {
    stop("no IQLearnFS_C object provided")
  }

  return( list("me" = me, "co" = co, "so" = so, "ss" = ss) )
}

#' @rdname IQLearnFS-methods
setMethod(f = "print",    
          signature = c(x = "IQLearnFS"), 
          definition = function(x, ...) {
              print(x = as(object = x@analysis, Class = "OutcomeObj"))
            })

#' @rdname IQLearnFS-methods
setMethod(f = "show",    
          signature = c(object = "IQLearnFS"), 
          definition = function(object) {
              show(object = as(object = object@analysis, Class = "OutcomeObj"))
            })

#' @rdname IQLearnFS-methods
setMethod(f = "summary",    
          signature = c(object = "IQLearnFS"), 
          definition = function(object, ...) {
              return( summary(object = as(object = object@analysis, 
                                          Class = "OutcomeObj")) )
            })
