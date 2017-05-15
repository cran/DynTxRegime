#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                           Class EARLOptim                            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of the optimization step of the                              #
# Efficient Augmentation and Relaxation Learning algorithm             #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# optim       : value object returned by optim method                  #
# weight      : matrix of positive/negative weights                    #
# surrogate   : character description of surrogate for loss function   #
# lambda      : numeric tuning parameter                               #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setClass(Class = "EARLOptim",
         slots = c(   weight = "matrix",
                   surrogate = "character"),
         contains = c("OptimBasic"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                          EARLOptim GENERICS                          #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setGeneric(name = ".newEARLOptim", 
           def = function(x, mu, ...){
                   standardGeneric(".newEARLOptim")
                 } )

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                          EARLOptim METHODS                           #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#----------------------------------------------------------------------#
# Retrieve the results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARLOptim                                #
#   returns                                                            #
# a list object with key information from optimization routine         #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="EARLOptim"),
          definition = function(object, ...) { 
                         res <- optimObj(object = as(object,"OptimBasic"))
                         res[[ "surrogate" ]] <- object@surrogate
                         return(res)  
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class EARLOptim                                     #
# newdata : the model matrix of the decision function                  #
#   returns                                                            #
# A list containing decision function (f(x)) and its sign              #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",
          signature = c(x = "EARLOptim", 
                        newdata = "matrix"),
          definition = function (x, newdata,...){

                         if( ncol(newdata) == {length(x@optim$par)-1L} ) {
                           newdata <- cbind(1,newdata)
                         } else if( ncol(newdata) != length(x@optim$par) ) {
                           stop("number of columns in newdata not appropriate")
                         }

                         fx <- drop(newdata %*% x@optim$par)
                         names(fx) <- NULL

                         return( list("optimalTx"    = sign(fx),
                                      "decisionFunc" = fx) )

                       } )

setMethod(f = ".predictOptimalTx",
          signature = c(x = "EARLOptim", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){

                         return(.predictOptimalTx(x = x, 
                                                  newdata = data.matrix(newdata)))

                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class EARLOptim                                #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "EARLOptim"), 
          definition = function(object, ...){
                         return( object@optim$par )
                       } )

setMethod(f = ".newEARLOptim",    
          signature = c(x = "matrix", 
                        mu = "missing"), 
          definition = function(x, mu, lambda, txVec, prWgt, response, 
                                surrogate, guess,
                                suppress, subset, ...) {

                         x <- x[subset,,drop=FALSE]
                         txVec <- txVec[subset]
                         prWgt <- prWgt[subset]
                         response <- response[subset]

                         Wneg1 <- {txVec < 0.0} * response / prWgt
                         Wpos1 <- {txVec > 0.0} * response / prWgt

                         res <- .EARL(x = x,
                                      lambda = lambda,
                                      wNeg = Wneg1,
                                      wPos = Wpos1,
                                      surrogate = surrogate,
                                      txVec = txVec,
                                      guess = guess,
                                      suppress = suppress)

                         return(res)
                       } )

#----------------------------------------------------------------------#
# x            : model matrix of decision rule                         #
# mu           : Q-functions at tx = -1 and +1                         #
# lambda       : tuning parameter                                      #
# txVec        : Treatment given coded as +/- 1                        #
# pr           : Probability that tx = +1                              #
# response     : Response vector                                       #
# surrogate    : character description of surrogate loss function      #
# suppress     : T/F indicating if messages are to be printed.         #
#----------------------------------------------------------------------#
setMethod(f = ".newEARLOptim",    
          signature = c(x = "matrix", 
                        mu = "matrix"), 
          definition = function(x, mu, lambda, txVec, prWgt, response, 
                                surrogate, guess, 
                                suppress, subset, ...) {

                         x <- x[subset,,drop=FALSE]
                         mu <- mu[subset,,drop=FALSE]
                         txVec <- txVec[subset]
                         prWgt <- prWgt[subset]
                         response <- response[subset]

                         Wneg1 <- {txVec < 0.0} * 
                                  {response - mu[,1L]} / prWgt +
                                  mu[,1L]
                         Wpos1 <- {txVec > 0.0} * 
                                  {response - mu[,2L]} / prWgt +
                                  mu[,2L]

                         res <- .EARL(x = x,
                                      lambda = lambda,
                                      wNeg = Wneg1,
                                      wPos = Wpos1,
                                      surrogate = surrogate,
                                      txVec = txVec,
                                      guess = guess,
                                      suppress = suppress)


                         return(res)
                       } )

#----------------------------------------------------------------------#
# x            : model matrix of decision rule                         #
# lambda       : tuning parameter                                      #
# wNeg         : weight vector for negative treatment                  #
# wPos         : weight vector for positive treatment                  #
# surrogate    : character description of surrogate loss function      #
# guess        : vector of initial parameter values for optim          #
# suppress     : T/F indicating if messages are to be printed.         #
#----------------------------------------------------------------------#
.EARL <- function(x, 
                  lambda, 
                  wNeg, 
                  wPos, 
                  surrogate, 
                  txVec,
                  guess, 
                  suppress) {

  #------------------------------------------------------------------#
  # Define objective function                                        #
  #------------------------------------------------------------------#
  objFn <- function(b, 
                    surrogate,  
                    wp,  
                    wn, 
                    x,  
                    lambda) {

             bee <- b[ 1L]
             vee <- b[-1L]

             xbeta <- x %*% vee + bee

             lb <- lambda * {vee %*% vee}

             if( surrogate == 'logit' ) {
               res <- sum(wp * log(1.0 + exp(-xbeta))) + 
                      sum(wn * log(1.0 + exp( xbeta))) + lb
             } else if( surrogate == 'exp' ) {
               res <- sum(wp * exp(-xbeta)) + sum(wn * exp( xbeta)) + lb
             } else if( surrogate == 'sqhinge' ) {
               t1 <- {1.0 - xbeta}
               t1t <- t1 > 0.0
               t2 <- 1.0 + xbeta
               t2t <- t2 > 0.0
               res <- sum(wp*{t1^2}*t1t) + sum(wn*{t2^2}*t2t) + lb
             } else if( surrogate == 'hinge' ) {
               t1 <- {1.0 - xbeta}
               t1t <- t1 > 0.0
               t2 <- 1.0 + xbeta
               t2t <- t2 > 0.0
               res <- sum(wp*t1*t1t) + sum(wn*t2*t2t) + lb
             }  

             return(res)

           }

  dobjFn <- function(b,  
                     surrogate,  
                     wp,  
                     wn,  
                     x,  
                     lambda) {

              bee <- b[ 1L]
              vee <- b[-1L]

              xbeta <- x %*% vee + bee

              lb <- c(0,lambda * 2.0 * vee)

              expxbp <- exp(xbeta)
              expxbm <- exp(-xbeta)

              x1 <- cbind(1,x)

              if( surrogate == 'logit' ) {
                res <- drop({-wp * {expxbm / {1.0 + expxbm}} + 
                         wn * {expxbp / {1.0 + expxbp}}}) %*% x1 + lb
              } else if( surrogate == 'exp' ) {
                res <- drop({-wp * expxbm + wn * expxbp}) %*% x1 + lb
              } else if( surrogate == 'sqhinge' ) {
                t1 <- -x1 * 2.0 * {1.0 - drop(xbeta)}
                t1t <- {1.0 - drop(xbeta)} > 0.0
                t2 <-  x1 * 2.0 * {1.0 + drop(xbeta)}
                t2t <- {1.0 + drop(xbeta)} > 0.0
                res <- colSums(wp*t1*t1t) + colSums(wn*t2*t2t) + lb
              } 

              return( drop(res) )

           }

  wp <-  wPos * {wPos > 0.0} - wNeg * {wNeg < 0.0}
  wn <- -wPos * {wPos < 0.0} + wNeg * {wNeg > 0.0}

  if( is(guess, "NULL") ) {
    pars <- rnorm(ncol(x)+1L)
  } else {
    pars <- guess
  }

  #------------------------------------------------------------------#
  # Derivative methods cannot be used for hinge loss                 #
  #------------------------------------------------------------------#
  if( surrogate %in% c('logit', 'exp', 'sqhinge') ) {

    #--------------------------------------------------------------#
    # Minimize objective function using optim                      #
    #--------------------------------------------------------------#
    conv <- 1L
    cnt <- 0L
    while( conv != 0L && cnt < 100L) {

      test <- stats::optim(par = pars, 
                           fn = objFn, 
#                           gr = dobjFn,
                           method = "BFGS",
                           surrogate = surrogate, 
                           wp = wp, 
                           wn = wn, 
                           x = x,
                           lambda = lambda,
                           control = list("trace" = !suppress))

      conv <- test$convergence
      pars <- test$par
      cnt <- cnt + 1L
    }

    if( test$convergence > 0.5 ) {
      cat("stats::optim did not converge.\n")
      return( NULL )
    }

    if( !suppress ) {
      cat("Results returned by stats::optim\n")
      print(test)
    }

  } else {
    conv <- 1L
    cnt <- 0L
    while(conv != 0L && cnt < 100L) {
      test <- dfoptim::hjk(par = pars, 
                           fn = objFn,
                           surrogate = surrogate, 
                           wp = wp, 
                           wn = wn, 
                           x = x,
                           lambda = lambda,
                           control = list("info"=!suppress))
      conv <- test$convergence
      pars <- test$par
      cnt <- cnt + 1L
    }

    if( test$convergence > 0.5 ) {
      cat("dfoptim::hjk did not converge.\n")
      return( NULL )
    }

    if( !suppress ) {
      cat("Results returned by dfoptim::hjk\n")
      print(test)
    }

  }

  res <- new("EARLOptim",
             "optim"     = test,
             "weight"    = cbind(wPos,wNeg),
             "surrogate" = surrogate,
             "lambda"    = lambda)

  return(res)

}


