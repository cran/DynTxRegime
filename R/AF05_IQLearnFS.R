setClass(Class = "IQLearnFS", 
         contains = c("VIRTUAL"))


#----------------------------------------------------------------------#
# Calculate first-stage IQ-Learning Q-Function                         #
#----------------------------------------------------------------------#
#  params                                                              #
# lhat  : Qhat for fitted main effects component of final outcome      #
# mu    : Qhat for fitted contrast component of final outcome          #
# sig   : Qhat for fitted variance                                     #
# dens  : Density model                                                #
# txVec : vector of treatments coded as +/- 1                          #
# rso   : residuals                                                    #
#  returns                                                             #
# matrix of Q-functions for first stage                                #
#----------------------------------------------------------------------#
.qfIQLearnFS <- function(lhat, mu, sig, dens, txVec, rso){

  dens <- tolower(dens)

  #-------------------------------------------------------------------#
  # Estimate Q1 using either normal density or empirical estimate     #
  #-------------------------------------------------------------------#
  q1Hat <- matrix(data = 0.0,  
                  nrow = nrow(lhat),  
                  ncol = 2L,  
                  dimnames=list(NULL, c(-1,1)))

  if( dens == "norm" ) {

    q1Hat[,2L] <- mu[,2L] * (1.0 - 2.0 * pnorm(-mu[,2L] / sig[,2L])) +
                  sqrt(2.0 / pi) * sig[,2L] * 
                  exp(-mu[,2L]^2 / (2.0 * sig[,2L]^2)) 

    q1Hat[,1L] <- mu[,1L] * (1.0 - 2.0 * pnorm(-mu[,1L] / sig[,1L])) +
                  sqrt(2.0 / pi) * sig[,1L] * 
                    exp(-mu[,1L]^2 / (2.0 * sig[,1L]^2)) 

  } else {
    func <- function(x,y,r,t,a){
              tma <- as.numeric(t==a)
              if( sum(tma) > 0.0 ) {
                return(sum(abs(x + y*r)*tma)/sum(tma))
              } else {
                return(0.0)
              }
            }

    q1Hat[,2L] <- mapply(func, mu[,2L], sig[,2L], 
                         MoreArgs = list(r = rso, 
                                         t = as.integer(round(txVec,0)),  
                                         a = 1L))
    q1Hat[,1L] <- mapply(func, mu[,1L], sig[,1L], 
                         MoreArgs = list(r = rso, 
                                         t = as.integer(round(txVec,0)),  
                                         a = -1L))

  }

  q1Hat <- lhat + q1Hat

  return( q1Hat )

}

setMethod(f = "optTx", 
          signature = c(x = "IQLearnFS",
                        newdata = "data.frame"), 
          definition = function(x, newdata, ..., y=NULL, z=NULL, dens=NULL){

                         if( {is(y,"NULL") & is(z,"NULL")} || is(dens,"NULL") ) {
                           stop("must provide at least two first stage objects and dens")
                         }                         
                         me <- NULL
                         co <- NULL
                         so <- NULL

                         if( is(y, "IQLearnFS_ME") ) {
                           me <- y
                         } else if( is(y, "IQLearnFS_C") ) {
                           co <- y
                         } else if( is(y, "IQLearnFS_VHet") ) {
                           so <- y
                         }


                         if( is(z, "IQLearnFS_ME") ) {
                           me <- z
                         } else if( is(z, "IQLearnFS_C") ) {
                           co <- z
                         } else if( is(z, "IQLearnFS_VHet") ) {
                           so <- z
                         }

                         if( is(x, "IQLearnFS_ME") ) {
                           me <- x
                         } else if( is(x, "IQLearnFS_C") ) {
                           co <- x
                         } else if( is(x, "IQLearnFS_VHet") ) {
                           so <- x
                         }

                         if( is(me,"NULL") ) {
                           stop("no IQLearnFS_ME object provided")
                         }

                         lhat <- .predictAllTreatments(object = me@outcome, 
                                                       data = newdata,
                                                       response = rep(NA,nrow(newdata)))$vals

                         if( is(co, "NULL") ) {
                           stop("no IQLearnFS_C object provided")
                         }

                         mu <- .predictAllTreatments(object = co@outcome, 
                                                     data = newdata,
                                                       response = rep(NA,nrow(newdata)))$vals

                         if( !is(so, "NULL") ) {
                           sig <- .predictAllTreatments(object = so@outcome, 
                                                        data = newdata,
                                                       response = rep(NA,nrow(newdata)))$vals

                           sig <- exp(sig / 2.0) * exp(so@scale / 2.0)

                           if( dens=="norm" ) {
                             rso <- NULL
                           } else {
                             rso <- residuals(so)
                           }
                         } else {
                           sig <- matrix(sd(co), ncol=2L)

                           if( dens=="norm" ) {
                             rso <- NULL
                           } else {
                             rso <- scale(residuals(co),center=FALSE)
                           }
                         }

                         qf <- .qfIQLearnFS(lhat = lhat, 
                                            mu = mu, 
                                            sig = sig, 
                                            dens = dens, 
                                            txVec = co@txVec, 
                                            rso = rso)
 
                         #-------------------------------------------#
                         # Optimal tx is that with the largest value #
                         # function                                  #
                         #-------------------------------------------#
                         optTx <- apply(X = qf, 
                                        MARGIN = 1L, 
                                        FUN = which.max)
                         optTx <- c(-1L,1L)[optTx]
  
                         return( list("decisionFunc" = qf, 
                                      "optimalTx"  = optTx) )
                       } )


setMethod(f = "optTx", 
          signature = c(x = "IQLearnFS",
                        newdata = "missing"), 
          definition = function(x, newdata, ..., y = NULL, z = NULL, dens = NULL){

                         if( {is(y,"NULL") & is(z,"NULL")} || is(dens,"NULL") ) {
                           stop("must provide at least two first stage objects and dens")
                         }                         

                         me <- NULL
                         co <- NULL
                         so <- NULL

                         if( is(y, "IQLearnFS_ME") ) {
                           me <- y
                         } else if( is(y, "IQLearnFS_C") ) {
                           co <- y
                         } else if( is(y, "IQLearnFS_VHet") ) {
                           so <- y
                         }


                         if( is(z, "IQLearnFS_ME") ) {
                           me <- z
                         } else if( is(z, "IQLearnFS_C") ) {
                           co <- z
                         } else if( is(z, "IQLearnFS_VHet") ) {
                           so <- z
                         }

                         if( is(x, "IQLearnFS_ME") ) {
                           me <- x
                         } else if( is(x, "IQLearnFS_C") ) {
                           co <- x
                         } else if( is(x, "IQLearnFS_VHet") ) {
                           so <- x
                         }

                         if( is(me,"NULL") ) {
                           stop("no IQLearnFS_ME object provided")
                         }

                         lhat <- me@qFunc

                         if( is(co, "NULL") ) {
                           stop("no IQLearnFS_C object provided")
                         }

                         mu <- co@qFunc

                         if( !is(so, "NULL") ) {
                           sig <- so@qFunc
                           sig <- exp(sig / 2.0) * exp(so@scale / 2.0)

                           if( dens=="norm" ) {
                             rso <- NULL
                           } else {
                             rso <- residuals(so)
                           }
                         } else {
                           sig <- matrix(sd(co), ncol=2L)

                           if( dens=="norm" ) {
                             rso <- NULL
                           } else {
                             rso <- scale(residuals(co),center=FALSE)
                           }
                         }

                         qf <- .qfIQLearnFS(lhat = lhat, 
                                            mu = mu, 
                                            sig = sig, 
                                            dens = dens, 
                                            txVec = co@txVec, 
                                            rso = rso)
 
                         #-------------------------------------------#
                         # Optimal tx is that with the largest value #
                         # function                                  #
                         #-------------------------------------------#
                         optTx <- apply(X = qf, 
                                        MARGIN = 1L, 
                                        FUN = which.max)
                         optTx <- c(-1L,1L)[optTx]
  
                         return( list("decisionFunc" = qf, 
                                      "optimalTx"  = optTx) )
                       } )


setMethod(f = "estimator",    
          signature = c(x = "IQLearnFS"), 
          definition = function(x,  w = NULL, y = NULL, z = NULL, dens = NULL){ 

                         tst <- c(is(w,"NULL"), is(y,"NULL"), is(z,"NULL"))
                         if( sum(tst) > 1L || is(dens,"NULL") ) {
                           stop("must provide at least 3 IQLearn results and dens")
                         }

                         me <- NULL
                         co <- NULL
                         so <- NULL
                         ss <- NULL

                         if( is(w, "IQLearnFS_ME") ) {
                           me <- w
                         } else if( is(w, "IQLearnFS_C") ) {
                           co <- w
                         } else if( is(w, "IQLearnFS_VHet") ) {
                           so <- w
                         } else if( is(w, "IQLearnSS") ) {
                           ss <- w
                         }

                         if( is(y, "IQLearnFS_ME") ) {
                           me <- y
                         } else if( is(y, "IQLearnFS_C") ) {
                           co <- y
                         } else if( is(y, "IQLearnFS_VHet") ) {
                           so <- y
                         } else if( is(y, "IQLearnSS") ) {
                           ss <- y
                         }


                         if( is(z, "IQLearnFS_ME") ) {
                           me <- z
                         } else if( is(z, "IQLearnFS_C") ) {
                           co <- z
                         } else if( is(z, "IQLearnFS_VHet") ) {
                           so <- z
                         } else if( is(z, "IQLearnSS") ) {
                           ss <- z
                         }

                         if( is(x, "IQLearnFS_ME") ) {
                           me <- x
                         } else if( is(x, "IQLearnFS_C") ) {
                           co <- x
                         } else if( is(x, "IQLearnFS_VHet") ) {
                           so <- x
                         } else if( is(x, "IQLearnSS") ) {
                           ss <- x
                         }

                         dens <- tolower(dens)

                         if( !(dens %in% c("norm","nonpar")) ) {
                           msg <- "dens must be one of {norm, nonpar}"
                           e <- simpleError(msg)
                           stop(e)
                         }

                         q1res <- optTx(x = me,
                                        y = co,
                                        z = so,
                                        dens = dens)

                         q2res <- ss@delta * 
                                  {co@txVec == q1res$optimalTx}

                         return( sum(q2res)/sum(abs(q2res)>0.0) )

                       } )


setMethod(f = "estimator",    
          signature = c(x = "IQLearnSS"), 
          definition = function(x, w = NULL, y = NULL, z = NULL, dens = NULL){ 

                         tst <- c(is(w,"NULL"), is(y,"NULL"), is(z,"NULL"))
                         if( sum(tst) > 1L || is(dens,"NULL") ) {
                           stop("must provide at least 3 IQLearn results and dens")
                         }

                         me <- NULL
                         co <- NULL
                         so <- NULL

                         if( is(w, "IQLearnFS_ME") ) {
                           me <- w
                         } else if( is(w, "IQLearnFS_C") ) {
                           co <- w
                         } else if( is(w, "IQLearnFS_VHet") ) {
                           so <- w
                         }

                         if( is(y, "IQLearnFS_ME") ) {
                           me <- y
                         } else if( is(y, "IQLearnFS_C") ) {
                           co <- y
                         } else if( is(y, "IQLearnFS_VHet") ) {
                           so <- y
                         }


                         if( is(z, "IQLearnFS_ME") ) {
                           me <- z
                         } else if( is(z, "IQLearnFS_C") ) {
                           co <- z
                         } else if( is(z, "IQLearnFS_VHet") ) {
                           so <- z
                         }

                         dens <- tolower(dens)

                         q1res <- optTx(x = me,
                                        y = co,
                                        z = so,
                                        dens = dens)

                         q2res <- x@delta * {co@txVec == q1res$optimalTx}

                         return( sum(q2res)/sum(abs(q2res)>0.0) )

} )

