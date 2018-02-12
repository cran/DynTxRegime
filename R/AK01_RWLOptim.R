#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                            Class RWLOptim                            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# Results of the optimization step of Residual Weighted Learning       #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
# beta       : numeric, derivative of concave objective function       # 
# covariates : matrix of covariates and treatment histories            #
# optim      : value object returned by optim                          #
# kernel     : character description of kernel function                #
# kParam     : numeric parameter for kernel function                   #
# residuals  : residuals of outcome regression                         #
# lambda     : numeric tuning parameter                                #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
setClass(Class = "RWLOptim",
         slots = c(       beta = "numeric",
                     residuals = "numeric"),
         contains = c("OptimKernel"))

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                          RWLOptim GENERICS                           #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

setGeneric(name = ".newRWLOptim", 
           def = function(x, ...){
                   standardGeneric(".newRWLOptim")
                 })

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#
#                          RWLOptim METHODS                            #
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++#

#----------------------------------------------------------------------#
# Retrieve the results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWLOptim                                 #
#   returns                                                            #
# a list object with key information from optimization routine         #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="RWLOptim"),
          definition = function(object, ...) { 
                         res <- optimObj(object = as(object,"OptimKernel"))
                         res[[ "beta" ]]   <- object@beta
                         return(res)  
                       } )

#----------------------------------------------------------------------#
# Print the key results.                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class RWLOptim                                      #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "RWLOptim"), 
          definition = function(x, ...){
                         print("beta")
                         print(x@beta)
                         print(x = as(x,"OptimKernel"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the residuals                                               # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWLOptim                                 #
#   returns                                                            #
# a numeric vector                                                     #
#----------------------------------------------------------------------#
setMethod(f = "residuals",
          signature = c(object = "RWLOptim"),
          definition = function(object, ...){
                         return( object@residuals )
                       } )

#----------------------------------------------------------------------#
# Predict optimal treatment for new data                               #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimKernel                                   #
# newdata : model matrix of new data.                                  #
#   returns                                                            #
# A list containing f(x) and its sign                                  #
#----------------------------------------------------------------------#
setMethod(f = ".predictOptimalTx",
          signature = c(x = "RWLOptim", 
                        newdata = "matrix"),
          definition = function (x, newdata,...){

                         #-------------------------------------------#
                         # Calculate kernel function with newdata    #
                         #-------------------------------------------#
                         kern <- .kernelFunc(X1 = x@covariates, 
                                             X2 = newdata, 
                                             kern = x@kernel, 
                                             param = x@kParam)

                         #-------------------------------------------#
                         # Calculate decision function               #
                         #-------------------------------------------#
                         fx <- colSums(x@optim$par[-1L] * kern) - x@optim$par[1L]

                         names(fx) <- NULL

                         #-------------------------------------------#
                         # optimal treatment is the sign of f(x)     #
                         #-------------------------------------------#
                         opt <- sign(fx)

                         return( list("optimalTx"    = opt,
                                      "decisionFunc" = fx) )

                       } )

setMethod(f = ".predictOptimalTx",
          signature = c(x = "RWLOptim", 
                        newdata = "data.frame"),
          definition = function (x, newdata,...){
                         newdata2 <- data.matrix(newdata)
                         if( !is.matrix(newdata2) ) newdata2 <- matrix(newdata2,nrow=nrow(newdata))
                         return(.predictOptimalTx(x, newdata2))
                       } )

#----------------------------------------------------------------------#
# Retrieve the parameter estimates for the class of regimes            # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OWLOptim                                 #
#   returns                                                            #
# The parameter estimates for the decision function                    #
#----------------------------------------------------------------------#
setMethod(f = "regimeCoef",    
          signature = c(object = "RWLOptim"), 
          definition = function(object, ...){
                         return( object@optim$par )
                       } )

#----------------------------------------------------------------------#
# Show the key results.                                                # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWLOptim                                 #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "RWLOptim"), 
          definition = function(object){
                         cat("beta\n")
                         show(object@beta)
                         show(object = as(object,"OptimKernel"))
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the optimization                      # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class RWLOptim                                 #
#   returns                                                            #
# a list, key results from the optimization                            #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "RWLOptim"), 
          definition = function(object, ...){
                         res <- summary(object = as(object,"OptimKernel"))
                         res[[ "beta" ]]   <- object@beta
                         return(res)  
                       } )

#----------------------------------------------------------------------#
# x         : matrix of covariates for kernel function                 #
# subset    : subset of data to be used for training                   #
# kernel    : character description of kernel to be used               #
# kparam    : numeric value of parameter in kernel                     #
# lambda    : regularization parameters                                #
# txVec     : vector of treatments coded as -1.0/1.0                   #
# prWgt     : propensity weight vector (pi tx=1, 1-pi tx=-1)           #
# residual  : vector of residuals                                      #
# response  : vector of response                                       #
# guess     : initial guess for optimization routine                   #
# suppress  : T/F indicating if screen prints are generated.           #
#----------------------------------------------------------------------#
.rwlOptim <- function(x, 
                      subset, 
                      kernel, 
                      kparam, 
                      lambda, 
                      txVec, 
                      prWgt,
                      residual, 
                      response, 
                      guess,
                      suppress) {

  #------------------------------------------------------------------#
  # Limit inputs to the subset of data being considered.             #
  #------------------------------------------------------------------#
  x <- x[subset,,drop=FALSE]
  txVec <- txVec[subset]
  prWgt <- prWgt[subset]
  residual <- residual[subset]
  response <- response[subset]

  #------------------------------------------------------------------#
  # Define objective function                                        #
  #------------------------------------------------------------------#
  objFn <- function(par, lambda, res, wgt, beta, 
                    kernel, tkern, txVec) {

             bee <- par[ 1L]
             vee <- par[-1L]

             temp <- kernel %*% vee

             u <- txVec * {drop(temp) + bee}

             lims <- res > 0.0

             phi1 <- {1.0 - u} * {1.0 - u} * {{0.0 <= u} & {u < 1.0}} +
                     {1.0 - 2.0 * u} * {u < 0.0}

             phi0 <- u * u * {{-1.0 <= u} & {u < 0.0}} - 
                     {2.0 * u + 1.0} * {u < -1.0}

             tFunc <- phi1 * lims + phi0 * {1.0 - lims}

             veevee <- vee %*% temp

             res <- {lambda / 2.0} * veevee + 
                    mean(tFunc * wgt) + sum(beta * u)

             return(res)

           }

  dobjFn <- function(par, lambda, res, wgt, beta, 
                    kernel, tkern, txVec) {

             bee <- par[ 1L]
             vee <- par[-1L]

             temp <- drop(kernel %*% vee)

             u <- txVec * {temp + bee}

             dudv <- tkern
             dudb <- txVec

             lims <- res > 0.0

             phi1lim1 <- {0.0 <= u} & {u < 1.0}
             phi1lim2 <- u < 0.0

             phi0lim1 <- {-1.0 <= u} & {u < 0.0}
             phi0lim2 <- u < -1.0

             onemu <- 1.0 - u

             tFuncdv <- 2.0 * {{-lims * {phi1lim1 * onemu + phi1lim2}} + 
                               {{1.0 - lims} * {phi0lim1 * u - phi0lim2}}} * dudv

             tFuncdb <- 2.0 * {{-lims * {phi1lim1 * onemu + phi1lim2}} + 
                               {{1.0 - lims} * {phi0lim1 * u - phi0lim2}}} * dudb

             dresdv <- lambda * temp + 
                       colMeans(tFuncdv * wgt) + colSums(beta * dudv)

             dresdb <- mean(tFuncdb * wgt) + sum(beta * dudb)

             return(c(dresdb,dresdv))

           }

  n <- nrow(x)
  p <- ncol(x)

  bee <- NULL
  vee <- NULL

  maxIter <- 1000L

  #------------------------------------------------------------------#
  # Absolute value of residuals                                      #
  #------------------------------------------------------------------#
  wgt <- abs(residual) / prWgt

  #------------------------------------------------------------------#
  # Calculate kernel                                                 #
  #------------------------------------------------------------------#
  kern <- .kernelFunc(X1 = x, X2 = x, kern = kernel, param = kparam)
  tkern <- txVec * kern

  #------------------------------------------------------------------#
  # Initialize beta                                                  #
  #------------------------------------------------------------------#
  beta <- 2.0 * wgt / n * {residual < 0.0}

  #------------------------------------------------------------------#
  # parameter estimates initialized to 0 or guess                    #
  #------------------------------------------------------------------#
  if( is(guess, "NULL") || length(guess) != {n+1L} ) {
    par <- numeric(n + 1L)
  } else {
    par <- guess
  }

  if( !suppress ) {
    cArg <- list("info" = TRUE)
  } else {
    cArg <- list()
  }

  iter <- 0L
  while( iter < maxIter ) {

    if( !suppress ) cat(iter, "")

    #--------------------------------------------------------------#
    # Perform optimization to obtain parameter estimates.          #
    #--------------------------------------------------------------#
    optimResult <- optim(fn = objFn, 
                         gr = dobjFn,
                         par = par,
                         method = "BFGS",
                         lambda = lambda, 
                         res = residual, 
                         wgt = wgt, 
                         beta = beta, 
                         kernel = kern, 
                         tkern = tkern,
                         txVec = txVec)

    #--------------------------------------------------------------#
    # If optimization did not converge randomize parameters and    #
    # try again.                                                   #
    #--------------------------------------------------------------#
    if( optimResult$convergence > 1.5 ) {
      par <- runif(n + 1L)
      iter <- iter + 1L
      next
    } else if( optimResult$convergence > 0.5 ) {
      par <- optimResult$par
      next
    }

    #--------------------------------------------------------------#
    # Retrieve parameter estimates                                 #
    #--------------------------------------------------------------#
    bee <- optimResult$par[ 1L]
    vee <- optimResult$par[-1L]

    #--------------------------------------------------------------#
    # Calculate u at these parameters                              #
    #--------------------------------------------------------------#
    u <- txVec * {drop(kern %*% vee) + bee}

    #--------------------------------------------------------------#
    # Calculate derivative of phi at these parameters              #
    #--------------------------------------------------------------#
    dphi1du <- - 2.0 * { {1.0 - u} * {{0.0 <= u} & {u < 1.0}} + 
                                   {u < 0.0} }
    dphi0du <- - 2.0 * { -u * {{-1.0 <= u} & {u < 0.0}} + 
                            {u < -1.0} }

    #--------------------------------------------------------------#
    # Identify those residuals that are negative                   #
    #--------------------------------------------------------------#
    lims <- residual < 0.0

    #--------------------------------------------------------------#
    # Calculate new estimates for beta                             #
    #--------------------------------------------------------------#
    betaN <- -{dphi1du * lims + dphi0du * {1.0-lims}} * wgt / n

    #--------------------------------------------------------------#
    # If new beta are same as old beta, exit loop                  #
    #--------------------------------------------------------------#
    if( !suppress ) cat("max beta diff: ", max(abs(betaN-beta)),"\n")
    if( all( abs(betaN - beta) < 1.e-7) ) {
      beta <- betaN
      break
    }

    #--------------------------------------------------------------#
    # Store these beta estimates for next iteration                #
    #--------------------------------------------------------------#
    beta <- betaN

    #--------------------------------------------------------------#
    # Increment iteration count.                                   #
    #--------------------------------------------------------------#
    iter <- iter + 1L

    #--------------------------------------------------------------#
    # Set initial parameter estimates for next iteration to        #
    # parameter estimates obtained in this iteration.              #
    #--------------------------------------------------------------#
    par <- optimResult$par
  }

  #------------------------------------------------------------------#
  # If max iteration was reached and optim did not converge, warn    #
  #------------------------------------------------------------------#
  if( iter > maxIter && optimResult$convergence > 0.5 ) {
    cat("optimization did not converge.\n")
    return( NULL )
  }

  if( !suppress ) {
    cat("\n")
    cat("Optimization results\n")
    print(optimResult)
    cat("Betas\n")
    print(beta)
  }

  if( is(bee, "NULL") ) {
    stop("optimization was unsuccessful")
  }

  names(beta) <- NULL

  object <- new("RWLOptim",
                "lambda"     = lambda,
                "optim"      = optimResult,
                "beta"       = beta,
                "covariates" = x,
                "kernel"     = kernel,
                "kParam"     = kparam,
                "residuals"  = residual)

  return(object)
}

setMethod(f = ".newRWLOptim",    
          signature = c(x =  "matrix"), 
          definition = .rwlOptim)


setMethod(f = ".newRWLOptim",    
          signature = c(x = "ANY"), 
          definition = function(x, ...) {
                         badMethod(nm = ".newRWLOptim", x)
                       } )


