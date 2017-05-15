
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                          Class OWLOptim                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Optimization step of the Outcome Weighted Learning algorithm         #
#----------------------------------------------------------------------#
setClass(Class = "OWLOptim",
         slots = c(txVec = "numeric"),
         contains = c("OptimKernel"))

setGeneric(name = ".newOWLOptim",
           def = function(x, ...){
                   standardGeneric(".newOWLOptim")
                 })
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         OWLOptim METHODS                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve key results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OWLOptim                                 #
#   returns                                                            #
# a list object with information from optimization step                #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="OWLOptim"),
          definition = function(object, ...) {
                         res <- optimObj(object = as(object, "OptimKernel"))
                         res[[ "txVec" ]] <- object@txVec
                         return( res )
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
          signature = c(x = "OWLOptim",
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
                         fx <- colSums(kernlab::primal(x@optim) * x@txVec * kern) - kernlab::dual(x@optim)
                         names(fx) <- NULL

                         #-------------------------------------------#
                         # optimal treatment is the sign of f(x)     #
                         #-------------------------------------------#
                         opt <- sign(fx)

                         return( list("optimalTx"    = opt,
                                      "decisionFunc" = fx) )

                       } )

setMethod(f = ".predictOptimalTx",
          signature = c(x = "OWLOptim",
                        newdata = "data.frame"),
          definition = function (x, newdata,...){
                         return(.predictOptimalTx(x, data.matrix(newdata)))
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
          signature = c(object = "OWLOptim"),
          definition = function(object, ...){
                         return( c(-kernlab::dual(object@optim),
                                   kernlab::primal(object@optim)) )
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the optimization                      #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimKernel                              #
#   returns                                                            #
# a list, key results from the optimization                            #
#----------------------------------------------------------------------#
setMethod(f = "summary",
          signature = c(object = "OWLOptim"),
          definition = function(object, ...){
                         res <- summary(object = as(object, "OptimKernel"))
                         res[[ "txVec" ]]  <- object@txVec
                         return(res)
                       } )

#----------------------------------------------------------------------#
# create a new OWLOptim object                                         #
#----------------------------------------------------------------------#
#   params                                                             #
# x         : matrix of covariates for kernel                          #
# subset    : vector of patients to include in training                #
# lambda    : regularization parameter                                 #
# txVec     : vector of treatments coded as +/-1                       #
# prWgt     : vector of propensity valued according to tx              #
#             ie if patient received + tx, pi; if - tx 1-pi            #
# response  : vector of response                                       #
# suppress  : T/F indicating if prints to screen are executed.         #
# kernel    : character description of kernel to be used               #
# kparam    : numeric object, value of parameter in kernel             #
#   returns                                                            #
# an object of class OWLOptim                                          #
#----------------------------------------------------------------------#
.owlOptim <- function(x,
                      subset,
                      lambda,
                      txVec,
                      prWgt,
                      response,
                      suppress,
                      kernel,
                      kparam, ...) {

  #------------------------------------------------------------------#
  # Remove cases not to be included in optimization                  #
  #------------------------------------------------------------------#
  x <- x[subset,,drop=FALSE]
  txVec <- txVec[subset]
  prWgt <- prWgt[subset]
  response <- response[subset]

  #------------------------------------------------------------------#
  # Calculate Kernel matrix                                          #
  #------------------------------------------------------------------#
  kern <- .kernelFunc(X1 = x, X2 = x, kern = kernel, param = kparam)

  #------------------------------------------------------------------#
  # Define objective function                                        #
  # kernlab::ipop uses the following notation:                       #
  # min( c'x + 1/2 x' H x)                                           #
  # s.t. b <= A x <= b + r; l <= x <= u                              #
  #------------------------------------------------------------------#
  n <- nrow(x)

  #------------------------------------------------------------------#
  # Calculate weights                                                #
  #------------------------------------------------------------------#
  w <- lambda * response / prWgt

  #------------------------------------------------------------------#
  # c is a negative sign.                                            #
  #------------------------------------------------------------------#
  cVec <- matrix(-1.0, nrow = n, ncol = 1L)

  #------------------------------------------------------------------#
  # Equality condition: sum of treatments * parameters = 0.0         #
  #------------------------------------------------------------------#
  aMatrix <- matrix(txVec, nrow = 1L, ncol = n)

  #------------------------------------------------------------------#
  # H = txVec * kernel matrix * txVec                                #
  #------------------------------------------------------------------#
  h1 <- txVec %o% txVec
  H <- h1 * kern

  #------------------------------------------------------------------#
  # Optimize.                                                        #
  #------------------------------------------------------------------#
  optimResults <- try(kernlab::ipop(c = cVec,
                                    H = H,
                                    A = aMatrix,
                                    b = 0.0,
                                    l = rep(0.0, n),
                                    u = w,
                                    r = 0.0,
                                    verb = as.numeric(!suppress)),
                      silent = FALSE)

  if( is(optimResults, "try-error") ||
      optimResults@how != "converged" ) {
    stop("kerfnlab::ipop did not converge or encountered errors.\n")
  }

  if( !suppress ) {
    cat("\nOptimization results\n")
    print(optimResults)
  }

  res <- new("OWLOptim",
             "txVec"      = txVec,
             "covariates" = x,
             "kernel"     = kernel,
             "kParam"     = kparam,
             "lambda"     = lambda,
             "optim"      = optimResults)

  return( res )
}

setMethod(f = ".newOWLOptim",
          signature = c(x = "matrix"),
          definition = .owlOptim)
