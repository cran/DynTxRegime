# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                           CLASS QLearn                           + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of a single step of Q-Learning algorithm                     #
#----------------------------------------------------------------------#
#   step         : Step in the Q-Learning algorithm                    #
#   decisionFunc : Matrix of Q-functions                               #
#   outcome      : OutcomeRegression object.                           #
#----------------------------------------------------------------------#
.checkValidity_QLearn <- function(object) {

  errors <- character()

  if( !is(object@outcome, "SingleDecisionPoint") ) {
    msg <- "outcome is not of appropriate class"
    errors <- c(errors, msg)
  }

  if( object@step <= 0L ) {
    msg <- "step must be > 0"
    errors <- c(errors,msg)
  }

  if( nrow(object@decisionFunc) == 0L ||
      ncol(object@decisionFunc) == 0L ) {
    msg <- "decisionFunc not defined properly"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "QLearn",
         slots = c(step        = "integer",
                   decisionFunc = "matrix"),
         contains = c("OutcomeOnly", "DynTxRegime"),
         validity = .checkValidity_QLearn )


# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                          QLearn METHODS                          + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve a string describing the method                              #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class QLearn                                   #
#   returns                                                            #
# a string describing method                                           #
#----------------------------------------------------------------------#
setMethod(f = "DTRstep",
          signature = c(object = "QLearn"),
          definition = function(object){
                         res <- paste("Q-Learning: step", object@step)
                         return( res )
                       } )


#----------------------------------------------------------------------#
# Estimate optimal treatment for new data                              #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class QLearn                                        #
# newdata : a data.frame of covariates and treatments for which the    #
# optimal treatment is to be estimated.                                #
#   returns                                                            #
# the estimated optimal treatment for data provided through newdata    #
# and the q-functions at each treatment.                               #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "QLearn",
                        newdata = "data.frame"),
          definition = function(x, newdata,...) {

          	               qf <- .predictAllTreatments(object = x@outcome,
                                                     data = newdata,
                                                     response = rep(1/0,nrow(newdata)))


                         single <- apply(X = qf$vals,
                                         MARGIN = 1L,
                                         FUN = function(x){any(is.infinite(x))})

                         opt <- apply(X = qf$vals, MARGIN = 1L, FUN = which.max)

                         qf$vals[is.infinite(qf$vals)] <- NA

                         opt <- qf$ss[opt]

                         return( list("optimalTx" = opt,
                                      "decisionFunc" = qf$vals) )
                       } )

#----------------------------------------------------------------------#
# Retrieve optimal treatment for training data                         #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class QLearn                                        #
# newdata : a data.frame of covariates and treatments for which the    #
# optimal treatment is to be estimated.                                #
#   returns                                                            #
# the estimated optimal treatment for training data                    #
# and the q-functions at each treatment.                               #
#----------------------------------------------------------------------#
setMethod(f = "optTx",
          signature = c(x = "QLearn",
                        newdata = "missing"),
          definition = function(x, newdata,...) {

                         return( list("optimalTx" = x@optimalTx,
                                      "decisionFunc" = x@decisionFunc) )
                       } )

#----------------------------------------------------------------------#
# Print the key results of method.                                     #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class QLearn                                        #
#----------------------------------------------------------------------#
setMethod(f = "print",
          signature = c(x = "QLearn"),
          definition = function(x, ...){
            cat("\n", DTRstep(x), "\n")
            print(as(x,"OutcomeOnly"))
            cat("\nValue: ", estimator(x), "\n")
          } )

#----------------------------------------------------------------------#
# Show the key results of method.                                      #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class QLearn                                   #
#----------------------------------------------------------------------#
setMethod(f = "show",
          signature = c(object = "QLearn"),
          definition = function(object){
            cat("\n", DTRstep(object), "\n")
            show(as(object,"OutcomeOnly"))
            cat("Value: ", estimator(object), "\n")
          } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the outcome regression and method     #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class QLearn                                   #
#   returns                                                            #
# a list, structure and content determined by regression class         #
#----------------------------------------------------------------------#
setMethod(f = "summary",
          signature = c(object = "QLearn"),
          definition = function(object, ...){
            res <- summary(as(object,"OutcomeOnly"))
            res[[ "value" ]] <- estimator(object)
            return(res)
          } )
