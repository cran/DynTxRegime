# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         Class OptimBasic                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Optimization step of the Outcome Weighted Learning algorithm         #
#----------------------------------------------------------------------#
# threshold  : named numeric, parameter associated with intercept of   #
#              decision function                                       #
# alpha      : numeric, parameter estimates                            #
# lambda     : numeric, tuning parameter                               #
# optim      : value object returned by kernlab::ipop                  #
#----------------------------------------------------------------------#
setClass(Class = "OptimBasic",
         slots = c(lambda = "numeric",
                    optim = "ANY"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        OptimBasic METHODS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve key results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimBasic                               #
#   returns                                                            #
# a list object with information from optimization step                #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="OptimBasic"),
          definition = function(object, ...) { 
                         res <- list()
                         res[[ "optim" ]]   <- object@optim
                         res[[ "lambda" ]]  <- object@lambda
                         return( res )  
                       } )

#----------------------------------------------------------------------#
# Print key results.                                                   #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimBasic                                    #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimBasic"), 
          definition = function(x, ...){
                         cat("lambda= ", x@lambda, "\n")
                         print(x@optim)
                       } )

#----------------------------------------------------------------------#
# Show key results.                                                    #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimBasic                                 #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimBasic"), 
          definition = function(object){
                         cat("lambda=  ", object@lambda, "\n")
                         show(object@optim)
                       } )

#----------------------------------------------------------------------#
# Retrieve the summary object of the optimization                      #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimBasic                                 #
#   returns                                                            #
# a list, key results from the optimization                            #
#----------------------------------------------------------------------#
setMethod(f = "summary",    
          signature = c(object = "OptimBasic"), 
          definition = function(object, ...){
                         res <- list()
                         res[[ "lambda" ]]  <- object@lambda
                         res[[ "optim" ]] <- object@optim
                         return(res)
                       } )


