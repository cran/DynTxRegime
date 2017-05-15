setClassUnion("NumericOrNULL",
              members = c("numeric", "NULL"))
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         Class OptimKernel                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Optimization step for Kernel Methods                                 #
#----------------------------------------------------------------------#
# extends OptimKernel
# covariates : matrix, covariates of kernel function
# kernel     : character, description of kernel function
# kParam     : numeric, parameter for kernel function
#----------------------------------------------------------------------#
setClass(Class = "OptimKernel",
         slots = c(covariates = "matrix",
                       kernel = "character",
                       kParam = "NumericOrNULL"),
         contains = c("OptimBasic"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       OptimKernel METHODS                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve key results of the optimization step                        #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimKernel                              #
#   returns                                                            #
# a list object with information from optimization step                #
#----------------------------------------------------------------------#
setMethod(f = "optimObj",
          signature = c(object="OptimKernel"),
          definition = function(object, ...) { 
                         res <- optimObj(object = as(object,"OptimBasic"))
                         res[[ "kernel" ]] <- object@kernel
                         res[[ "kParam" ]] <- object@kParam
                         return( res )  
                       } )

#----------------------------------------------------------------------#
# Print key results.                                                   #
#----------------------------------------------------------------------#
#   params                                                             #
# x : an object of class OptimKernel                                      #
#----------------------------------------------------------------------#
setMethod(f = "print",    
          signature = c(x = "OptimKernel"), 
          definition = function(x, ...){

                         cat("kernel= ", x@kernel,
                             "kparam= ", x@kParam,"\n")
                         print(x = as(x,"OptimBasic"))
                       } )

#----------------------------------------------------------------------#
# Show key results.                                                    #
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class OptimKernel                                 #
#   returns                                                            #
# Nothing returned                                                     #
#----------------------------------------------------------------------#
setMethod(f = "show",    
          signature = c(object = "OptimKernel"), 
          definition = function(object){
                         cat("kernel= ", object@kernel,
                             "kparam= ", object@kParam, "\n")
                         show(object = as(object,"OptimBasic"))
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
          signature = c(object = "OptimKernel"), 
          definition = function(object, ...){
                         res <- summary(object = as(object,"OptimBasic"))
                         res[[ "kernel" ]]  <- object@kernel
                         res[[ "kparam" ]]  <- object@kParam
                         return(res)
                       } )
