setClass(Class = "OutcomeRegression", contains = c("VIRTUAL"))

setClass(Class = "SubsetsModeled", contains = c("VIRTUAL"))

setClass(Class = "SubsetsNotModeled", contains = c("VIRTUAL"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                        TypedSimpleFit CLASS                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of an outcome regression call obtained with a single model   #
#----------------------------------------------------------------------#
# Extends modelObjFit of modelObj package, SingleDecisionPoint,
# OutcomeRegression, and SubsetsNotModeled directly
#
# slots:
#
#   type : a string indicating 'Combined', 'moMain' or 'moCont'
#   txInfo : TxInfoBasic
#   fitObj : ANY, value returned by fitting function
#
#----------------------------------------------------------------------#
.checkValidity_TypedSimpleFit <- function(object) {

  errors <- character()

  if( !object@type %in% c("Combined", "moMain", "moCont") ) {
    msg <- "inappropriate type"
    errors <- c(errors, msg)
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }

}

setClass(Class = "TypedSimpleFit",
         slots = c(type = "character",
                   txInfo = "ANY"),
         contains = c("modelObjFit",
                      "SubsetsNotModeled",  
                      "SingleDecisionPoint",
                      "OutcomeRegression"),
         validity = .checkValidity_TypedSimpleFit)

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      TypedSimpleFit GENERICS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setGeneric(name = ".predictAllTreatments", 
           def = function(object, data, ...){
                   standardGeneric(".predictAllTreatments")
                 })

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      TypedSimpleFit METHODS                      + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Retrieve the coefficient estimates                                   # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TypedSimpleFit                         #
# returns                                                              #
#   a list; elements are defined in the inherited class modelObjFit.   #
#----------------------------------------------------------------------#
setMethod(f = "coef", 
          signature = c(object = "TypedSimpleFit"), 
          definition = function(object, ...){
                         res <- list()
                         res[[ object@type ]] <- coef(object = as(object, "modelObjFit"))
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Retrieve the regression results                                      # 
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : an object of class TypedSimpleFit                         #
# returns                                                              #
#   a list; elements are defined in the inherited class modelObjFit.   #
#----------------------------------------------------------------------#
setMethod(f = "fitObject", 
          signature = c(object = "TypedSimpleFit"), 
          definition = function(object, ...){
                         res <- list()
                         res[[ object@type ]] <- fitObject(object = as(object, "modelObjFit"))
                         return( res )
                       } )

#----------------------------------------------------------------------#
# Generate plots of the regression results                             #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   x : an object of class TypedSimpleFit                              #
#   suppress : logical indicating if titles should be augmented with   #
#              @type information                                       #
# returns                                                              #
#   No object return.                                                  #
#----------------------------------------------------------------------#
setMethod(f = "plot", 
          signature = c(x = "TypedSimpleFit"), 
          definition = function(x, suppress=FALSE, ...){

                         argList <- list(...)
                         if( !suppress ) {
                           argList <- .titleIt(argList, x@type)
                         }

                         argList[[ "x" ]] <- fitObject(object = as(x,"modelObjFit"))

                         do.call(what = modelObj::plot, args = argList)

                       } )


#----------------------------------------------------------------------#
# Predict the outcome regression                                       #
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class TypedSimpleFit.                          #
# data : data.frame of covariates and treatment history                #
#   returns                                                            #
# A numeric vector. The estimated outcome.                             #
#----------------------------------------------------------------------#
setMethod(f = "predict", 
          signature = c(object = "TypedSimpleFit"), 
          definition = function(object,...){ 
                         return(predict(object = as(object,"modelObjFit"),...)) } )

#----------------------------------------------------------------------#
# Predict the outcome regression for all treatment options             #
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class TypedSimpleFitNoSubsets.                 #
# data : data.frame of covariates and treatment history                #
#   returns                                                            #
# A matrix. The estimated outcome.                                     #
#----------------------------------------------------------------------#
setMethod(f = ".predictAllTreatments", 
          signature = c(object = "TypedSimpleFit"), 
          definition = function(object, data, response){ 

                         txSet <- .getSuperSet(object@txInfo)
                         txName <- .getTxName(object@txInfo)

                         vals <- matrix(data = 0.0, 
                                        nrow = nrow(data), 
                                        ncol = length(txSet),
                                        dimnames = list(NULL,txSet))

                         for( i in 1L:length(txSet) ) {
                           data[,txName] <- txSet[i]
                           data[,txName] <- .convertTx(object@txInfo, data[,txName])
                           vals[,i] <- predict(object = as(object,"modelObjFit"), 
                                               newdata = data)
                         }

                         return(list(vals = vals, ss = txSet))
                       } )

#----------------------------------------------------------------------#
# Print key regression results                                         # 
#----------------------------------------------------------------------#
# input arguments                                                      #
# x : an object of class TypedSimpleFit                                #
#   returns                                                            #
# No object return.                                                    #
#----------------------------------------------------------------------#
setMethod(f = "print",
          signature = c(x = "TypedSimpleFit"),
          definition = function(x, ...){
                         cat("\n", x@type, "\n")
                         print(x = as(x,"modelObjFit"))
                        } )

#----------------------------------------------------------------------#
# Show key regression results                                          # 
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class TypedSimpleFit                           #
#   returns                                                            #
# No object return.                                                    #
#----------------------------------------------------------------------#
setMethod(f = "show", 
          signature = c(object = "TypedSimpleFit"), 
          definition = function(object){
                         cat("\n", object@type, "\n")
                         show(object = as(object,"modelObjFit"))
                       } )

#----------------------------------------------------------------------#
# Retrieve summary information of regression results                   # 
#----------------------------------------------------------------------#
# input arguments                                                      #
# object : an object of class TypedSimpleFit                           #
#   returns                                                            #
# a list; elements are defined in the inherited class modelObjFit.     #
#----------------------------------------------------------------------#
setMethod(f = "summary", 
          signature = c(object = "TypedSimpleFit"), 
          definition = function(object, ...){
                         res <- list()
                         res[[ object@type ]] <- summary(object = as(object,"modelObjFit"))
                         return( res )
                       } )
