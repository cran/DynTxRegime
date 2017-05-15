setClass("PropensityRegression",
         contains = c("VIRTUAL"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                       CLASS PropensityFit                        + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# Results of a propensity regression.                                  #
#----------------------------------------------------------------------#
#   small : logical indicating if the smallest valued treatment or     #
#           largest valued treatment is missing from predictions.      #
#   levs  : superset of treatment options for column headers           #
#   extends modelObjFit                                                #
#----------------------------------------------------------------------#
setClass("PropensityFit",
         slots = c(small = "logical",
                   levs  = "character"),
         contains = c("modelObjFit",
                      "PropensityRegression",
                      "SubsetsNotModeled", 
                      "SingleDecisionPoint"))

# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                      PropensityFit METHODS                       + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

setMethod(f = "plot", 
          signature = c(x = "PropensityFit"), 
          definition = function(x, suppress=FALSE, ...){

                         argList <- list(...)
                         argList[[ "x" ]] <- fitObject(x)

                         do.call(what = modelObj::plot, args = argList)

                       } )
#----------------------------------------------------------------------#
# Predict propensity for treatment                                     # 
#----------------------------------------------------------------------#
#   params                                                             #
# object : an object of class PropensityFit                            #
# newdata: if given, data.frame of covariates and treatment history    #
#   returns                                                            #
# matrix of propensity for each treatment                              #
#----------------------------------------------------------------------#
setMethod(f = "predict", 
          signature = c(object = "PropensityFit"), 
          definition = function(object, newdata, ...){ 

                         if( missing(newdata) ) {
                           mm <- predict(object = as(object, "modelObjFit"))
                         } else {
                           mm <- predict(object = as(object, "modelObjFit"), 
                                         newdata = newdata)
                         }

                         if( any(mm < -1.5e-8) ) {
                           UserError("input", 
                                     "cannot have negative probabilities")
                         }

                         if( any(mm > {1.0 + 1.5e-8}) ) {
                           UserError("input", 
                                     "cannot have probabilities > 1")
                         }

                         #-------------------------------------------#
                         # If not all treatments are represented,    #
                         # add missing propensity                    #
                         #-------------------------------------------#
                         if( ncol(mm) != length(object@levs) ) {

                           correction <- 1.0 - rowSums(mm)

                           if( object@small ) {
                             mm <- cbind(correction, mm)
                           } else {
                             mm <- cbind(mm, correction)
                           }

                         }
                         colnames(mm) <- object@levs

                         return( mm ) 

                       } )
