# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -         ModelObj_SubsetList_DecisionPointList GENERICS           + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
setGeneric(name = ".newModelObjSubset",  
           def = function(object){
                   standardGeneric(".newModelObjSubset")
                 } )


#----------------------------------------------------------------------#
# Create an object of class ModelObj_SubsetList or                     #
# ModelObj_SubsetList_DecisionPointList.                               #
#----------------------------------------------------------------------#
# input arguments                                                      #
#   object : A list of objects of class ModelObjSubset.                #
# returns                                                              #
#   An object of class ModelObj_SubsetList if a single decision point  #
#   or an object of class ModelObj_SubsetList_DecisionPointList        #
#   if multiple decision points.                                       #
#----------------------------------------------------------------------#
.modelObjSubset <- function(object) {

  #------------------------------------------------------------------#
  # Assume more than 1 decision point                                #
  #------------------------------------------------------------------#
  dptList <- list()

  #------------------------------------------------------------------#
  # Determine the number of objects in length                        #
  #------------------------------------------------------------------#
  numModels <- length(object)

  #------------------------------------------------------------------#
  # Ensure object is not empty                                       #
  #------------------------------------------------------------------#
  if( numModels <= 0L ) stop("object is empty")

  dp <- 1L
  cnt <- 0L

  while( cnt < numModels ) {

    ssList <- list()

    #--------------------------------------------------------------#
    # Determine which models correspond to the current dp.         #
    #--------------------------------------------------------------#
    for( i in 1L:length(object) ) {

       if( !is(object[[i]], "ModelObjSubset") ) {
         UserError("input",
                   "all elements must be of class ModelObjSubset")
       }

       if( .getDecisionPoint(object[[i]]) == dp ) {
         nm <- paste(.getSubset(object[[i]]), collapse = ",")
         ssList[[nm]] <- .getModelObj(object[[i]])
         cnt <- cnt + 1L
       }
 
    }

    if( length(ssList) == 0L ) {
     stop(paste("no subsets found for decision pt",dp))
    }

    #--------------------------------------------------------------#
    # Convert list to ModelObj_SubsetList                          #
    #--------------------------------------------------------------#
    dptList[[dp]] <- new("ModelObj_SubsetList",
                         loo = ssList)

    dp <- dp + 1L
  }

  if( length(dptList) > 1L ) {
    #--------------------------------------------------------------#
    # If more than one decision point, convert to                  #
    # ModelObj_SubsetList_DecisionPointList                        #
    #--------------------------------------------------------------#
    obj <- new("ModelObj_SubsetList_DecisionPointList",
               "loo" = dptList)
  } else {
    #--------------------------------------------------------------#
    # If only one decision point, drop outermost list to return    #
    # ModelObj_SubsetList object                                   #
    #--------------------------------------------------------------#
    obj <- dptList[[1L]]
  }

  return(obj)

}

setMethod(f = ".newModelObjSubset",
          signature = c(object = "list"), 
          definition = .modelObjSubset )
