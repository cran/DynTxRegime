# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -         CLASS PropensityFit_SubsetList_DecisionPointList         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# List of PropensityFit_SubsetList results.                            #
#----------------------------------------------------------------------#
# extends DecisionPointList                                            #
#----------------------------------------------------------------------#
.checkValidity_PropensityFit_SubsetList_DecisionPointList <- function(object){

  errors <- character()

  for( i in 1L:length(object) ) {

    if( !is(object[[i]], "PropensityFit_SubsetList") ) {
      msg <- paste("You are attempting to create a",
                   "PropensityFit_SubsetList_DecisionPointList with",
                   "an object that is not",
                   "PropensityFit_SubsetList",
                   class(object[[i]]))
      errors <- c(errors, msg)
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "PropensityFit_SubsetList_DecisionPointList",
         contains = c("DecisionPointList", 
                      "PropensityRegression",
                      "SubsetsModeled"),
         validity = .checkValidity_PropensityFit_SubsetList_DecisionPointList)
