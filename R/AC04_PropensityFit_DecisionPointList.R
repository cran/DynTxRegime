# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -               CLASS PropensityFit_DecisionPointList              + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# propensity fits obtained using modelObjs for multiple dp             #
#----------------------------------------------------------------------#
# extends DecisionPointList                                            #
#----------------------------------------------------------------------#
.checkValidity_PropensityFit_DecisionPointList <- function(object){

  errors <- character()

  for( i in 1L:length(object) ) {

    if( !is(object[[i]], "PropensityRegression") ) {
      msg <- paste("You are attempting to create a",
                   "PropensityFit_DecisionPointList with",
                   "an object that is not PropensityRegression",
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

setClass(Class = "PropensityFit_DecisionPointList",
         contains = c("DecisionPointList", 
                      "PropensityRegression",
                      "SubsetsNotModeled"),
         validity = .checkValidity_PropensityFit_DecisionPointList)
