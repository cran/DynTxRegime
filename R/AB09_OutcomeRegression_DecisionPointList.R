# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -            CLASS OutcomeRegression_DecisionPointList             + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# A list of objects of class OutcomeRegression                         #
#----------------------------------------------------------------------#
.checkValidity_OutcomeRegression_DecisionPointList <- function(object){

  errors <- character()

  for( i in 1L:length(object) ) {

    if( !is(object[[i]], "OutcomeRegression") ) {
      msg <- paste("You are attempting to create an",
                   "OutcomeRegression_DecisionPointList with",
                   "an object that is not OutcomeRegression",
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

setClass(Class = "OutcomeRegression_DecisionPointList",
         contains = c("DecisionPointList", 
                      "OutcomeRegression",
                      "SubsetsNotModeled"),
         validity = .checkValidity_OutcomeRegression_DecisionPointList)

