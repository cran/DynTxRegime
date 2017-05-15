# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -              CLASS SubsetListFit_DecisionPointList               + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# A list of objects of class SubsetListFit                             #
#----------------------------------------------------------------------#
.checkValidity_SubsetListFit_DecisionPointList <- function(object){

  errors <- character()

  if( is(errors, "logical") ) errors <- character()

  for( i in 1L:length(object) ) {
    if( !is(object[[i]], "SubsetListFit") ) {
      msg <- paste("all elements of",
                   "SubsetListFit_DecisionPointList",
                   "must be SubsetListFit")
      errors <- c(errors, msg)
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "SubsetListFit_DecisionPointList",
         contains = c("DecisionPointList", 
                      "OutcomeRegression",
                      "SubsetsModeled"),
         validity = .checkValidity_SubsetListFit_DecisionPointList)

