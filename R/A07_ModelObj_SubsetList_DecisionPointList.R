# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -           ModelObj_SubsetList_DecisionPointList CLASS            + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# a SubsetList of model objects for all decision points                #
#----------------------------------------------------------------------#
# extends DecisionPointList directly 
# extends MultipleDecisionPoint at 2nd
#
# slots:
#
#  loo : an object of class "list" whose elements must be 
#        ModelObj_SubsetList
#
#----------------------------------------------------------------------#
.checkValidity_ModelObj_SubsetList_DecisionPointList <- function(object){

  errors <- character()

  for( i in 1L:length(object) ) {
    if( !is(object[[i]], "ModelObj_SubsetList") ) {
      msg <- "all elements of ModelObj_SubsetList_DecisionPointList must be ModelObj_SubsetList"
      errors <- c(errors, msg)
      break
    }
  }

  if( length(errors) == 0L ) {
    return(TRUE)
  } else {
    return(errors)
  }
}

setClass(Class = "ModelObj_SubsetList_DecisionPointList",
         contains = c("DecisionPointList"),
         validity = .checkValidity_ModelObj_SubsetList_DecisionPointList)
