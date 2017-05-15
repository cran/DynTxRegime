# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                 CLASS ModelObj_DecisionPointList                 + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# model objects for multiple decision point methods                    #
#----------------------------------------------------------------------#
# extends DecisionPointList directly
# extends List and MultipleDecisionPoint 2nd
#
# slots:
#
#  loo : an object of class "list" whose elements must be modelObj
#
#----------------------------------------------------------------------#
.checkValidity_ModelObj_DecisionPointList <- function(object){

  errors <- character()

  #------------------------------------------------------------------#
  # Each element must be an object of class modelObj                 #
  #------------------------------------------------------------------#
  for( i in 1L:length(object) ) {
    if( !is(object[[i]], "modelObj") ) {
      msg <- "all elements of ModelObj_DecisionPointList must be modelObj"
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

setClass(Class = "ModelObj_DecisionPointList",
         contains = c("DecisionPointList"),
         validity = .checkValidity_ModelObj_DecisionPointList)
