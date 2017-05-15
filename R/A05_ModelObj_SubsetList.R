# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                    ModelObj_SubsetList CLASS                     + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #

#----------------------------------------------------------------------#
# modelObjs defined for subsets of data at a single decision point     #
#----------------------------------------------------------------------#
# extends SubsetList directly
# extends List and SingleDecisionPoint 2nd
#
# slots:
#
#  loo : an object of class "list" whose elements must be modelObj
#
#----------------------------------------------------------------------#
.checkValidity_ModelObj_SubsetList <- function(object){

  errors <- character()

  #------------------------------------------------------------------#
  # Each element must be an object of class modelObj                 #
  #------------------------------------------------------------------#
  for( i in 1L:length(object) ) {
    if( !is(object[[i]], "modelObj") ) {
      msg <- "all elements of ModelObj_SubsetList must be modelObj"
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

setClass(Class = "ModelObj_SubsetList",
         contains = c("SubsetList"),
         validity = .checkValidity_ModelObj_SubsetList)
