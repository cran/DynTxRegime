# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -                         CLASS TxInfoList                         + #
# +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+- #
# -+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+ #


#----------------------------------------------------------------------#
# A list of TxInfo objects. Used in multiple decision point methods.   #
#----------------------------------------------------------------------#
.checkValidity_TxInfoList <- function(object){

  if( length(object) <= 1L ) {
    msg <- "list is of length 0 or 1"
    return( msg )
  }

  for( i in 1L:length(object) ) {
    if( !is(object[[i]], "TxInfoBasic") ) {
      msg <- "all elements of TxInfoList must be TxInfoBasic"
      return( msg )
    }

  }

  return( TRUE )

}

setClass(Class = "TxInfoList",
         contains = c("List", "MultipleDecisionPoint"),
         validity = .checkValidity_TxInfoList)

