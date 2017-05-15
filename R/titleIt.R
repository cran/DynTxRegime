#----------------------------------------------------------------------#
# Add titles to plots.                                                 #
#----------------------------------------------------------------------#
.titleIt <- function(argList, nm) {

  if( is(argList[[ "main" ]], "NULL") ) {
    argList[[ "main" ]] <- nm
  } else {
    argList[[ "main" ]] <- paste(argList[[ "main" ]], 
                                " (",nm,")", sep="")
  }

  return(argList)

}
