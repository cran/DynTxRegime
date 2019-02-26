#----------------------------------------------------------------------#
# Add titles to plots.                                                 #
#----------------------------------------------------------------------#
.titleIt <- function(argList, nm) {

  if (is.null(x = argList[[ "main" ]])) {
    argList[[ "main" ]] <- nm
  } else {
    argList[[ "main" ]] <- paste(argList[[ "main" ]], 
                                " (",nm,")", sep="")
  }

  return(argList)

}
