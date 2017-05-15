UserError <- function(reason, msg){
  if( tolower(reason) == 'input' ) {
    e <- simpleError(msg)
    stop(e)
  }
}

DeveloperError <- function(msg, place){

  cat("You have discovered a bug in DynTxRegime.\n")
  cat("Please forward the following information to sthollow@ncsu.edu.\n")
  cat(msg, " received from ", place, "\n")
  e <- simpleError("End Error Report")
  stop(e)

}

badMethod <- function(nm, ...){

  alist <- list(...)

  clist <- sapply(alist, class)

  msg <- paste("This method was called with inappropriate classes",
               paste(clist, collapse = ", "))

  DeveloperError(msg, nm)

}



