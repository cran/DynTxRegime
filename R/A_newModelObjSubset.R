# September 25, 2018

#' Create Internal Model Objects for Subsets of Data
#'
#' @param object A list of modelObj or ModelObjSubset
#'
#' @return An object of class \code{ModelObj_SubsetList} if a single decision 
#'   point or an object of class \code{ModelObj_DecisionPointList} if multiple 
#'   decision points.
#'
#' @name newModelObjSubset
#'
#' @keywords internal
#'
.newModelObjSubset <- function(object) {

  if (!is.list(x = object) ) stop("inappropriate input")

  # assume more than 1 decision point
  dptList <- list()

  # determine the number of objects in list
  numModels <- length(x = object)

  # ensure object is not empty
  if (numModels <= 0L ) stop("object is empty")

  dp <- 1L
  cnt <- 0L
  dpcnt <- 0L

  msg <- NULL

  while (cnt < numModels) {

    ssList <- list()

    # determine which models correspond to the current dp.
    for (i in 1L:length(x = object)) {
      
      if (!is(object = object[[ i ]], class2 = "ModelObjSubset")) {
        stop("all elements must be of class ModelObjSubset")
      }

      if (.getDecisionPoint(object = object[[ i ]]) == dp) {
        nm <- paste(.getSubset(object = object[[ i ]]), collapse = ",")
        ssList[[ nm ]] <- as(object = object[[ i ]], Class = "modelObj")
        cnt <- cnt + 1L
      }

    }

    if (length(x = ssList) == 0L) {
      msg <- c(msg, paste("no subsets found for decision point", dp))
    } else {
      dptList[[ dp ]] <- new(Class = "ModelObj_SubsetList", ssList)
      dpcnt <- dpcnt + 1L
    }

    dp <- dp + 1L
  }

  if (dpcnt > 1L) {
    testNULL <- sapply(X = dptList, FUN = is.null)
    if (any(testNULL)) {
      stop(paste("models for decision point", which(testNULL), "missing"),
           call. = FALSE)
    }
    obj <- new(Class = "ModelObj_DecisionPointList", dptList)
    if (!is.null(msg)) warning(msg, call. = FALSE)
  } else {
    obj <- dptList[[ length(x = dptList) ]]
  }

  return( obj )

}
