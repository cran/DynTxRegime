.checkModelObjOrListModelObjSubset <- function(object, nm) {

  if (is.null(x = object)) return(object)

  if (is(object = object, class2 = "ModelObjSubset")) {
    object <- list(object)
  }

  if (is(object = object, class2 = "modelObj")) return(object)

  if (!is.list(x = object) || length(x = object) == 0L) {
    stop("single modelObj or a list of ModelObjSubset objects expected")
  }

  if (length(x = object) == 1L && 
      is(object = object[[ 1L ]], class2 = "modelObj") &&
      !is(object = object[[ 1L ]], class2 = "ModelObjSubset")) {
    return( object[[ 1L ]] )
  }

  for (i in 1L:length(x = object)) {
    if (!is(object = object[[ i ]], class2 = "ModelObjSubset")) {
      stop("single modelObj or a list of ModelObjSubset objects expected")
    }
  }

  # Convert list into recognized internal class.
  return( .newModelObjSubset(object = object) )

}

.checkTxData <- function(txName, data) {

  # txName must be an object of class character
  if (!is.character(x = txName)) stop("txName must be a character")

  # txName can only be a single name
  if (length(x = txName) != 1L) {
    stop("txName must be of length 1 for this method")
  }

  # test to see if tx is in data set provided.
  txVec <- tryCatch(expr = data[,txName], 
                    condition = function(x) {
                        cat(x$message, "\n")
                        stop(paste0(txName, " not found in data"))
                      })

  # if tx is not a factor or an integer, attempt to coerce to integer
  if (!is.factor(x = txVec) && !is.integer(x = txVec)) {
    if (is.character(x = txVec)) {
      data[,txName] <- factor(x = txVec)
    } else {
      if (!isTRUE(x = all.equal(target = txVec, current = round(x = txVec)))) {
        stop("treatment variable must be a factor or an integer")
      }
      data[,txName] <- as.integer(x = round(x = data[,txName]))
   }
  }

  return( data )
}

.checkBinaryTx <- function(txName, data) {

  txVec <- numeric(length = nrow(x = data))

  # identify the levels of treatment
  if (is.factor(x = data[,txName])) {
    levs <- levels(x = data[,txName])
  } else {
    levs <- unique(x = data[,txName])
    levs <- levs[!is.na(x = levs)]
    levs <- sort(x = levs)
  }

  # if more than 2 tx options throw error
  if (length(x = levs) > 2L ) {
    stop("only binary tx options can be used in this method")
  }

  # Create treatment vector cast as +/- 1 where -1 = base level
  txVec[data[,txName] == levs[1L]] <- -1.0
  txVec[data[,txName] == levs[2L]] <-  1.0

  return( txVec )
}

.checkModelObjOrListModelObjSubsetOrList <- function(object, nm) {

  # if object is null, return object unchanged
  if (is.null(x = object)) return(object)

  # if object is a single modelObjSubset object, convert to a list
  if (is(object = object, class2 = "ModelObjSubset")) object <- list(object)

  # if object is a single modelObj, return object unchanged
  if (is(object = object, class2 = "modelObj")) return(object)

  # if object is now not a list or has zero length, stop with error
  if (!is.list(x = object) || length(x = object) == 0L) {
    stop("single modelObj, a list of modelObj, or a list of ModelObjSubset objects expected")
  }

  # if only one object is in the list and it is a modelObj, return object as 
  # a modelObj
  if (length(x = object) == 1L && 
      is(object = object[[ 1L ]], class2 = "modelObj") &&
      !is(object = object[[ 1L ]], class2 = "ModelObjSubset")) {
    return( object[[ 1L ]] )
  }

  # ensure that all elements of the list are modelObj or that all elements
  # of the list are ModelObjSubset
  firstClass <- class(x = object[[ 1L ]])
  if (!is(object = object[[ 1L ]], class2 = "modelObj") && 
      !is(object = object[[ 1L ]], class2 = "ModelObjSubset")) {
    stop("single modelObj, a list of modelObj, or a list of ModelObjSubset objects expected")
  }
 
  for (i in 1L:length(x = object)) {
    if (!is(object = object[[ i ]], class2 = firstClass)) {
      stop("single modelObj, a list of modelObj, or a list of ModelObjSubset objects expected")
    }
  }

  # if the list contains only modelObj, return as a ModelObj_DecisionPointList
  if (!is(object = object[[ 1L ]], class2 = "ModelObjSubset")) {
    return( new("ModelObj_DecisionPointList", object) )
  }

  # Convert list into recognized internal class.
  return( .newModelObjSubset(object = object) )
}
