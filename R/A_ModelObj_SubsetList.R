# September 26, 2018
#
# Extends SubsetList to indicate that contents are modeling objects
#
# Defines methods
#  plot(x = ModelObj_SubsetList, ...)
# inherits methods
#  .cycleList(object = SubsetList, ...)
#  print(x = SubsetList, ...)
#  show(object = SubsetList)

.validity_ModelObj_SubsetList <- function(object) {

  # each element must be an object of class modelObj
  for (i in 1L:length(x = object)) {
    if (!is(object = object[[ i ]], class2 = "modelObj")) {
      return( "all elements of ModelObj_SubsetList must be modelObj" )
    }
  }

  return( TRUE )
}

#' Class ModelObj_SubsetList
#'
#' Class \code{ModelObj_SubsetList} represents a \code{List} for subset
#' modelObj.
#'
#' @name ModelObj_SubsetList-class
#' @rdname ModelObj_SubsetList-class
#'
#' @include A_SubsetList.R
#'
#' @keywords internal
setClass(Class = "ModelObj_SubsetList",
         contains = c("SubsetList"),
         prototype = prototype(list(NA), names = "NA"),
         validity = .validity_ModelObj_SubsetList)

#' Constructor Method of ModelObj_SubsetList Class
#'
#' @rdname ModelObj_SubsetList-class
#'
setMethod(f = "initialize",
          signature = c(.Object = "ModelObj_SubsetList"),
          definition = function(.Object, ...) {
              as(object = .Object, Class = "SubsetList") <- new(Class = "SubsetList", as.list(...))
              validObject(object = .Object)
              return( .Object )
            })

##########
# METHODS
##########

# extends plot() to prevent its use
#
#' @rdname DynTxRegime-internal-api 
setMethod(f = "plot",
          signature = c(x = "ModelObj_SubsetList"),
          definition = function(x, ...) { stop("not allowed") })
