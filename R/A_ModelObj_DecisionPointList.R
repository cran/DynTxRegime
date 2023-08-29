# September 26, 2018
#
# Extends DecisionPointList to indicate that contents are modeling objects
#
# Defines methods
#  plot(x = ModelObj_DecisionPointList, ...)
# inherits methods
#  .cycleList(object = DecisionPointList, ...)
#  print(x = DecisionPointList, ...)
#  show(object = DecisionPointList)

.validity_ModelObj_DecisionPointList <- function(object){

  # each element must be an object of class modelObj or ModelObj_SubsetList
  for (i in 1L:length(x = object)) {
    if (!is(object = object[[ i ]], class2 = "modelObj") && 
        !is.null(x = object[[ i ]]) &&
        !is(object = object[[ i ]], class2 = "ModelObj_SubsetList")) {
      return( "elements are not of appropriate class" )
    }
  }

  return( TRUE )
}

#' Class ModelObj_DecisionPointList
#'
#' Class \code{ModelObj_DecisionPointList} represents a \code{List} for 
#' multiple decision points. Contents can be other modelObj or ModeObj_SubsetList.
#'
#' @name ModelObj_DecisionPointList-class
#' @rdname ModelObj_DecisionPointList-class
#'
#' @include A_DecisionPointList.R A_ModelObj_SubsetList.R
#'
#' @keywords internal
setClass(Class = "ModelObj_DecisionPointList",
         contains = c("DecisionPointList"),
         prototype = prototype(list(), names="NA"),
         validity = .validity_ModelObj_DecisionPointList)

#' Constructor Method of ModelObj_DecisionPointList Class
#'
#' @rdname ModelObj_DecisionPointList-class
#'
setMethod(f = "initialize",
          signature = c(.Object = "ModelObj_DecisionPointList"),
          definition = function(.Object, ...){
              as(object = .Object, Class = "DecisionPointList") <- new(Class = "DecisionPointList", as.list(...))
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
          signature = c(x = "ModelObj_DecisionPointList"),
          definition = function(x, ...) { stop("not allowed") })
