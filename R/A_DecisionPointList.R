# October 23, 2018

.validity_DecisionPointList <- function(object) {

  # cannot create an empty DecitionPointList object.
  if (length(x = object) == 0L) {
    return( "a DecisionPointList cannot be empty" )
  }

  return( TRUE )
}

#' Class \code{DecisionPointList}
#'
#' Class \code{DecisionListList} represents a \code{List} for decision points.
#' This class extends \code{List} to require non-zero length.
#'
#' @name DecisionPointList-class
#'
#' @keywords internal
#'
#' @include A_List.R
setClass(Class = "DecisionPointList",
         contains = c("List"),
         prototype = prototype(list(), names = NULL),
         validity = .validity_DecisionPointList)

#' @rdname DecisionPointList-class
setMethod(f = "initialize",
          signature = c(.Object = "DecisionPointList"),
          definition = function(.Object, ...) {
              lst <- list(...)
              if (length(x = lst) == 0L) return( .Object )
              if (length(x = lst) == 1L && is.list(x = lst[[ 1L ]])) {
                lst <- lst[[1L]]
              }
              if (is.null(x = attr(x = lst, which = "names"))) {
                names(x = lst) <- 1L:length(x = lst)
              }
              .Object@names <- attr(x = lst, which = "names")
              as(object = .Object, Class = "List") <- new("List", lst)
              validObject(object = .Object)
              return( .Object )
            })

##########
# METHODS
##########

#' @rdname DynTxRegime-internal-api
setMethod(f = "[[<-",
          signature = c(x = "DecisionPointList"),
          definition = function(x,i,value){ 

              nms <- names(x = x)
              lst <- as(object = x, Class = "list")
              names(x = lst) <- nms

              if (is.null(x = value)) {
                if (i %in% nms) {
                  lst <- lst[ !{nms %in% i} ]
                  return( new(Class = "DecisionPointList", lst) )
                }
              }

              if (is.numeric(x = i)) {
                if (i <= length(x = lst)) {
                  lst[[ i ]] <- value
                  return( new(Class = "DecisionPointList", lst) )
                }
              }

              lst[[ i ]] <- value
              if (!{i %in% nms}) names(x = lst) <- c(nms,i)
                
              return( new(Class = "DecisionPointList", lst) )
            })

#' Methods Available for Objects of Class \code{DecisionPointList}
#'
#' @name DecisionPointList-methods
#'
#' @keywords internal
NULL

#' \code{.cycleList()} 
#'   extends \code{List} method to include decision point in name
#'
#' @rdname cycleList
setMethod(f = ".cycleList",
          signature = c(object = "DecisionPointList"),
          definition = function(object, func, trm = "object", nm = "dp=", ...) {
              return( .cycleList(object = as(object = object, Class = "List"),
                                 func = func,
                                 trm = trm,
                                 nm = paste0("dp=", 1L:length(x = object)), ...) )
            })

#' \code{plot(x,suppress)}
#'   generates plots of the regression analysis for each decision point.
#'   If suppress = FALSE, titles of plot will include the decision point
#'   identifier. 
#'
#' @rdname DecisionPointList-methods 
#' @importFrom modelObj plot
#' @exportMethod plot
setMethod(f = "plot",
          signature = c(x = "DecisionPointList"),
          definition = function(x, suppress = FALSE, ...) {
              for (i in 1L:length(x = x)) {
                argList <- list(...)
                if (!suppress) {
                  argList <- .titleIt(argList = argList, nm = paste0("dp=", i))
                }
                argList[[ "x" ]] <- x[[ i ]]
                do.call(what = plot, args = argList)
              }
            })

#' \code{print(x)}
#'   adds decision point information to print statements.
#'   Each decision point is preceded by 'dp=x' where x is the decision point
#'   number.
#'
#' @rdname DecisionPointList-methods 
#' @importFrom modelObj print
#' @exportMethod print
setMethod(f = "print",
          signature = c(x = "DecisionPointList"),
          definition = function(x, ...) {
              .cycleList(object = x, func = "print", trm = "x", ...)
            })

#' \code{show(object)}
#'   adds decision point information to show statements.
#'   Each decision point is preceded by 'dp=x' where x is the decision point
#'   number.
#'
#' @rdname DecisionPointList-methods
#' @importFrom modelObj show
#' @exportMethod show
setMethod(f = "show",
          signature = c(object = "DecisionPointList"),
          definition = function(object) {
              .cycleList(object = object, func = "show")
            })
