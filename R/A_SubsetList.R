# October 23, 2018

.validity_SubsetList <- function(object) {

  # cannot create an empty SubsetList object.
  if (length(x = object) <= 0L) return( "cannot be empty" )

  # SubsetList must have named elements. The names are expected to be
  # the nicknames specified by user. If multiple subsets are
  # associated, they are collapsed into a single comma separated
  # string for the purposes of naming.
  nms <- object@names

  for (i in nms) {
    if (is.null(x = i) || {nchar(x = i) == 0L}) {
      return( "name for subset not provided" )
    }
  }

  # ensure no duplicate names
  uniqueNames <- unique(x = nms)
  if (length(x = uniqueNames) != length(x = nms)) {
    return( "at least two elements have the same name" )
  }

  # ensure no overlap of names
  tmpNames <- NULL
  for (i in 1L:length(x = nms)) {
    tmpNames <- c(tmpNames, unlist(x = strsplit(x = nms[i], split = ",")))
  }
  tstNames <- unique(x = tmpNames)
  if (length(x = tstNames) != length(x = tmpNames)) {
    return( "at least two elements contain the same subset" )
  }

  return( TRUE )
}

#' Class \code{SubsetList}
#'
#' Class \code{SubsetList} represents a \code{List} for subset specifications.
#' This class extends \code{List} to require non-zero length and named elements.
#'
#' @name SubsetList-class
#' @docType class
#'
#' @include A_List.R
#'
#' @keywords internal
setClass(Class = "SubsetList",
         contains = c("List"),
         prototype = prototype(list(NA), names = "NA"),
         validity = .validity_SubsetList)

#' Constructor method of SubsetList Class
#'
#' @rdname DynTxRegime-internal-api
setMethod(f = "initialize",
          signature = c(.Object = "SubsetList"),
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

#' Methods Available for Objects of Class \code{SubsetList}
#'
#' @name SubsetList-methods
#'
#' @keywords internal
NULL

#' @rdname DynTxRegime-internal-api
setMethod(f = "[[<-",
          signature = c(x = "SubsetList"),
          definition = function(x,i,value){ 

              nms <- names(x = x)
              lst <- as(object = x, Class = "list")
              names(x = lst) <- nms

              if (is.null(x = value)) {
                if (i %in% nms) {
                  lst <- lst[ !{nms %in% i} ]
                  return( new(Class = "SubsetList", lst) )
                }
              }

              if (is.numeric(x = i)) {
                if (i <= length(x = lst)) {
                  lst[[ i ]] <- value
                  return( new(Class = "SubsetList", lst) )
                }
              }

              lst[[ i ]] <- value
              if (!{i %in% nms}) names(x = lst) <- c(nms,i)
                
              return( new(Class = "SubsetList", lst) )
            })

#' \code{.cycleList()} 
#'   extends \code{List} method to include subset names
#'
#' @rdname cycleList
setMethod(f = ".cycleList",
          signature = c(object = "SubsetList"),
          definition = function(object, 
                                func,  
                                trm = "object",  
                                nm = "Subset=", ...) {
              return( .cycleList(object = as(object = object, Class = "List"),
                                 func = func,
                                 trm = trm,
                                 nm = paste0("Subset=", object@names), ...))
            })

# Extends plot() to generate plots of the regression analysis for each subset. 
# If suppress = FALSE, titles of plot will include the subset name.
#
#' @rdname SubsetList-methods 
#' @importFrom modelObj plot
setMethod(f = "plot",
          signature = c(x = "SubsetList"),
          definition = function(x, suppress = FALSE, ...) {
              for (i in 1L:length(x = x)) {
                argList <- list(...)
                if (!suppress) {
                  argList <- .titleIt(argList = argList, 
                                      nm = paste0("Subset=", names(x = x)[i]))
                }
                argList[[ "x" ]] <- x[[ i ]]
                do.call(what = plot, args = argList)
              }
            })

# Extends print() to add subset name information to print statements.
# Each subset is preceded by 'Subset=x' where x is the user specified subset
# name.
#
#' @rdname SubsetList-methods 
setMethod(f = "print",
          signature = c(x = "SubsetList"),
          definition = function(x, ...) {
              .cycleList(object = x, func = "print", trm = "x", ...)
            })

# Extends show() to add subset name information to show statements.
# Each subset is preceded by 'Subset=x' where x is the user specified subset
# name.
#
#' @rdname SubsetList-methods 
setMethod(f = "show",
          signature = c(object = "SubsetList"),
          definition = function(object) { 
              .cycleList(object = object, func = "show")
            })
