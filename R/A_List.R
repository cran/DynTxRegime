# November 5, 2018

#' Class \code{List}
#'
#' Class \code{List} mimics a \code{list}.
#'
#' @slot names Character vector of names for elements
#'
#' @name List-class
#' @include A_generics.R
#' @keywords internal
setClass(Class = "List",
         slots = c(names = "ANY"),
         contains = c("list"),
         prototype = prototype(list(), names = NULL))

#' @rdname DynTxRegime-internal-api
setMethod(f = "initialize",
          signature = c(.Object = "List"),
          definition = function(.Object, ...) {
              lst <- list(...)
              if (length(x = lst) == 0L) return( .Object )
              if (length(x = lst) == 1L && is.list(x = lst[[ 1L ]])) {
                lst <- lst[[ 1L ]]
              }
              if (is.null(x = attr(x = lst, which = "names"))) {
                names(x = lst) <- 1L:length(x = lst)
              }
              as(object = .Object, Class = "list") <- lst
              .Object@names <- attr(x = lst, which = "names")
              return( .Object )
            })

#' @rdname DynTxRegime-internal-api
setMethod(f = "[[<-",
          signature = c(x = "List"),
          definition = function(x,i,value){ 

              nms <- names(x = x)
              lst <- as(object = x, Class = "list")
              names(x = lst) <- nms

              if (is.null(x = value)) {
                if (i %in% nms) {
                  lst <- lst[ !{nms %in% i} ]
                  return( new(Class = "List", lst) )
                }
              }

              if (is.numeric(x = i)) {
                if (i <= length(x = lst)) {
                  lst[[ i ]] <- value
                  return( new(Class = "List", lst) )
                }
              }

              lst[[ i ]] <- value
              if (!{i %in% nms}) names(x = lst) <- c(nms,i)
                
              return( new(Class = "List", lst) )
            })

##########
# GENERICS
##########

#' apply() for \code{List} objects
#'
#' Applies the specified function to each element of the \code{List}.
#'
#' @name cycleList
#'
#' @keywords internal
setGeneric(name = ".cycleList",
           def = function(object, ...) { standardGeneric(".cycleList") })

##########
# METHODS
##########

#' @rdname cycleList
#'
#' @param object The object inheriting from list to which func is applied.
#' @param func A character. The name of the function to be called for
#'   each element of object.
#' @param trm A character. The formal input argument name through
#'   which each element of object is passed to func.
#' @param nm A character. The naming convention for element of the
#'   returned list or displayed in print/show calls.
#' @param ...  Additional arguments to be passed to func.
#'
#' @return If func returns a value object, a list containing the
#'   value objects returned by func.
setMethod(f = ".cycleList",
          signature = c(object = "List"),
          definition = function(object, 
                                func,  
                                trm = 'object',  
                                nm = NULL, ...) {

              # if names are not specified attempt to retrieve names 
              # from object
              if (is.null(x = nm)) nm <- names(x = object)

              # if object is unnamed, use element numbers
              if (is.null(x = nm)) nm <- 1L:length(x = object)

              if (is.numeric(x = nm)) nm <- as.character(x = nm)

              # ensure that length of object matches number of names
              if ({any(sapply(X = nm, FUN = nchar) == 0L)} || 
                  {length(x = object) != length(x = nm)}) {
                stop("number of names does not match length of list")
              }

              # retrieve additional arguments to be passed to func
              argList <- list(...)

              # initialize storage list of results
              res <- list()

              # for each element of object, call func w/ argList
              for (i in 1L:length(x = object)) {

                # add the object stored in the ith element to the 
                # argument list
                argList[[ trm ]] <- object[[ i ]]

                # call the function
                if (func %in% c("show", "print")) {
                  cat("$", nm[i], "\n", sep = "")
                  do.call(what = func, args = argList)
                } else {
                  res[[ nm[i] ]] <- do.call(what = func, args = argList)
                }

              }

              if (length(x = res) == 0L) return()

              return( res )

            })
