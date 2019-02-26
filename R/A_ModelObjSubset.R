# October 23, 2018

.validity_ModelObjSubset <- function(object) {

  # decision point must be positive non-zero value.
  if (object@decisionPoint < 1L) {
    return( "decision point must be a positive value" )
  }

  # must provide a nickname for the subset.
  if (length(x = object@subset) == 0L) {
    return( "subset must have a unique name" )
  }

  if (nchar(x = object@subset) == 0L) {
    return( "subset cannot be empty string" )
  }

  return( TRUE )
}

#' Class \code{ModelObjSubset}
#'
#' Class \code{ModelObjSubset} stores a modelObj object with pertinent subset
#' information
#'
#' @name ModelObjSubset-class
#'
#' @slot decisionPoint integer indicating the decision point for modelObj
#' @slot subset character indicating the subset for modelObj
#'
#' import modelObj
#' @keywords internal
setClass(Class = "ModelObjSubset",
         slots = c(decisionPoint = "integer",
                   subset        = "character"),
         contains = c('modelObj'),
         validity = .validity_ModelObjSubset)

#' Create Model Objects for Subsets of Data
#'
#' Extends the buildModelObj() function of package \pkg{modelObj}. Here,
#'   the returned model object includes a specification of the 
#'   decision point and subset of the data to which the model is to 
#'   be applied.
#'
#' In some settings, an analyst may want to use different models for unique 
#'   subsets of the data. \code{buildModelObjSubset()} provides a mechanism for 
#'   users to define models for such subset. Specifically, models are specified 
#'   in connection with the decision point and subset to which they are to be 
#'   applied. 
#'
#' See ?modelObj for further details
#'
#' @param ... ignored. Included to require named input.
#' @param model An object of class \code{formula}. The symbolic description
#'   of the model to be fitted. If the regression method specified in
#'   solver.method accepts as input a \code{formula} object,
#'   model is passed to the solver.method function. If the regression method
#'   instead accepts a matrix of covariates as the model to fit, \code{model} is
#'   used to obtain the model matrix that is passed to the solver.method
#'   function.
#' @param solver.method An object of class \code{character}. The name of 
#'   the R function to be used to obtain parameter estimates, e.g., 'lm', 
#'   'glm', or 'rpart'. The specified function MUST have a 
#'   corresponding predict method, which can be the generic predict() function.
#' @param solver.args An object of class \code{list}. Additional arguments 
#'   to be sent to the function specified in solver.method. This 
#'   argument must be provided as a named list, where the name of each element 
#'   matches a formal argument of the function specified in 
#'   solver.method. For example, if a logistic regression using 
#'   'glm' is desired, 
#' \preformatted{
#' solver.method = "glm"
#' solver.args = list("family"=binomial)
#' }
#'   See Details section for further information.
#' @param predict.method An object of class \code{character}. The name of 
#'   the R function to be used to obtain  predictions, e.g., 'predict.lm', 
#'   'predict', or 'predict.glm'.  If no function is explicitly 
#'   given, the generic \code{predict()} is assumed. For many regression 
#'   methods, the generic \code{predict()} method is appropriate.
#' @param predict.args  An object of class \code{list}. Additional arguments 
#'   to be sent to the function specified in predict.method. This 
#'   argument must be provided as a named list, where the name of each 
#'   element matches a formal argument of the function specified in 
#'   predict.method. For example, if a logistic regression using 
#'   'glm' was used to fit the model and predictions on the scale of the 
#'   response are desired, 
#'\preformatted{
#'predict.method = "predict.glm"
#'predict.args = list("type"="response").
#'}
#'
#'  See Details section for further information.
#' @param dp An object of class \code{integer}. The decision point for which 
#'   this model and subset are defined.
#' @param subset An object of class \code{character}. A nickname for the 
#'   subset for which model and methods are to be used. This argument will 
#'   be used by the methods of \pkg{DynTxRegime} to "link" input arguments. 
#'   In the event that a model is to be fit using more than 1 subset, 
#'   collapse the subset names into a single character string separating 
#'   each with a comma. For example, if the model is to be fit using patients 
#'   in both subsets "a" and "b," the subset nickname should be "a,b" (no space).
#'
#' @return  An object of class \code{ModelObjSubset}, which contains a  
#'   complete description of the conditions under which a model  is to be  
#'   used and the R methods to be used to obtain parameter estimates and  
#'   predictions.
#'
#' @examples
#' # Consider a 2 decision point trial. At the 1st decision point, the subset of 
#' # treatment options available to each patient is always set "set1."
#' # At the 2nd decision point, some patients are eligible to receive
#' # treatment from set "set2a" and others from set "set2b." The outcome
#' # for these subsets will be modeled as ~ x1 + x2 and ~ x2 + x3, respectively.
#' #
#' # All parameter estimates are to be obtained used lm and predictions obtained using predict.
#' #
#' # The following illustrates how to build these model objects.
#' 
#'   model <- list()
#' 
#'   model[[1]] <- buildModelObjSubset(dp = 1, subset = "set1",
#'                                     model = ~ x1 + x2 + x3, solver.method = 'lm')
#' 
#'   model[[2]] <- buildModelObjSubset(dp = 2, subset = "set2a",
#'                                     model = ~ ~ x1 + x2, solver.method = 'lm')
#' 
#'   model[[3]] <- buildModelObjSubset(dp = 2, subset = "set2b",
#'                                     model = ~ x2 + x3, solver.method = 'lm')
#'
#' @export
#'
#' @importFrom modelObj buildModelObj

buildModelObjSubset <-  function(...,
                                 model,
                                 solver.method,
                                 solver.args = NULL,
                                 predict.method = NULL,
                                 predict.args = NULL,
                                 dp = 1L,
                                 subset = NA) {

 if (!is.character(x = subset)) stop("subset must a character")

 if (nchar(x = subset) == 0L) stop("subset must be defined")

 if (!is.numeric(x = dp)) stop("dp must be a numeric")

 dp <- as.integer(round(x = dp, digits = 0L))
 if (dp <= 0L) stop("dp must be positive")

 if (is.null(x = predict.method) ) predict.method <- 'predict'

 myobjTemp <- modelObj::buildModelObj(model = model,
                                      solver.method = solver.method,
                                      solver.args = solver.args,
                                      predict.method = predict.method,
                                      predict.args = predict.args)

 myobj <- new(Class = "ModelObjSubset",
              "decisionPoint" = dp,
              "subset"        = subset,
              myobjTemp)

 return(myobj)

}

##########
# GENERICS
##########


#' Retrieve the Decision Point to which modelObj Pertains
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getDecisionPoint",
           def = function(object) { standardGeneric(f = ".getDecisionPoint") })

#' Retrieve the Subset to which modelObj Pertains
#'
#' @rdname DynTxRegime-internal-api
setGeneric(name = ".getSubset",
           def = function(object) { standardGeneric(f = ".getSubset") })

##########
# METHODS
##########
#' Methods Available for Objects of Class \code{ModelObjSubset}
#'
#' @name ModelObjSubset-methods
#'
#' @keywords internal
NULL

#' \code{.getDecisionPoint(object)}
#'   retrieves the decision point to which object pertains
#'
#' @rdname ModelObjSubset-methods
setMethod(f = ".getDecisionPoint",
          signature = c(object = "ModelObjSubset"),
          definition = function(object) { return( object@decisionPoint ) })

#' \code{.getSubset(object)}
#'   retrieves the subset to which object pertains
#'
#' @rdname ModelObjSubset-methods
setMethod(f = ".getSubset",
          signature = c(object = "ModelObjSubset"),
          definition = function(object) { return( object@subset ) })
