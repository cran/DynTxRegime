# September 28, 2022

#' Class \code{IQLearnFS_VHet}
#'
#' Class \code{IQLearnFS_VHet} contains the results for the first stage
#'  residuals component of the interactive Q-Learning algorithm.
#'  Objects of this class are returned by iqLearnFSV().
#'
#' @name IQLearnFS_VHet-class
#'
#' @slot residuals : Standardized residuals of contrast after modeling
#' @slot scale : Scaling factor for stdization
#' @slot step : Not used in this context.
#' @slot outcome : The outcome regression analysis
#' @slot txInfo : The feasible tx information
#' @slot optimal : The estimated optimal tx, decision function, and value
#'
#' @template outcomeOnly
#' @template regression
#' @template DynTxRegime_methods
#' @section Methods For Accessing Main Results:
#' \describe{
#'    \item{residuals}{:Retrieve the residuals of the regression.}
#'    \item{qqplot}{QQ plot of the residuals for the interactive Q-Learning algorithm.}
#'}
#'
#' @include E_class_IQLearnSS.R E_class_IQLearnFS.R
setClass(Class = "IQLearnFS_VHet", 
         slots = c(    scale = "numeric",
                   residuals = "numeric"),
         contains = c("IQLearnFS"))

#' Complete First Stage Analysis of Residuals for Interactive Q-Learning Algorithm
#'
#' Performs log-linear regression on the residuals.
#'
#' @rdname newIQLearnFS_VHet
#'
#' @keywords internal
setGeneric(name = ".newIQLearnFS_VHet", 
           def = function(object, moMain, moCont, ...) {
               standardGeneric(f = ".newIQLearnFS_VHet")
             })

##########
## METHODS
##########
#' Methods Available for Objects of Class \code{IQLearnFS_VHet}
#'
#' @name IQLearnFS_VHet-methods
#'
#' @keywords internal
NULL

# @param object An object of class IQLearnFS_C
# @param moMain An object of class modelObj or NULL
# @param moCont An object of class modelObj or NULL
#    note that at least 1 of moMain and moCont must be defined
# @param data A data.frame of covariates
# @param iter The maximum number of iterations if iterative algorithm used
# @param suppress T/F indicating if prints to screen are to be executed
.iqLearnFS_VHet <- function(object, 
                            moMain,  
                            moCont,  
                            data,  
                            iter, 
                            suppress) {

  if (!suppress) {
    cat("IQ-Learning Algorithm\n")
    cat("Regression of residuals using log-linear modeling.\n")
  }

  resid <- residuals(object = object)
  response <- log(x = resid*resid)

  txName <- .getTxName(object = object@analysis@txInfo)

  qLearnObj <- .qLearn(moMain = moMain, 
                       moCont = moCont,  
                       data = data,  
                       response = response,
                       txName = txName,  
                       fSet = NULL,  
                       iter = iter,  
                       step = 1L,  
                       suppress = suppress)

  qLearnObj@analysis@optimal@optimalTx <- NA
  qLearnObj@analysis@optimal@estimatedValue <- NA

  fitted <- predict(object = as(object = qLearnObj@analysis,
                                Class = "OutcomeObj"), 
                    newdata = data)

  # standardize the fitted residuals
  sig <- exp(x = fitted / 2.0)
  stdResids <- as.vector(x = resid / sig)
  sdr <- sd(x = stdResids)

  # ln(E[r^2/|rHat|^2] - (E[r/|rHat|])^2)
  sd.stdResids <- 2.0 * log(x = sdr)
  sig <- sig*(sdr)

  # r/|rHat| 1/sqrt(E[r^2/|rHat|^2] - (E[r/|rHat|])^2)
  stdResids <- as.vector(x = resid) / sig

  return( new(Class = "IQLearnFS_VHet",
              "residuals" = drop(x = stdResids),
              "scale"     = sd.stdResids,
              qLearnObj) )

}

#' @rdname newIQLearnFS_VHet
setMethod(f = ".newIQLearnFS_VHet",
          signature = c(object = "IQLearnFS_C",
                        moMain = "modelObj",
                        moCont = "modelObj"), 
          definition = .iqLearnFS_VHet)

#' @rdname newIQLearnFS_VHet
setMethod(f = ".newIQLearnFS_VHet",
          signature = c(object = "IQLearnFS_C",
                        moMain = "modelObj",
                        moCont = "NULL"), 
          definition = .iqLearnFS_VHet)

#' @rdname newIQLearnFS_VHet
setMethod(f = ".newIQLearnFS_VHet",
          signature = c(object = "IQLearnFS_C",
                        moMain = "NULL",
                        moCont = "modelObj"), 
          definition = .iqLearnFS_VHet)

#' @rdname DynTxRegime-internal-api
setMethod(f = "DTRstep", 
          signature = c(object = "IQLearnFS_VHet"), 
          definition = function(object) { 
              cat("IQ-Learning: Variance log-linear model\n") 
            })

#' @rdname IQLearnFS_VHet-methods
setMethod(f = "print",    
          signature = c(x = "IQLearnFS_VHet"), 
          definition = function(x, ...) {
              DTRstep(object = x)
              callNextMethod()
            })


#' @noRd
#' @importFrom stats qqplot qqnorm qqline
#'
#' @exportMethod qqplot
qqplot.IQLearnFS_VHet <- function(x, y, plot.it = TRUE, 
                                  xlab = deparse1(substitute(x)), 
                                  ylab = deparse1(substitute(y)), ...,
                                  conf.level = NULL, 
                                  conf.args = list(exact = NULL, 
                                                   simulate.p.value = FALSE, 
                                                   B = 2000, col = NA, 
                                                   border = NULL)) {
              x <- residuals(x)
              qqnorm(x, ...)
              qqline(x, ...)
            }

#' @describeIn IQLearnFS_VHet-methods
#'
#' @exportMethod qqplot
setMethod(f = "qqplot",
          signature = c(x = "IQLearnFS_VHet", y = "ANY"),
          definition = qqplot.IQLearnFS_VHet)


#' @rdname residuals
setMethod(f = "residuals",
          signature = c(object="IQLearnFS_VHet"),
          definition = function(object, ...) { return( object@residuals ) })

#' @rdname IQLearnFS_VHet-methods
setMethod(f = "show",    
          signature = c(object = "IQLearnFS_VHet"), 
          definition = function(object) {
              DTRstep(object = object)
              callNextMethod()
            })
