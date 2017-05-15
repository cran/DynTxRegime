\name{rwl}
\alias{rwl}
\title{
Residual Weighted Learning to Estimate Optimal Treatment Regimes
}
\description{
Estimation of an optimal dynamic treatment regime using residual 
weighted learning (RWL). The method is limited to single-decision-point
scenarios with binary treatment options. 
}
\usage{ 
rwl(..., moPropen, moMain, data, reward, txName, regime,
    lambdas = 2.0, cvFolds = 0L, kernel = "linear",
    kparam = NULL, responseType = "continuous", 
    guess = NULL, verbose = TRUE)
}
\arguments{
 \item{\dots    }{ignored. Included to require named input.}

 \item{moPropen }{An object of class \code{"modelObj."}
                  This object specifies the model of the
                  propensity for treatment regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the probability,
                  i.e., returned values must be in the interval (0,1).
                  See \link{moPropen} for further information.}

 \item{moMain   }{A single object of class \code{"modelObj."}
                  This object specifies the model of the
                  main effects component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{data     }{An object of class \code{"data.frame."} 
                  The covariates and treatment histories.}

  \item{reward }{An object of class \code{"vector."}
                 A vector of the outcome of interest.}

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  as given in input data. Treatment must be binary
                  and will be recoded as -1/+1 if not provided as
                  such.}

  \item{regime }{An object of class \code{"formula."}
                 The formula defines the decision rule, i.e.,
                 the covariates to be included in the kernel.}

  \item{lambdas }{An object of class \code{"numeric."}
                  Tuning parameter to avoid overfitting.
                  If more than one is given and cvFolds > 0, 
                  cross-validation will be used to select
                  an optimal from among those provided.}

  \item{cvFolds}{An object of class \code{"integer."}
                 If cross-validation is to be used to find
                 an optimal lambda and/or kernel parameter, 
                 the number of folds to use in the CV.}

  \item{kernel  }{An object of class \code{"character."}
                  In conjunction with input \code{kparam}, 
                  this input specifies the kernel function to be used.
                  Must be one of \{'linear', 'poly', or 'radial'\}. 
                  If 'linear,' the linear kernel; \code{kparam}
                  is ignored. 
                  If 'poly,'
                  the polynomial kernel; \code{kparam} must be specified.
                  If 'radial,' the Gaussian radial basis
                  function kernel; \code{kparam} must be specified.}

  \item{kparam  }{An object of class \code{"numeric."}
                  If input \code{kernel} = 'linear', this input is ignored.
                  If input \code{kernel} = 'poly', this input is the order
                  of the polynomial.
                  If input \code{kernel} = 'radial', this input is sigma; i.e.,
                    \deqn{K(x,y) = exp(||x-y||^2 / (2*sigma^2)).}
                  For \code{kernel} = 'radial', 
                  a vector of kernel parameters
                  can be provided, and cross-validation
                  will be used to determine the optimal of those provided.
                  Note that input \code{cvFolds} must be > 0.}


  \item{responseType}{An object of class \code{"character."}
                      One of {continuous, binary, count} indicating
                      the type of response variable.}

  \item{guess }{An object of class \code{"numeric"} or NULL.
                Starting parameter values for optimization method.}

  \item{verbose }{An object of class \code{"logical."} 
                  If FALSE, screen prints will be suppressed.}
}


\value{
       Returns an object of class \code{"RWL"} that inherits directly from class \code{"DynTxRegime."} 
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "RWL")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{cvInfo}{\code{signature(object = "RWL")}: 
       Retrieve cross-validation results. }

    \item{DTRstep}{\code{signature(object = "RWL")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "RWL")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "RWL")}: 
       Retrieve value object returned by regression methods. }

    \item{optimObj}{\code{signature(object = "RWL")}: 
       Retrieve value object returned by optimization routine. }

    \item{optTx}{\code{signature(x = "RWL", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "RWL", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "RWL")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "RWL")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "RWL")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "RWL")}: 
       Retrieve value object returned by propensity score regression methods.}

    \item{regimeCoef}{\code{signature(object = "RWL")}: 
       Retrieve the estimated decision function parameter estimates.}

    \item{residuals}{\code{signature(object = "RWL")}: 
       Retrieve residuals of outcome regression.}

    \item{show}{\code{signature(object = "RWL")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "RWL")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
Shannon T. Holloway \email{sthollow@ncsu.edu}
}

\references{
Zhou, X., Mayer-Hamblett, N., Kham, U., and Kosorok, M. R.
(2016+).
Residual Weighted Learning for Estimating Individualized Treatment Rules.
Journal of the American Statistical Association, in press.
}


\examples{
# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData$month12BMI - bmiData$baselineBMI) /
                     bmiData$baselineBMI


# Constant propensity model
  moPropen <- buildModelObj(model = ~1,
                            solver.method = 'glm',
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response'))

# Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')
  \dontrun{
    rwlRes <- rwl(moPropen = moPropen, moMain = moMain,
                  data = bmiData, reward = bmiData$y, txName = "A2", 
                  regime = ~ parentBMI + month4BMI)

##Available methods

  # Coefficients of the propensity score regression
  coef(rwlRes)

  # Description of method used to obtain object
  DTRstep(rwlRes)

  # Estimated value of the optimal treatment regime for training set
  estimator(rwlRes)

  # Value object returned by propensity score regression method
  fitObject(rwlRes)

  # Summary of optimization routine
  optimObj(rwlRes)

  # Estimated optimal treatment for training data
  optTx(rwlRes)

  # Estimated optimal treatment for new data
  optTx(rwlRes, bmiData)

  # Value object returned by outcome regression method
  outcome(rwlRes)

  # Plots if defined by propensity regression method
  dev.new()
  par(mfrow = c(2,4))
  plot(rwlRes)

  dev.new()
  par(mfrow = c(2,4))
  plot(rwlRes, suppress = TRUE)

  # Value object returned by propensity score regression method
  propen(rwlRes)

  # Parameter estimates for decision function
  regimeCoef(rwlRes)

  # Residuals used on method
  residuals(rwlRes)

  # Show main results of method
  show(rwlRes)

  # Show summary results of method
  summary(rwlRes)
 
  }
}

