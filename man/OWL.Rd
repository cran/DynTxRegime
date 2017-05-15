\name{owl}
\alias{owl}
\title{
Outcome Weighted Learning
}
\description{
Estimation of an optimal treatment regime using outcome weighted 
learning. The convex surrogate of the 0-1 loss function is
taken to be the hinge function. The method is for 
single-decision-point analysis with binary treatment options. 
}
\usage{ 
owl(..., moPropen, data, reward, txName, regime, lambdas = 2.0, cvFolds = 0L, 
    kernel = "linear", kparam = NULL, verbose = TRUE)
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

 \item{data     }{An object of class \code{"data.frame."}
                  The covariates and treatment histories.}

  \item{reward }{An object of class \code{"vector."}
                  A vector of the outcome of interest.}

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  as given in input \code{data}. Treatment must be
                  binary and will be recoded as -1/+1 if not provided
                  as such.}

  \item{regime }{An object of class \code{"formula."}
                 The formula defines the decision rule, i.e.,
                 the covariates to be included in the kernel.}

  \item{lambdas }{An object of class \code{"numeric."}
                  Tuning parameter to avoid overfitting.
                  If more than one is given and \code{cvFolds} > 0, 
                  cross-validation will be used to select
                  an optimal from among those provided.}

  \item{cvFolds}{An object of class \code{"integer."}
                 If cross-validation is to be used to find
                 an optimal lambda and/or kernel parameter, 
                 the number of folds to use in the CV.}

  \item{kernel  }{An object of class \code{"character."}
                  In conjunction with input \code{kparam}, 
                  specification of the kernel function to be used.
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
                  can be provided and cross-validation
                  will be used to determine the optimal of those provided.
                  Note that input \code{cvFolds} must be > 0.}

  \item{verbose }{An object of class \code{"logical."}. 
                  If FALSE, screen prints will be suppressed.}
}

\value{
       Returns an object of class \code{"OWL"} that inherits directly from class \code{"DynTxRegime."} 
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "OWL")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{cvInfo}{\code{signature(object = "OWL")}: 
       Retrieve cross-validation results. }

    \item{DTRstep}{\code{signature(object = "OWL")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "OWL")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "OWL")}: 
       Retrieve value object returned by regression methods. }

    \item{optimObj}{\code{signature(object = "OWL")}: 
       Retrieve value object returned by optimization routine.}

    \item{optTx}{\code{signature(x = "OWL", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "OWL", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{plot}{\code{signature(x = "OWL")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "OWL")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "OWL")}: 
       Retrieve value object returned by propensity score regression methods.}

    \item{regimeCoef}{\code{signature(object = "OWL")}: 
       Retrieve the estimated decision function parameter estimates.}

    \item{show}{\code{signature(object = "OWL")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "OWL")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\references{
Zhao, Y-Q., Zeng, D., Rush, A. J., and Kosrok, M. R.
(2012).
Estimating Individualized Treatment Rules Using Outcome Weighted Learning.
Journal of the American Statistical Association, 107, 1106--1118.
}


\examples{
# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]


# Constant propensity model
  moPropen <- buildModelObj(model = ~1,
                            solver.method = 'glm',
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response'))

  owlRes <- owl(moPropen = moPropen,
                data = bmiData, reward = bmiData$y,  txName = "A2", 
                regime = ~ parentBMI + month4BMI)

##Available methods

  # Coefficients of the propensity score regression
  coef(owlRes)

  # Description of method used to obtain object
  DTRstep(owlRes)

  # Estimated value of the optimal treatment regime for training set
  estimator(owlRes)

  # Value object returned by propensity score regression method
  fitObject(owlRes)

  # Summary of optimization routine
  optimObj(owlRes)

  # Estimated optimal treatment for training data
  optTx(owlRes)

  # Estimated optimal treatment for new data
  optTx(owlRes, bmiData)

  # Plots if defined by propensity regression method
  dev.new()
  par(mfrow = c(2,4))

  plot(owlRes)
  plot(owlRes, suppress = TRUE)

  # Value object returned by propensity score regression method
  propen(owlRes)

  # Parameter estimates for decision function
  regimeCoef(owlRes)

  # Show main results of method
  show(owlRes)

  # Show summary results of method
  summary(owlRes)
 
}

