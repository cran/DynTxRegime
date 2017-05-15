\name{bowl}
\alias{bowl}

\title{
Backward Outcome Weighted Learning
}

\description{
A statistical learning method for estimating the optimal 
dynamic treatment regime (DTR) termed
backward outcome weighted learning (BOWL). This approach converts
individualized treatment selection into a sequential classification
problem. The method directly maximizes over all DTRs a nonparametric
estimator of the expected long-term outcome. This function implements
a single step of a BOWL analysis. A complete BOWL analysis 
requires repeated calls to this function.
Note that this method is limited to binary treatment regimes. 
}

\usage{
 bowl(..., moPropen, data, reward, txName, regime, BOWLObj = NULL, 
      lambdas = 2.0, cvFolds = 0L, kernel = "linear", kparam = NULL, 
      fSet = NULL, verbose = TRUE)
}

\arguments{
 \item{\dots    }{ignored. Included to require named input.}

 \item{moPropen }{An object of class \code{"modelObj"} or
                  an object of class \code{"list"}
                  containing objects of class \code{"modelObjSubset."}
                  This input specifies the model(s) for the
                  propensity score regression, Pr(A=a|X),
                  and the R methods to be used to obtain parameter
                  estimates and predictions. 
                  The method specified to obtain predictions must return
                  the prediction on the scale of the probability,
                  i.e., returned values must be in the interval (0,1).
                  See \link{moPropen} for additional information.}

 \item{data     }{An object of class \code{"data.frame."} 
                  The covariates and treatment history.}

  \item{reward  }{An object of class \code{"numeric."}
                  The reward for the decision point under analysis.}

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  in input \code{data}. Treatment must be binary
                  and will be recoded to -1/+1 if not provided as such.}

  \item{regime  }{An object of class \code{"formula"} or
                  an object of class \code{"list"}
                  containing objects of class \code{"formula."}
                  The formula defines the decision rule, i.e.,
                  the covariates to be included in the kernel.
                  If \code{regime} is a single \code{"formula,"}
                  all patients are used to obtain parameter
                  estimates. If \code{regime} is a list,
                  subsets of patients are used to fit each
                  decision rule.
                  When using subset models, 
                  input \code{fSet} must be defined, and the
                  name of each element of the list must correspond to
                  a subset defined by \code{fSet}.
                  See \link{fSet} for further details.}

  \item{BOWLObj }{An object of class \code{"BOWL"} or NULL.
                  For the first step of the BOWL algorithm
                  (the final decision point),
                  this input must be NULL. For all other
                  steps, it is the value object returned by
                  the previous call to bowl().}

  \item{lambdas }{An object of class \code{"numeric."}
                  This input is the tuning parameter to avoid over fitting.
                  If more than one value is given and input
                  \code{cvFolds} is greater
                  than zero, 
                  cross-validation will be used to select
                  an optimal value from among those provided.}

  \item{cvFolds }{An object of class \code{"integer."}
                  If cross-validation is to be used to find
                  an optimal lambda and/or kernel parameter, 
                  the number of folds to use in the 
                  cross-validation procedure.}

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

 \item{fSet     }{An object of class \code{"function"} or NULL.
                  If a function, \code{fSet}
                  defines (i) the conditions under 
                  which only a subset of treatment options is
                  available to a patient or (ii) 
                  the conditions under which patients are to
                  be subset and the subsets modeled uniquely.
                  The form of this input has been extended from the original
                  release of \pkg{DynTxRegime}.
                  See \link{fSet} for additional information.}

  \item{verbose }{An object of class \code{"logical."}
                  If FALSE, screen prints will be suppressed.}
}


\value{
  Returns a \code{"BOWL"} object, 
  which inherits directly from class \code{"DynTxRegime."} 
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "BOWL")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{cvInfo}{\code{signature(object = "BOWL")}: 
       Retrieve cross-validation results.}

    \item{DTRstep}{\code{signature(object = "BOWL")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "BOWL")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "BOWL")}: 
       Retrieve value object returned by regression methods. }

    \item{optimObj}{\code{signature(object = "BOWL")}: 
       Retrieve value object returned by optimization routine.}

    \item{optTx}{\code{signature(x = "BOWL", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "BOWL", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{plot}{\code{signature(x = "BOWL")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "BOWL")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "BOWL")}: 
       Retrieve value object returned by propensity score regression methods.}

    \item{regimeCoef}{\code{signature(object = "BOWL")}: 
       Retrieve the estimated decision function parameter estimates.}

    \item{show}{\code{signature(object = "BOWL")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "BOWL")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
Shannon T. Holloway \email{sthollow@ncsu.edu}
}

\references{
Zhao, Y-Q., Zeng, D., Laber, E. B., and Kosorok, M. R.
(2015).
New Statistical Learning Methods for Estimating Optimal Dynamic 
Treatment Regimes.
Journal of the American Statistical Association, 110, 583--598.
}


\examples{
# Load and process data set
data(bmiData)

# define the negative 12 month change in BMI from baseline
y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]

# define the negative 4 month change in BMI from baseline
y4 <- -100*(bmiData[,5L] - bmiData[,4L])/bmiData[,4L]

# reward for second stage
rewardSS <- y12 - y4

#### Second-stage regression

# Constant propensity model
moPropen <- buildModelObj(model = ~1,
                          solver.method = 'glm',
                          solver.args = list('family'='binomial'),
                          predict.method = 'predict.glm',
                          predict.args = list(type='response'))

fitSS <- bowl(moPropen = moPropen,
              data = bmiData, reward = rewardSS,  txName = 'A2', 
              regime = ~ parentBMI + month4BMI)

##Available methods

  # Coefficients of the propensity score regression
  coef(fitSS)

  # Description of method used to obtain object
  DTRstep(fitSS)

  # Estimated value of the optimal treatment regime for training set
  estimator(fitSS)

  # Value object returned by propensity score regression method
  fitObject(fitSS)

  # Summary of optimization routine
  optimObj(fitSS)

  # Estimated optimal treatment for training data
  optTx(fitSS)

  # Estimated optimal treatment for new data
  optTx(fitSS, bmiData)

  # Plots if defined by propensity regression method
  dev.new()
  par(mfrow = c(2,4))

  plot(fitSS)
  plot(fitSS, suppress = TRUE)

  # Value object returned by propensity score regression method
  propen(fitSS)

  # Parameter estimates for decision function
  regimeCoef(fitSS)

  # Show main results of method
  show(fitSS)

  # Show summary results of method
  summary(fitSS)
 
#### First-stage regression

# Constant propensity model
fitFS <- bowl(moPropen = moPropen,
              data = bmiData, reward = y4,  txName = 'A1', 
              regime = ~ gender + parentBMI,
              BOWLObj = fitSS)

}

