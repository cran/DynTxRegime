\name{optimalClass}
\alias{optimalClass}
\title{
Classification Based Estimation of Optimal Dynamic Treatment Regimes
}
\description{
Uses an Augmented Inverse Propensity Weighted
Estimator (AIPWE) or an Inverse Propensity Weighted Estimator (IPWE) 
of the contrast function to define a weighted classification
problem. The method is limited to single decision point binary treatment regimes. 
}
\usage{
optimalClass(..., moPropen, moMain, moCont, moClass, 
             data, response, txName, iter = 0L, verbose = TRUE)
}
\arguments{

 \item{\dots    }{ignored. Included to require named input.}

 \item{moPropen }{An object of class \code{"modelObj."}
                  This object specifies the model of the
                  propensity score regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the probability,
                  i.e., returned values must be in the interval (0,1).
                  See \link{moPropen} for further information.}

 \item{moMain   }{An object of class \code{"modelObj."}
                  This object specifies the model of the
                  main effects component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{moCont   }{An object of class \code{"modelObj."}
                  This object specifies the models of the
                  contrasts component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{moClass  }{An object of class \code{"modelObj."}
                  This object defines the classification model
                  (covariates to be used) and the R methods to be
                  used to obtain parameter estimates and 
                  predictions.
                  The method chosen to obtain predictions must return
                  the predicted category.}

 \item{data     }{An object of class \code{"data.frame."}
                  The covariates and treatment histories.}

  \item{response}{An object of class \code{"vector."}
                  A vector of the outcome of interest.}

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  as given in input \code{data}. Treatment must be binary
                  and will be recoded as 0/1 if not provided as such.}

  \item{iter    }{An object of class \code{"numeric."}
                  If >0, the iterative method will be used to 
                  obtain parameter estimates in the outcome regression step.
                  See \link{iter} for further information.}


  \item{verbose }{An object of class \code{"logical."} 
                  If FALSE, screen prints will be suppressed.}
}

\details{
The user can opt for the IPWE estimator by specifying \code{moMain}
 and \code{moCont} 
as NULL or not providing them in the named input.

The classification method must accept "weights" as a formal 
argument.
}

\value{
       Returns an object of class \code{"OptimalClass"} that inherits directly from class \code{"DynTxRegime."} 
}

\section{Methods}{
  \describe{
    \item{classif}{\code{signature(object = "OptimalClass")}: 
       Retrieve value object from classification step. NA for
       all objects returned by \pkg{DynTxRegime} statistical methods 
       except for those returned by optimalClass(). }

    \item{coef}{\code{signature(object = "OptimalClass")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{DTRstep}{\code{signature(object = "OptimalClass")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "OptimalClass")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "OptimalClass")}: 
       Retrieve value object returned by regression methods. }

    \item{optTx}{\code{signature(x = "OptimalClass", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "OptimalClass", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "OptimalClass")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "OptimalClass")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "OptimalClass")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "OptimalClass")}: 
       Retrieve value object returned by propensity score regression methods.}

    \item{show}{\code{signature(object = "OptimalClass")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "OptimalClass")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\references{
Zhang, B., Tsiatis, A. A., Davidian, M., Zhang, M., and Laber, E. B.
(2012).
Estimating Optimal Treatment Regimes from a Classification Perspective.
Stat, 1, 103--114
}


\examples{

library(rpart)

# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

  # Define the propensity for treatment model and methods.
  moPropen <- buildModelObj(model =  ~ 1, 
                            solver.method = 'glm', 
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response'))

  # Define the classification model.
  moClass <- buildModelObj(model = ~parentBMI + month4BMI,
                           solver.method = 'rpart',
                           solver.args = list(method="class"),
                           predict.args = list(type='class'))

# IPWE estimator
  estIPWE <- optimalClass(moPropen = moPropen, moClass = moClass,
                          data = bmiData, response = bmiData$y, txName = "A2",
                          iter = 0L)

  # Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')

  # Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                          solver.method = 'lm')

# AIPWE estimator
  estAIPWE <- optimalClass(moPropen = moPropen, moMain = moMain, 
                           moCont = moCont, moClass = moClass,
                           data = bmiData, response = bmiData$y, txName = "A2",
                           iter = 0L)

## Available methods

  # Value object returned by classification method
  classif(estAIPWE)

  # Coefficients of the propensity score and outcome regression
  coef(estAIPWE)

  # Description of method used to obtain object
  DTRstep(estAIPWE)

  # Estimated value of estimated optimal treatment for training data
  estimator(estAIPWE)

  # Value object returned by outcome regression method
  fitObject(estAIPWE)

  # Estimated optimal treatment for training data
  optTx(estAIPWE)

  # Estimated optimal treatment for new data
  optTx(estAIPWE, newdata = bmiData)

  # Value object returned by outcome regression method
  outcome(estAIPWE)

  # Plots if defined by outcome regression method
  dev.new()
  par(mfrow = c(2,4))
  plot(estAIPWE)

  dev.new()
  par(mfrow = c(2,4))
  plot(estAIPWE, suppress = TRUE)

  # Value object returned by propensity score regression method
  propen(estAIPWE)

  # Show main results of method
  show(estAIPWE)

  # Show summary results of method
  summary(estAIPWE)

}

