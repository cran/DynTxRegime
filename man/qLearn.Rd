\name{qLearn}
\alias{qLearn}

\title{ Q-learning}

\description{As a single call, regresses the response on
covariates and treatment histories to estimate the optimal 
treatment decision rule. If called repeatedly, once for each
treatment decision point, each call is a step of the Q-Learning
algorithm.
}

\usage{ qLearn(..., moMain, moCont, data, response, txName, fSet, iter = 0L, verbose = TRUE) }

\arguments{

 \item{\dots    }{ignored. Included to require named input.}

 \item{moMain   }{A single object of class \code{"modelObj"} or 
                  an object of class \code{"list"} 
                  containing objects of class \code{"modelObjSubset."}
                  This object specifies the model(s) of the
                  main effects component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{moCont   }{A single object of class \code{"modelObj"} or 
                  an object of class \code{"list"} 
                  containing objects of class \code{"modelObjSubset."}
                  This object specifies the models of the
                  contrasts component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method chosen to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{data     }{An object of class \code{"data.frame."}
                  The covariates and treatment histories.}

  \item{response}{An object of class \code{"vector"} or \code{"QLearn."}
                  If performing a single decision point analysis
                  or the first step of the Q-Learning algorithm,
                  i.e., the final-stage regression, response is a 
                  vector of the outcome of interest. 
                  For multiple decision point analysis, this
                  is the value object returned by the previous
                  call to \code{qLearn()}.}

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  as given in input \code{data}.}

 \item{fSet     }{An object of class \code{"function"} or NULL.
                  See \link{fSet} for further information.
                     }

  \item{iter    }{An object of class \code{"numeric."}
                  If >0, the iterative method will be used to 
                  obtain parameter estimates in the outcome regression step.
                  See \link{iter} for further information.}

  \item{verbose }{An object of class \code{"logical."}
                  If FALSE, screen prints will be suppressed.}
}

\value{
       Returns an object of class \code{"QLearn"} that inherits directly from class \code{"DynTxRegime."} 
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "QLearn")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{DTRstep}{\code{signature(object = "QLearn")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "QLearn")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "QLearn")}: 
       Retrieve value object returned by regression methods. }

    \item{optTx}{\code{signature(x = "QLearn", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "QLearn", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "QLearn")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "QLearn")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "QLearn")}: 
       Print main results of analysis.}

    \item{show}{\code{signature(object = "QLearn")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "QLearn")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

# Second-stage regression
  # Create modeling object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method='lm')

  # Create modeling object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                          solver.method='lm')

  fitQ2 <- qLearn(moMain = moMain, 
                  moCont = moCont, 
                  data = bmiData,  
                  response = bmiData$y, 
                  txName = "A2", 
                  iter = 0L)

##Available methods

  # Coefficients of the propensity score regression
  coef(fitQ2)

  # Description of method used to obtain object
  DTRstep(fitQ2)

  # Estimated value of the optimal treatment regime for training set
  estimator(fitQ2)

  # Value object returned by propensity score regression method
  fitObject(fitQ2)

  # Estimated optimal treatment for training data
  optTx(fitQ2)

  # Estimated optimal treatment for new data
  optTx(fitQ2, bmiData)

  # Value object returned by outcome regression method
  outcome(fitQ2)

  # Plots if defined by propensity regression method
  dev.new()
  par(mfrow = c(2,4))

  plot(fitQ2)
  plot(fitQ2, suppress = TRUE)

  # Show main results of method
  show(fitQ2)

  # Show summary results of method
  summary(fitQ2)
 

# First-stage regression

  # Create modeling object for main effect component
  moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                          solver.method='lm')

  # Create modeling object for contrast component
  moCont <- buildModelObj(model = ~ gender + parentBMI,
                          solver.method='lm')

  fitQ1 <- qLearn(moMain = moMain, 
                  moCont = moCont,  
                  response = fitQ2, 
                  data = bmiData,  
                  txName = "A1",  
                  iter = 100L)

}
