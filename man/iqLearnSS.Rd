\name{iqLearnSS}
\alias{iqLearnSS}

\title{IQ-Learning: Second-Stage Regression}

\description{
Estimate an optimal dynamic treatment regime using the 
Interactive Q-learning (IQ-learning) algorithm when the data has been 
collected from a two-stage randomized trial with binary treatments. 
iqLearnSS implements the second-stage regression step of the
IQ-Learning algorithm (IQ1).
}

\usage{ 
iqLearnSS(..., moMain, moCont, data, response, txName, iter = 0L, verbose = TRUE) }

\arguments{

 \item{\dots    }{ignored. Included to require named input.}

 \item{moMain   }{An object of class \code{"modelObj."}
                  This object specifies the main effects component
                  of the model for the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the response variable.
                  }

 \item{moCont   }{An object of class \code{"modelObj."}
                  This object specifies the contrasts component
                  of the model for the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the response variable.
                  }

 \item{data     }{An object of class \code{"data.frame."}
                  The covariates and treatment histories.}

  \item{response}{An object of class \code{"vector."}
                  The outcome of interest. }

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  as given in input \code{data}. The treatment variable must
                  be binary and will be recoded as -1/+1 if not provided
                  as such.}

  \item{iter    }{An object of class \code{"integer."}
                  If >0, the iterative method will be used to 
                  obtain parameter estimates in the outcome regression step.
                  See \link{iter} for further information.}

  \item{verbose }{An object of class \code{"logical."} 
                  If FALSE, screen prints will be suppressed.}

}

\value{
       Returns an object of class \code{"IQLearnSS"} that inherits directly from class \code{"DynTxRegime."}
}
\note{
The implementation of IQ-Learning utilizes
methods \code{optTx()} and \code{estimator()} in a 
different way than other methods
implemented in \pkg{DynTxRegime}. For the first-stage
component of the IQ-Learning method, all first-stage steps
must be complete before an optimal treatment or value can be
estimated. Therefore, \code{optTx()}
requires the objects
returned by first stage methods \code{iqLearnFSM()}, 
\code{iqLearnFSC()}, and
\code{iqLearnVar()} if using a log-linear model of the variance or
\code{iqLearnFSM()} and \code{iqLearnFS()} 
if using a constant model of the variance. 
Function \code{estimator()} requires the first-stage objects
and the second stage object returned by \code{iqLearnSS()}.
In addition, the density must be specified. See \code{\link{optTx}} and
\code{\link{estimator}} for further details.

In addition to the standard methods available to all objects
of class \code{"DynTxRegime"}, 
the following methods can be applied
to objects of class \code{"IQLearnSS"}

\code{fittedCont(object)} returns the estimated contrast component of the
second-stage regression. This is the 'outcome' regressed during 
the \code{iqLearnFSC()} step of the IQ-Learning algorithm.

\code{fittedMain(object)} returns the estimated main effects component of
the second-stage regression. This is the 'outcome' regressed during the
iqLearnFSM() step of the IQ-Learning algorithm.
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "IQLearnSS")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{DTRstep}{\code{signature(object = "IQLearnSS")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "IQLearnSS")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "IQLearnSS")}: 
       Retrieve value object returned by regression methods. }

    \item{fittedCont}{\code{signature(object = "IQLearnSS")}: 
       Retrieve estimated contrast component of outcome regression.}

    \item{fittedMain}{\code{signature(object = "IQLearnSS")}: 
       Retrieve estimated main effects component of outcome regression.}

    \item{optTx}{\code{signature(x = "IQLearnSS", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "IQLearnSS", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "IQLearnSS")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "IQLearnSS")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "IQLearnSS")}: 
       Print main results of analysis.}

    \item{show}{\code{signature(object = "IQLearnSS")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "IQLearnSS")}: 
       Retrieve summary information from regression analyses.}

  }
}


\references{
  Laber, E. B., Linn, K. A., and Stefanski, L.A. 
  (2014). 
  Interactive model building for Q-learning. 
  Biometrika, 101, 831--847.
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\seealso{
  \code{\link{iqLearnFSM}}, 
  \code{\link{iqLearnFSC}}, 
  \code{\link{iqLearnFSV}}
}

\examples{
# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

# Second-stage regression - Single Regression Analysis
  # Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')

  # Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                          solver.method = 'lm')

  iqSS <- iqLearnSS(moMain = moMain, moCont = moCont, 
                    data = bmiData, response = bmiData$y, txName = "A2", 
                    iter = 0L)

## Available methods for second stage step

  # Coefficients of the outcome regression
  coef(iqSS)

  # Description of method used to obtain object
  DTRstep(iqSS)

  # Value object returned by outcome regression method
  fitObject(iqSS)

  # Contrast component of outcome regression
  fittedCont(iqSS)

  # Main effects component of outcome regression
  fittedMain(iqSS)

  # Value object returned by outcome regression method
  outcome(iqSS)

  # Plots if defined by outcome regression method
  dev.new()
  par(mfrow = c(2,4))
  plot(iqSS)
  plot(iqSS, suppress = TRUE)

  # Show main results of method
  show(iqSS)

  # Show summary results of method
  summary(iqSS)

}
