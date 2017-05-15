\name{iqLearnFSV}
\alias{iqLearnFSV}

\title{IQ-Learning: Variance of the Regression of the Estimated Second-Stage Contrast (IQ3)}

\description{
Estimates the variance function of the regression of
the estimated second-stage contrast by fitting a
log-linear model to the residuals.
}

\usage{ 
iqLearnFSV( ..., object, moMain, moCont, data = NULL, iter = 0, verbose = TRUE) }

\arguments{
 \item{\dots    }{ignored. Included to require named input.}

  \item{object  }{An object of class \code{"IQLearnFS_C."}
                  The value object returned from a previous call to 
                   \code{iqLearnFSC()}}

 \item{moMain   }{An object of class \code{"modelObj."}
                  This object specifies the model for the
                  main effects component for the regression of
                  the residual
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the response variable.
                  }

 \item{moCont   }{An object of class \code{"modelObj."}
                  This object specifies the model for the
                  contrast component for the regression of
                  the residual
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the response variable.
                  }


 \item{data     }{An object of class \code{"data.frame."} 
                  The covariates and treatment histories.}

  \item{iter    }{An object of class \code{"numeric."}
                 If >0, the iterative method will be used to 
                 obtain parameter estimates in the outcome regression step.
                  See \link{iter} for further information.}

  \item{verbose }{An object of class \code{"logical."} 
                   If FALSE, screen prints will be suppressed.}
}
\value{
       Returns an object of class \code{"IQLearnFS_VHet"} that inherits directly from class \code{"DynTxRegime."} 
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

In addition to the standard methods available to all objects of class
\code{DynTxRegime}, objects of class \code{"IQLearnFS_VHet"} have
qqPlot(x) available. The plot can be used to determine if the normal 
or empirical density estimator is appropriate. If the observations 
deviate from the
line, den='nonpar' should be used in the final IQ-Learning step.
}

\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "IQLearnFS_VHet")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{DTRstep}{\code{signature(object = "IQLearnFS_VHet")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "IQLearnFS_VHet")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "IQLearnFS_VHet")}: 
       Retrieve value object returned by regression methods. }

    \item{optTx}{\code{signature(x = "IQLearnFS_VHet", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "IQLearnFS_VHet", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "IQLearnFS_VHet")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "IQLearnFS_VHet")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "IQLearnFS_VHet")}: 
       Print main results of analysis.}

    \item{qqPlot}{\code{signature(x = "IQLearnFS_VHet")}: 
       Generate a qq-plot.}

    \item{residuals}{\code{signature(object = "IQLearnFS_VHet")}: 
       Retrieve standardized fitted residuals. }

    \item{show}{\code{signature(object = "IQLearnFS_VHet")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "IQLearnFS_VHet")}: 
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
  \code{\link{iqLearnFSM}}, \code{\link{iqLearnFSC}}, \code{\link{iqLearnSS}} 
}

\examples{
#### Full IQ-Learning Algorithm

## Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

## Second-stage regression (IQ1)
  # Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')

  # Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                          solver.method = 'lm')

  iqSS <- iqLearnSS(moMain = moMain, moCont = moCont, 
                    data = bmiData, response = bmiData$y, txName = "A2", 
                    iter = 0L)

## Model conditional mean of main effects function (IQ2)
  # Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                          solver.method = 'lm')

  # Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')

  iqFSM <- iqLearnFSM(moMain = moMain, moCont = moCont, 
                      data = bmiData, response = iqSS, txName = "A1", 
                      iter = 0L)

 
## Model conditional mean of contrast function (IQ3)
  # Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                          solver.method = 'lm')

  # Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method = 'lm')

  iqFSC <- iqLearnFSC(moMain = moMain, moCont = moCont, 
                      data = bmiData, response = iqSS, txName = "A1", 
                      iter = 0L)

## Estimated optimal treatment and value when variance assumed constant
  # optimal treatment
  ot <- optTx(iqFSM, y = iqFSC, dens = 'nonpar')

  # estimated value
  est <- estimator(x = iqSS, y = iqFSM, z = iqFSC, dens = 'nonpar')

## Log-Linear Variance Modeling (IQ4)

  # heteroskedastic variance
  # Create modelObj object for main effect component
  moMain <- buildModelObj(model = ~ gender + race + parentBMI + baselineBMI,
                          solver.method = 'lm')

  # Create modelObj object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + baselineBMI,
                          solver.method = 'lm')


  iqFSV <- iqLearnFSV(object = iqFSC,
                      moMain = moMain, moCont = moCont, 
                      data = bmiData, txName = "A1", 
                      iter = 0L)

## Estimated optimal treatment and value with log-linear variance modeling
  # optimal treatment
  ot <- optTx(iqFSM, y = iqFSC, z = iqFSV, dens = 'nonpar')

  # estimated value
  est <- estimator(x = iqSS, y = iqFSM, z = iqFSC, w = iqFSV, dens = 'nonpar')

## Available methods for variance step

  # Coefficients of the outcome regression
  coef(iqFSV)

  # Description of method used to obtain object
  DTRstep(iqFSV)

  # Value object returned by outcome regression method
  fitObject(iqFSV)

  # Value object returned by outcome regression method
  outcome(iqFSV)

  # Plots if defined by outcome regression method
  dev.new()
  par(mfrow = c(2,4))
  plot(iqFSV)
  plot(iqFSV, suppress = TRUE)

  # qq-plot
  dev.new()
  qqplot(iqFSV)

  # Show main results of method
  show(iqFSV)

  # Show summary results of method
  summary(iqFSV)

}
