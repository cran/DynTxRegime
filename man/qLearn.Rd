% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/E_qLearn.R
\name{qLearn}
\alias{qLearn}
\title{A Step of the Q-Learning Algorithm}
\usage{
qLearn(
  ...,
  moMain,
  moCont,
  data,
  response,
  txName,
  fSet = NULL,
  iter = 0L,
  verbose = TRUE
)
}
\arguments{
\item{...}{ignored. Provided to require named inputs.}

\item{moMain}{An object of class modelObj or a list of objects of class
modelObjSubset, which define the models and R methods to be used to
obtain parameter estimates and predictions for the main effects component 
of the outcome regression. \cr
See ?modelObj and/or ?modelObjSubset for details. \cr
NULL is an acceptable value if moCont is defined.}

\item{moCont}{An object of class modelObj or a list of objects of class
modelObjSubset, which define the models and R methods to be used to 
obtain parameter estimates and predictions for the contrasts component 
of the outcome regression. \cr
See ?modelObj and/or ?modelObjSubset for details. \cr
NULL is an acceptable value if moMain is defined.}

\item{data}{A data frame of covariates and treatment history.}

\item{response}{A response vector or object of class QLearn from a previous
Q-Learning step.}

\item{txName}{A character string giving column header of treatment variable
in data}

\item{fSet}{NULL or a function. This argument allows the user to specify 
the subset of treatment options available to a patient. 
See ?fSet for details of allowed structure}

\item{iter}{An integer. See ?iter for details}

\item{verbose}{A logical. If TRUE, screen prints are generated.}
}
\value{
An object of class \link{QLearn-class}
}
\description{
Performs a single step of the Q-Learning algorithm.
   If an object of class \code{QLearn} is passed through input response, 
   it is assumed that the \code{QLearn} object is the value object returned
   from the preceding step of the Q-Learning algorithm, and
   the value fit by the regression is taken from the \code{QLearn} object.
   If a vector is passed through input response, it is assumed that the
   call if for the first step in the Q-Learning algorithm, and 
   models are fit using the provided response.
}
\examples{

# Load and process data set
data(bmiData)

# define the negative 12 month change in BMI from baseline
y12 <- -100*(bmiData[,6L] - bmiData[,4L])/bmiData[,4L]

# outcome model
moMain <- buildModelObj(model = ~parentBMI+month4BMI,
                        solver.method = 'lm')

moCont <- buildModelObj(model = ~race + parentBMI+month4BMI,
                        solver.method = 'lm')

#### Second-Stage Analysis
fitSS <- qLearn(moMain = moMain, moCont = moCont,
                data = bmiData, response = y12,  txName = 'A2')

##Available methods

  # Coefficients of the outcome regression objects
  coef(fitSS)

  # Description of method used to obtain object
  DTRstep(fitSS)

  # Estimated value of the optimal treatment regime for training set
  estimator(fitSS)

  # Value object returned by outcome regression method
  fitObject(fitSS)

  # Estimated optimal treatment and decision functions for training data
  optTx(fitSS)

  # Estimated optimal treatment and decision functions for new data
  optTx(fitSS, bmiData)

  # Value object returned by outcome regression method
  outcome(fitSS)

  # Plots if defined by outcome regression method
  dev.new()
  par(mfrow = c(2,4))

  plot(fitSS)
  plot(fitSS, suppress = TRUE)

  # Show main results of method
  show(fitSS)

  # Show summary results of method
  summary(fitSS)
 
#### First-stage Analysis

# outcome model
moMain <- buildModelObj(model = ~parentBMI+baselineBMI,
                        solver.method = 'lm')

moCont <- buildModelObj(model = ~race + parentBMI+baselineBMI,
                        solver.method = 'lm')

fitFS <- qLearn(moMain = moMain, moCont = moCont,
                data = bmiData, response = fitSS,  txName = 'A1')

##Available methods for fitFS are as shown above for fitSS

}
\seealso{
Other statistical methods: 
\code{\link{bowl}()},
\code{\link{earl}()},
\code{\link{iqLearn}},
\code{\link{optimalClass}()},
\code{\link{optimalSeq}()},
\code{\link{owl}()},
\code{\link{rwl}()}

Other multiple decision point methods: 
\code{\link{bowl}()},
\code{\link{iqLearn}},
\code{\link{optimalClass}()},
\code{\link{optimalSeq}()}

Other single decision point methods: 
\code{\link{earl}()},
\code{\link{optimalClass}()},
\code{\link{optimalSeq}()},
\code{\link{owl}()},
\code{\link{rwl}()}
}
\concept{multiple decision point methods}
\concept{single decision point methods}
\concept{statistical methods}
