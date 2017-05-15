\name{earl}
\alias{earl}

\title{
Efficient Augmentation and Relaxation Learning
}

\description{
Estimation of optimal treatment regime using efficient augmentation 
and relaxation learning (EARL). 
The method is limited to single-decision-point scenarios with
binary treatment options. 
}

\usage{ 
earl(..., moPropen, moMain, moCont, data, response, txName, regime,
     iter = 0L, lambdas = 0.5, cvFolds = 0L, surrogate = "hinge",
     guess = NULL, verbose = TRUE)
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

 \item{moMain   }{An object of class \code{"modelObj"} or 
                  NULL.
                  If a \code{"modelObj,"} this input specifies the model
                  of the main effects component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{moCont   }{An object of class \code{"modelObj"} or 
                  NULL.
                  If a \code{"modelObj,"} this input specifies the model
                  of the contrasts component of the outcome regression
                  and the R methods to be used to obtain parameter
                  estimates and predictions.
                  The method specified to obtain predictions must return
                  the prediction on the scale of the response variable.}

 \item{data     }{An object of class \code{"data.frame."}
                  The covariates and treatment history.}

  \item{response }{An object of class \code{"vector."}
                  The outcome of interest.}

 \item{txName   }{An object of class \code{"character."}
                  The column header of the stage treatment variable
                  as given in input \code{data}. Not that treatment
                  must be binary and will be recoded as -1/+1 if
                  not provided as such.}

  \item{regime }{An object of class \code{"formula."}
                 A formula defining the decision rule.}

  \item{iter   }{An object of class \code{"integer."} 
                 If >0, the iterative method will be used to 
                 obtain parameter estimates in the outcome regression step.
                 See \link{iter} for further information.}

  \item{lambdas }{An object of class \code{"numeric."}
                  The tuning parameter to avoid overfitting.
                  If a vector of values is given and \code{cvFolds} > 0, 
                  cross-validation will be used to select
                  an optimal value from among those provided.}

  \item{cvFolds}{An object of class \code{"integer."}
                 If cross-validation is to be used to find
                 an optimal lambda parameter, 
                 the number of folds to use in the 
                 cross-validation procedure.}

  \item{surrogate}{An object of class \code{"character."}
                Specification of the surrogate
                for the 0-1 loss function.
                Must be one of \{'hinge', 'logit', 'exp', 'sqhinge'\}
                indicating the hinge, logistic, exponential, and
                squared-hinge functions, respectively}

  \item{guess }{An object of class \code{"numeric"} or NULL.
                Starting parameter values for optimization method.}

  \item{verbose }{An object of class \code{"logical."} 
                  If FALSE, screen prints will be suppressed.}

}


\details{

  The inverse propensity weighted estimator is calculated
  if both \code{moMain} and \code{moCont} are NULL; otherwise
  the augmented inverse propensity weighted estimator is calculated.

}

\value{
       Returns an object of class \code{"EARL"} 
that inherits directly from class \code{"DynTxRegime."}
}
\section{Methods}{
  \describe{
    \item{coef}{\code{signature(object = "EARL")}: 
       Retrieve parameter estimates for all regression steps. }

    \item{cvInfo}{\code{signature(object = "EARL")}: 
       Retrieve cross-validation results. }

    \item{DTRstep}{\code{signature(object = "EARL")}: 
       Retrieve description of method used to create object. }

    \item{estimator}{\code{signature(x = "EARL")}: 
       Retrieve the estimated value of the estimated
       optimal regime for the training data set. }

    \item{fitObject}{\code{signature(object = "EARL")}: 
       Retrieve value object returned by regression methods. }

    \item{optimObj}{\code{signature(object = "EARL")}: 
       Retrieve value object returned by optimization routine.}

    \item{optTx}{\code{signature(x = "EARL", newdata = "missing")}: 
       Retrieve the estimated optimal treatment regime for
       training data set. }

    \item{optTx}{\code{signature(x = "EARL", newdata = "data.frame")}: 
       Estimate the optimal treatment regime for newdata. }

    \item{outcome}{\code{signature(x = "EARL")}: 
       Retrieve value object returned by outcome regression methods.  }

    \item{plot}{\code{signature(x = "EARL")}: 
       Generate plots for regression analyses.  }

    \item{print}{\code{signature(object = "EARL")}: 
       Print main results of analysis.}

    \item{propen}{\code{signature(x = "EARL")}: 
       Retrieve value object returned by propensity score regression methods.}

    \item{regimeCoef}{\code{signature(object = "EARL")}: 
       Retrieve the estimated decision function parameter estimates.}

    \item{show}{\code{signature(object = "EARL")}: 
       Show main results of analysis.}

    \item{summary}{\code{signature(object = "EARL")}: 
       Retrieve summary information from regression analyses.}

  }
}

\author{
Shannon T. Holloway \email{sthollow@ncsu.edu}
}

\references{
Zhao, Y-Q., Laber, E. B., Saha, S., and Sands, B. E.
(2017+).
Efficient Augmentation and Relaxation Learning for Treatment
Regimes Using Observational Data.
in press.
}


\examples{
# Load and process data set
  data(bmiData)

  # define response y to be the negative 12 month
  # change in BMI from baseline
  bmiData$y <- -100*(bmiData[,6] - bmiData[,4])/bmiData[,4]

  bmiData$y <- bmiData$y - min(bmiData$y) + 0.001

# Constant propensity model
  moPropen <- buildModelObj(model = ~1,
                            solver.method = 'glm',
                            solver.args = list('family'='binomial'),
                            predict.method = 'predict.glm',
                            predict.args = list(type='response'))

# IPWE
  earlRes <- earl(moPropen = moPropen, moMain = NULL, moCont = NULL,
                  data = bmiData, response = bmiData$y, txName = "A2", 
                  regime = ~ parentBMI + month4BMI)

#Available methods

# Coefficients of the propensity score regression
coef(earlRes)

# Description of method used to obtain object
DTRstep(earlRes)

# Estimated value of the optimal treatment regime for training set
estimator(earlRes)

# Value object returned by propensity score regression method
fitObject(earlRes)

# Summary of optimization routine
optimObj(earlRes)

# Estimated optimal treatment for training data
optTx(earlRes)

# Estimated optimal treatment for new data
optTx(earlRes, bmiData)

# Plots if defined by propensity regression method
dev.new()
par(mfrow = c(2,4))

plot(earlRes, suppress = FALSE)
plot(earlRes, suppress = TRUE)

# Value object returned by propensity score regression method
propen(earlRes)

# Parameter estimates for decision function
regimeCoef(earlRes)

# Show main results of method
show(earlRes)

# Show summary results of method
summary(earlRes)
 


# Augmented IPWE
  # Create modeling object for main effect component
  moMain <- buildModelObj(model = ~ gender + parentBMI + month4BMI,
                          solver.method='lm')

  # Create modeling object for contrast component
  moCont <- buildModelObj(model = ~ parentBMI + month4BMI,
                          solver.method='lm')

  earlResA <- earl(moPropen = moPropen, moMain = moMain, moCont = moCont,
                   data = bmiData, response = bmiData$y, txName = "A2", 
                   regime = ~ parentBMI + month4BMI)

#Available methods

# Coefficients of the propensity score and outcome regressions
coef(earlResA)

# Description of method used to obtain object
DTRstep(earlResA)

# Estimated value of the optimal treatment regime for training set
estimator(earlResA)

# Value object returned by propensity score and outcome regression methods
fitObject(earlResA)

# Summary of optimization routine
optimObj(earlResA)

# Estimated optimal treatment for training data
optTx(earlResA)

# Estimated optimal treatment for new data
optTx(earlResA, bmiData)

# Value object returned by outcome regression method
outcome(earlResA)

# Plots if defined by propensity score and outcome regressionmethod
dev.new()
par(mfrow = c(2,4))
plot(earlResA, suppress = FALSE)

dev.new()
par(mfrow = c(2,4))
plot(earlResA, suppress = TRUE)

# Value object returned by propensity score regression method
propen(earlResA)

# Parameter estimates for decision function
regimeCoef(earlResA)

# Show main results of method
show(earlResA)

# Show summary results of method
summary(earlResA)
 
}

