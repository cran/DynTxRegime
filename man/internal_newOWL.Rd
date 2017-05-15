\name{.newOWL}
\docType{methods}
\alias{.newOWL}
\alias{.newOWL,modelObj-method}

\title{ Outcome Weighted Learning.}

\description{
Estimates the optimal treatment regime using outcome weighted learning.
Method is not exported.
}

\usage{
.newOWL(moPropen,...)
\S4method{.newOWL}{modelObj}(moPropen, data, reward, txName, regime, lambdas, cvFolds, kernel,
        kparam, txVec, suppress)
}

\arguments{
 \item{moPropen}{\code{"modelObj"} for propensity score modeling.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data    }{\code{"data.frame"} of covariates and treatment history.}
 \item{reward  }{Vector of reward.}
 \item{txName  }{Treatment variable column header in \code{data}.}
 \item{regime  }{\code{"formula"} description of decision function.}
 \item{lambdas }{Tuning parameter(s).}
 \item{cvFolds }{Number of cross-validation folds.}
 \item{kernel  }{\code{"character"} description of kernel function.}
 \item{kparam  }{\code{"numeric"} parameter for kernel function.}
 \item{txVec   }{Treatment vector cast as +/- 1.}
 \item{suppress}{T/F indicating if prints to screen are executed.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)

y <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$month4BMI * 100

moPropen <- buildModelObj(model = ~1, 
                          solver.method = 'glm',
                          solver.args = list("family" = "binomial"),
                          predict.args = list("type" = "response"))

regime <- ~ parentBMI + baselineBMI + gender

txVec <- numeric(nrow(bmiData)) - 1L
txVec[bmiData$A2 == "MR"] <- 1L
bmiData$A2 <- as.factor(bmiData$A2)

obj <- DynTxRegime:::.newOWL(moPropen = moPropen,
                             data = bmiData,
                             reward = y,
                             txName = "A2",
                             regime = regime,
                             lambdas = 0.1,
                             cvFolds = 0L,
                             kernel = "linear",
                             kparam = NULL,
                             txVec = txVec,
                             suppress = TRUE)

is(obj)
coef(obj)
cvInfo(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optimObj(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
print(obj)
propen(obj)
regimeCoef(obj)
show(obj)
summary(obj)

obj <- DynTxRegime:::.newOWL(moPropen = moPropen,
                             data = bmiData,
                             reward = y,
                             txName = "A2",
                             regime = regime,
                             lambdas = c(0.1,0.2),
                             cvFolds = 4L,
                             kernel = "linear",
                             kparam = NULL,
                             txVec = txVec,
                             suppress = TRUE)

is(obj)
coef(obj)
cvInfo(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optimObj(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
print(obj)
propen(obj)
regimeCoef(obj)
show(obj)
summary(obj)


obj <- DynTxRegime:::.newOWL(moPropen = moPropen,
                             data = bmiData,
                             reward = y,
                             txName = "A2",
                             regime = regime,
                             lambdas = c(0.1,0.2),
                             cvFolds = 4L,
                             kernel = "radial",
                             kparam = c(1.0,2.0),
                             txVec = txVec,
                             suppress = TRUE)

is(obj)
coef(obj)
cvInfo(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optimObj(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
print(obj)
propen(obj)
regimeCoef(obj)
show(obj)
summary(obj)

obj <- DynTxRegime:::.newOWL(moPropen = moPropen,
                             data = bmiData,
                             reward = y,
                             txName = "A2",
                             regime = regime,
                             lambdas = 0.1,
                             cvFolds = 4L,
                             kernel = "radial",
                             kparam = c(1.0,2.0),
                             txVec = txVec,
                             suppress = TRUE)

is(obj)
coef(obj)
cvInfo(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optimObj(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
print(obj)
propen(obj)
regimeCoef(obj)
show(obj)
summary(obj)

}

\keyword{internal}

