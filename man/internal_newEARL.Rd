\name{.newEARL}
\docType{methods}
\alias{.newEARL}
\alias{.newEARL,modelObj,NULL,NULL-method}
\alias{.newEARL,modelObj,modelObj,modelObj-method}
\alias{.newEARL,modelObj,modelObj,NULL-method}
\alias{.newEARL,modelObj,NULL,modelObj-method}

\title{ Efficient Augmentation and Relaxation Learning.}

\description{
Estimates the optimal treatment regime using Efficient Augmentation and Relaxation Learning.
Method is not exported.
}

\usage{
.newEARL(moPropen, moMain, moCont, ...)
\S4method{.newEARL}{modelObj,NULL,NULL}(moPropen, moMain, moCont, data, response, txName, regime, lambdas, 
         cvFolds, surrogate, guess, txVec, suppress)
\S4method{.newEARL}{modelObj,modelObj,modelObj}(moPropen, moMain, moCont, data, response, txName, regime, lambdas, 
         cvFolds, surrogate, iter, guess, txVec, suppress)
\S4method{.newEARL}{modelObj,modelObj,NULL}(moPropen, moMain, moCont, data, response, txName, regime, lambdas, 
         cvFolds, surrogate, iter, guess, txVec, suppress)
\S4method{.newEARL}{modelObj,NULL,modelObj}(moPropen, moMain, moCont, data, response, txName, regime, lambdas, 
         cvFolds, surrogate, iter, guess, txVec, suppress)
}

\arguments{
 \item{moPropen}{\code{"modelObj"} for propensity score modeling.}
 \item{moMain}{\code{"modelObj"} for main effects of outcome modeling.}
 \item{moCont}{\code{"modelObj"} for contrasts of outcome modeling.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
 \item{response}{Outcome of interest.}
 \item{txName}{Treatment variable column header in \code{data}.}
 \item{regime}{\code{"formula"} description of decision function.}
 \item{lambdas}{Tuning parameter(s).}
 \item{cvFolds}{Number of cross-validation folds.}
 \item{surrogate}{\code{"character"} description of 0-1 loss surrogate.}
 \item{iter}{Maximum iterations for outcome regression iterative algorithm.}
 \item{guess}{Inital parameter guesses for optimization routine.}
 \item{txVec}{Treatment vector cast as +/- 1.}
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

moMain <- buildModelObj(model = ~ parentBMI + baselineBMI + month4BMI, 
                        solver.method = 'lm')

regime <- ~ parentBMI + baselineBMI + gender

txVec <- numeric(nrow(bmiData)) - 1L
txVec[bmiData$A2 == "MR"] <- 1L
bmiData$A2 <- as.factor(bmiData$A2)

obj <- DynTxRegime:::.newEARL(moPropen = moPropen,
                              moMain = NULL,
                              moCont = NULL,
                              data = bmiData,
                              response = y,
                              txName = "A2",
                              regime = regime,
                              lambdas = 0.1,
                              cvFolds = 0L,
                              surrogate = "hinge",
                              guess = NULL,
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

obj <- DynTxRegime:::.newEARL(moPropen = moPropen,
                              moMain = moMain,
                              moCont = moMain,
                              data = bmiData,
                              response = y,
                              txName = "A2",
                              regime = regime,
                              lambdas = c(0.1,0.2),
                              cvFolds = 4L,
                              surrogate = "hinge",
                              guess = NULL,
                              txVec = txVec,
                              iter = 0L,
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


