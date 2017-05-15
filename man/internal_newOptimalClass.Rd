\name{.newOptimalClass}
\docType{methods}
\alias{.newOptimalClass}
\alias{.newOptimalClass,modelObj,modelObj,modelObj,modelObj-method}
\alias{.newOptimalClass,modelObj,modelObj,NULL,modelObj-method}
\alias{.newOptimalClass,modelObj,NULL,modelObj,modelObj-method}
\alias{.newOptimalClass,modelObj,NULL,NULL,modelObj-method}

\title{ Optimal Treatment Regime from Classification Perspective.}

\description{
Estimates the optimal treatment from the classification
perspective. Method is not exported.
}

\usage{
.newOptimalClass(moPropen, moMain, moCont, moClass,...)
\S4method{.newOptimalClass}{modelObj,NULL,NULL,modelObj}(moPropen,moMain,moCont,moClass,data,response,txName,suppress)
\S4method{.newOptimalClass}{modelObj,modelObj,modelObj,modelObj}(moPropen,moMain,moCont,moClass,data,response,txName,iter,suppress)
\S4method{.newOptimalClass}{modelObj,modelObj,NULL,modelObj}(moPropen,moMain,moCont,moClass,data,response,txName,iter,suppress)
\S4method{.newOptimalClass}{modelObj,NULL,modelObj,modelObj}(moPropen,moMain,moCont,moClass,data,response,txName,iter,suppress)
}

\arguments{

\item{moPropen}{\code{"modelObj"} for propensity score regression.}
\item{moMain}{\code{"modelObj"} for main effects of outcome regression.}
\item{moCont}{\code{"modelObj"} for contrasts of outcome regression.}
\item{moClass}{\code{"modelObj"} for classification.}
\item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
\item{data}{\code{"data.frame"} of covariates and treatment history.}
\item{response}{Outcome of interest.}
\item{txName}{\code{"character"} name of column header in \code{data}
        containing treatment variable.}
\item{iter}{Maximum number of iterations for iterative algorithm.}
\item{suppress}{T/F indicating if screen prints are generated.}

}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)
library(rpart)

y <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$month4BMI * 100

bmiData$A2 <- as.factor(bmiData$A2)

moPropen <- buildModelObj(model = ~1, 
                          solver.method = 'glm',
                          solver.args = list("family" = "binomial"),
                          predict.args = list("type" = "response"))

moMain <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                        solver.method = 'lm')

moClass <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                         solver.method = 'rpart',
                         predict.args = list("type"="class"))


obj <- DynTxRegime:::.newOptimalClass(moPropen = moPropen,
                                      moMain = NULL, 
                                      moCont = NULL, 
                                      moClass = moClass,
                                      data = bmiData,
                                      response = y,
                                      txName = 'A2', 
                                      suppress = TRUE)

is(obj)

coef(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
plot(obj)
propen(obj)
show(obj)
summary(obj)

obj <- DynTxRegime:::.newOptimalClass(moPropen = moPropen,
                                      moMain = moMain, 
                                      moCont = moCont, 
                                      moClass = moClass,
                                      data = bmiData,
                                      response = y,
                                      txName = 'A2', 
                                      iter = 0L,
                                      suppress = TRUE)

is(obj)

coef(obj)
DTRstep(obj)
estimator(obj)
fitObject(obj)
optTx(obj)
optTx(obj,bmiData)
outcome(obj)
plot(obj)
propen(obj)
show(obj)
summary(obj)

}

\keyword{internal}
