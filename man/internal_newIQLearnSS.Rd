\name{.newIQLearnSS}
\docType{methods}
\alias{.newIQLearnSS}
\alias{.newIQLearnSS,modelObj,modelObj-method}
\alias{.newIQLearnSS,modelObj,NULL-method}
\alias{.newIQLearnSS,NULL,modelObj-method}

\title{ First Step of IQ-Learning Algorithm}

\description{ Second Stage Regression Step of IQ-Learning Algorithm.
Method is not exported.}

\usage{
.newIQLearnSS(moMain, moCont, ...)

\S4method{.newIQLearnSS}{modelObj,modelObj}(moMain, moCont, data, response, txName, iter, suppress)
\S4method{.newIQLearnSS}{modelObj,NULL}(moMain, moCont, data, response, txName, iter, suppress)
\S4method{.newIQLearnSS}{NULL,modelObj}(moMain, moCont, data, response, txName, iter, suppress)
}
\arguments{

 \item{moMain}{\code{"modelObj"} for main effects component of outcome.}
 \item{moCont}{\code{"modelObj"} for contrasts component of outcome.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
 \item{response}{Outcome of interest.}
 \item{txName}{\code{"character"} name of treatment variable in data.}
 \item{iter}{Maximum number of iterations if iterative algorithm used.}
 \item{suppress}{T/F indicating if screen prints are generated.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


\examples{
data(bmiData)

bmiData$A2 <- as.factor(bmiData$A2)

moMain <- buildModelObj(model = ~month4BMI + baselineBMI, 
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~month4BMI + baselineBMI, 
                        solver.method = 'lm')

y <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$month4BMI * 100

obj1 <- DynTxRegime:::.newIQLearnSS(moMain = moMain, 
                                    moCont = moCont, 
                                    response = y,
                                    data = bmiData, 
                                    txName = 'A2', 
                                    iter = 0L, 
                                    suppress = TRUE)

is(obj1)

coef(obj1)
DTRstep(obj1)
fitObject(obj1)
fittedCont(obj1)
fittedMain(obj1)
optTx(obj1)
optTx(obj1,bmiData)
outcome(obj1)
plot(obj1)
propen(obj1)
show(obj1)
summary(obj1)
}

\keyword{internal}
