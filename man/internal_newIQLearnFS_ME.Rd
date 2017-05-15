\name{.newIQLearnFS_ME}
\docType{methods}
\alias{.newIQLearnFS_ME}
\alias{.newIQLearnFS_ME,modelObj,modelObj-method}
\alias{.newIQLearnFS_ME,modelObj,NULL-method}
\alias{.newIQLearnFS_ME,NULL,modelObj-method}

\title{ Third Step of IQ-Learning Algorithm}

\description{ Regression of Estimated Main Effects of Second Stage Outcome
 Step of IQ-Learning Algorithm. Method is not exported.}

\usage{
.newIQLearnFS_ME(moMain, moCont, response, ...)

\S4method{.newIQLearnFS_ME}{modelObj,modelObj}(moMain, moCont, response, data, txName, iter, suppress)
\S4method{.newIQLearnFS_ME}{modelObj,NULL}(moMain, moCont, response, data, txName, iter, suppress)
\S4method{.newIQLearnFS_ME}{NULL,modelObj}(moMain, moCont, response, data, txName, iter, suppress)
}
\arguments{

 \item{moMain}{\code{"modelObj"} for main effects component of estimated main effects.}
 \item{moCont}{\code{"modelObj"} for contrasts component of estimated main effects.}
 \item{response}{An object of class \code{"\link{IQLearnSS-class}."}}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
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
bmiData$A1 <- as.factor(bmiData$A1)

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


moMain <- buildModelObj(model = ~baselineBMI + parentBMI, 
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~baselineBMI + parentBMI, 
                        solver.method = 'lm')

obj2 <- DynTxRegime:::.newIQLearnFS_ME(moMain = moMain, 
                                       moCont = moCont, 
                                       response = obj1,
                                       data = bmiData, 
                                       txName = 'A1', 
                                       iter = 0L, 
                                       suppress = TRUE)

is(obj2)

coef(obj2)
DTRstep(obj2)
fitObject(obj2)
outcome(obj2)
plot(obj2)
propen(obj2)
show(obj2)
summary(obj2)
}

\keyword{internal}
