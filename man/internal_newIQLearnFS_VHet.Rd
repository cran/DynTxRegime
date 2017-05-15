\name{.newIQLearnFS_VHet}
\docType{methods}
\alias{.newIQLearnFS_VHet}
\alias{.newIQLearnFS_VHet,IQLearnFS_C,modelObj,modelObj-method}
\alias{.newIQLearnFS_VHet,IQLearnFS_C,modelObj,NULL-method}
\alias{.newIQLearnFS_VHet,IQLearnFS_C,NULL,modelObj-method}

\title{ Fourth Step of IQ-Learning Algorithm}

\description{ Regression of residuals. Method is not exported.}

\usage{
.newIQLearnFS_VHet(object, moMain, moCont, ...)

\S4method{.newIQLearnFS_VHet}{IQLearnFS_C,modelObj,modelObj}(object, moMain, moCont, data, iter, suppress)
\S4method{.newIQLearnFS_VHet}{IQLearnFS_C,modelObj,NULL}(object, moMain, moCont, data, iter, suppress)
\S4method{.newIQLearnFS_VHet}{IQLearnFS_C,NULL,modelObj}(object, moMain, moCont, data, iter, suppress)
}
\arguments{

 \item{object}{An object of class \code{"\link{IQLearnFS_C-class}."}}
 \item{moMain}{\code{"modelObj"} for main effects component of variance.}
 \item{moCont}{\code{"modelObj"} for contrasts component of variance.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
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

obj2 <- DynTxRegime:::.newIQLearnFS_C(moMain = moMain, 
                                      moCont = moCont,  
                                      response = obj1,
                                      data = bmiData,  
                                      txName = 'A1',  
                                      iter = 0L,  
                                      suppress = TRUE)

obj3 <- DynTxRegime:::.newIQLearnFS_VHet(object = obj2, 
                                         moMain = moMain, 
                                         moCont = moCont,
                                         data = bmiData, 
                                         iter = 0L, 
                                         suppress = TRUE)

is(obj3)

coef(obj3)
DTRstep(obj3)
fitObject(obj3)
outcome(obj3)
plot(obj3)
propen(obj3)
show(obj3)
summary(obj3)
}

\keyword{internal}
