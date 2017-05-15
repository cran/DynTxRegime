\name{.newQLearn}
\docType{methods}
\alias{.newQLearn}
\alias{.newQLearn,modelObj,modelObj,function,vector-method}
\alias{.newQLearn,modelObj,NULL,function,vector-method}
\alias{.newQLearn,NULL,modelObj,function,vector-method}
\alias{.newQLearn,modelObj,modelObj,NULL,vector-method}
\alias{.newQLearn,modelObj,NULL,NULL,vector-method}
\alias{.newQLearn,NULL,modelObj,NULL,vector-method}
\alias{.newQLearn,ModelObj_SubsetList,ModelObj_SubsetList,function,vector-method}
\alias{.newQLearn,ModelObj_SubsetList,NULL,function,vector-method}
\alias{.newQLearn,NULL,ModelObj_SubsetList,function,vector-method}
\alias{.newQLearn,modelObj,modelObj,function,QLearn-method}
\alias{.newQLearn,modelObj,NULL,function,QLearn-method}
\alias{.newQLearn,NULL,modelObj,function,QLearn-method}
\alias{.newQLearn,modelObj,modelObj,NULL,QLearn-method}
\alias{.newQLearn,modelObj,NULL,NULL,QLearn-method}
\alias{.newQLearn,NULL,modelObj,NULL,QLearn-method}
\alias{.newQLearn,ModelObj_SubsetList,ModelObj_SubsetList,function,QLearn-method}
\alias{.newQLearn,ModelObj_SubsetList,NULL,function,QLearn-method}
\alias{.newQLearn,NULL,ModelObj_SubsetList,function,QLearn-method}

\title{ A Step of the Q-Learning Algorithm}

\description{ Regression of a single stage. For multiple
stage analyses, function called for each decision point. Method is not exported.}

\usage{

.newQLearn(moMain, moCont, fSet, response, ...)
\S4method{.newQLearn}{modelObj,modelObj,function,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,NULL,function,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{NULL,modelObj,function,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,modelObj,NULL,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,NULL,NULL,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{NULL,modelObj,NULL,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{ModelObj_SubsetList,ModelObj_SubsetList,function,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{ModelObj_SubsetList,NULL,function,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{NULL,ModelObj_SubsetList,function,vector}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,modelObj,function,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,NULL,function,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{NULL,modelObj,function,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,modelObj,NULL,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{modelObj,NULL,NULL,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{NULL,modelObj,NULL,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{ModelObj_SubsetList,ModelObj_SubsetList,function,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{ModelObj_SubsetList,NULL,function,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
\S4method{.newQLearn}{NULL,ModelObj_SubsetList,function,QLearn}(moMain, moCont, fSet, response, data, txName, iter, suppress)
}

\arguments{
  \item{moMain}{\code{"modelObj"(s)} for main effects component of outcome.}
  \item{moCont}{\code{"modelObj"(s)} for contrasts component of outcome.}
  \item{fSet}{\code{"function"} defining feasible treatments and/or modeling subsets.}
  \item{response}{If vector, outcome of interest. If \code{"QLearn"}, value object
    returned by previous call to Q-Learning algorithm}
  \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
  \item{data}{\code{"data.frame"} of covariates and treatment history.}
  \item{txName}{\code{"character"} indicating column header of \code{data} for treatment variable.}
  \item{iter}{Maximum number of iterations if iterative algorithm requested.}
  \item{suppress}{T/F indicating if screen prints are generated.}
}


\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}


\examples{

data(bmiData)

y <- -(bmiData$month12BMI - bmiData$baselineBMI) / bmiData$baselineBMI * 100

bmiData$A2 <- as.factor(bmiData$A2)
bmiData$A1 <- as.factor(bmiData$A1)

moMain <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~parentBMI+baselineBMI+month4BMI, 
                        solver.method = 'lm')

obj1 <- DynTxRegime:::.newQLearn(moMain = moMain, 
                                 moCont = moCont, 
                                 fSet = NULL, 
                                 response = y,
                                 data = bmiData, 
                                 txName = 'A2', 
                                 iter = 0L, 
                                 suppress = TRUE)

is(obj1)

coef(obj1)
DTRstep(obj1)
estimator(obj1)
fitObject(obj1)
optTx(obj1)
optTx(obj1,bmiData)
outcome(obj1)
plot(obj1)
propen(obj1)
show(obj1)
summary(obj1)

moMain2 <- buildModelObj(model = ~parentBMI+baselineBMI, 
                         solver.method = 'lm')
moCont2 <- buildModelObj(model = ~parentBMI+baselineBMI, 
                         solver.method = 'lm')

obj2 <- DynTxRegime:::.newQLearn(moMain = moMain2, 
                                 moCont = moCont2,  
                                 fSet = NULL,  
                                 response = obj1,
                                 data = bmiData,  
                                 txName = 'A1',  
                                 iter = 0L,  
                                 suppress = TRUE)

is(obj2)

coef(obj2)
DTRstep(obj2)
estimator(obj2)
fitObject(obj2)
optTx(obj2)
optTx(obj2,bmiData)
outcome(obj2)
plot(obj2)
propen(obj2)
show(obj2)
summary(obj2)

fSet <- function(data){
          subsets <- list(list("subset1",c("CD","MR")),
                          list("subset2",c("CD","MR")))
          txOpts <- character(nrow(data))
          txOpts[data$baselineBMI < 32] <- "subset2"
          txOpts[data$baselineBMI >=32] <- "subset1"
          return(list("subsets" = subsets, "txOpts" = txOpts))
        }

moMain3 <- list()
moMain3[[1L]] <- buildModelObjSubset(model = ~parentBMI+baselineBMI, 
                                     solver.method = 'lm',  
                                     subset = "subset1")
moMain3[[2L]] <- buildModelObjSubset(model = ~parentBMI+baselineBMI,  
                                     solver.method = 'lm',  
                                     subset = "subset2")
moCont3 <- list()
moCont3[[1L]] <- buildModelObjSubset(model = ~parentBMI+baselineBMI,  
                                     solver.method = 'lm',  
                                     subset = "subset1")
moCont3[[2L]] <- buildModelObjSubset(model = ~parentBMI+baselineBMI,  
                                     solver.method = 'lm',  
                                     subset = "subset2")

moMain3 <- DynTxRegime:::.newModelObjSubset(moMain3)
moCont3 <- DynTxRegime:::.newModelObjSubset(moCont3)

obj2 <- DynTxRegime:::.newQLearn(moMain = moMain3, 
                                 moCont = moCont3, 
                                 fSet = fSet, 
                                 response = obj1,
                                 data = bmiData, 
                                 txName = 'A1', 
                                 iter = 0L, 
                                 suppress = TRUE)

is(obj2)

coef(obj2)
DTRstep(obj2)
estimator(obj2)
fitObject(obj2)
optTx(obj2)
optTx(obj2,bmiData)
outcome(obj2)
plot(obj2)
propen(obj2)
show(obj2)
summary(obj2)

}
\keyword{internal}
