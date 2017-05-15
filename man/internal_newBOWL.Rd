\name{.newBOWL}
\docType{methods}
\alias{.newBOWL}
\alias{.newBOWL,ModelObj_SubsetList,formula,NULL,function-method}
\alias{.newBOWL,modelObj,formula,NULL,NULL-method}
\alias{.newBOWL,modelObj,formula,NULL,function-method}
\alias{.newBOWL,ModelObj_SubsetList,list,NULL,function-method}
\alias{.newBOWL,modelObj,list,NULL,function-method}
\alias{.newBOWL,ModelObj_SubsetList,formula,BOWL,function-method}
\alias{.newBOWL,modelObj,formula,BOWL,NULL-method}
\alias{.newBOWL,modelObj,formula,BOWL,function-method}
\alias{.newBOWL,ModelObj_SubsetList,list,BOWL,function-method}
\alias{.newBOWL,modelObj,list,BOWL,function-method}

\title{ Backward Outcome Weighted Learning.}

\description{
Estimates the optimal treatment using outcome weighted learning.
Method is not exported.
}

\usage{
.newBOWL(moPropen, regime, BOWLObj, fSet, ...)
\S4method{.newBOWL}{ModelObj_SubsetList,formula,NULL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas, 
         kernel, kparam, suppress)
\S4method{.newBOWL}{modelObj,formula,NULL,NULL}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{modelObj,formula,NULL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{ModelObj_SubsetList,list,NULL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{modelObj,list,NULL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{ModelObj_SubsetList,formula,BOWL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{modelObj,formula,BOWL,NULL}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{modelObj,formula,BOWL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{ModelObj_SubsetList,list,BOWL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
\S4method{.newBOWL}{modelObj,list,BOWL,function}(moPropen, regime, BOWLObj, fSet, data, reward, txName, cvFolds, lambdas,  
         kernel, kparam, suppress)
}

\arguments{
 \item{moPropen}{\code{"modelObj"} or \code{"ModelObj_SubsetList"}
                 for propensity score regression(s).}
 \item{regime}{\code{"formula"} object indicating covariates to include in kernel.}
 \item{BOWLObj}{NULL or a \code{"BOWL"} object from a previous call to bowl()}
 \item{fSet}{NULL or function indicating treatment subsets.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
 \item{reward}{Vector of rewards for current decision point.}
 \item{txName}{\code{"character"} giving name of treatment variable in data.}
 \item{cvFolds}{Number of cross-validation folds.}
 \item{lambdas}{Tuning parameter(s).}
 \item{kernel}{\code{"character"} description of kernel function.}
 \item{kparam}{NULL or \code{"numeric"} kernel parameter.}
 \item{suppress}{T/F indicating if screen prints are generated.}

}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
data(bmiData)

y12 <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$baselineBMI * 100

y4 <- -(bmiData$month4BMI - bmiData$baselineBMI) / bmiData$baselineBMI * 100

moPropen <- buildModelObj(model=~1,
                          solver.method = 'glm',
                          solver.args = list("family"="binomial"),
                          predict.args = list("type" = "response"))

regime <- ~ parentBMI + baselineBMI + gender

bmiData$A2 <- as.factor(bmiData$A2)
bmiData$A1 <- as.factor(bmiData$A1)

obj1 <- DynTxRegime:::.newBOWL(moPropen = moPropen,
                               regime = regime,
                               BOWLObj = NULL,
                               fSet = NULL,
                               data = bmiData,
                               reward = y12,
                               txName = "A2",
                               cvFolds = 0L,
                               lambdas = 0.1,
                               kernel = "linear",
                               kparam = NULL,
                               suppress = TRUE)

is(obj1)
cvInfo(obj1)
DTRstep(obj1)
optimObj(obj1)
optTx(obj1)
optTx(obj1,bmiData)
print(obj1)
regimeCoef(obj1)
show(obj1)
summary(obj1)


fSet <- function(data){
          subsets = list(list("subset1", c("CD","MR")),
                         list("subset2", c("CD","MR")))
          txOpts <- character(nrow(data))
          txOpts[data$A1 == "CD"] <- "subset1"
          txOpts[data$A1 == "MR"] <- "subset2"

          return(list("subsets" = subsets, "txOpts" = txOpts))
        }

obj1 <- DynTxRegime:::.newBOWL(moPropen = moPropen,
                               regime = list("subset1"=regime,"subset2"=regime),
                               BOWLObj = NULL,
                               fSet = fSet,
                               data = bmiData,
                               reward = y12,
                               txName = "A2",
                               cvFolds = 0L,
                               lambdas = 0.1,
                               kernel = "linear",
                               kparam = NULL,
                               suppress = TRUE)

is(obj1)
cvInfo(obj1)
DTRstep(obj1)
optimObj(obj1)
optTx(obj1)
optTx(obj1,bmiData)
print(obj1)
regimeCoef(obj1)
show(obj1)
summary(obj1)

bmiData$A2[bmiData$A1 == "MR"] <- "CD"

fSet <- function(data){
          subsets = list(list("subset1", c("CD","MR")),
                         list("subset2", c("CD")))
          txOpts <- character(nrow(data))
          txOpts[data$A1 == "CD"] <- "subset1"
          txOpts[data$A1 == "MR"] <- "subset2"

          return(list("subsets" = subsets, "txOpts" = txOpts))
        }

obj1 <- DynTxRegime:::.newBOWL(moPropen = moPropen,
                               regime = regime,
                               BOWLObj = NULL,
                               fSet = fSet,
                               data = bmiData,
                               reward = y12,
                               txName = "A2",
                               cvFolds = 0L,
                               lambdas = 0.1,
                               kernel = "linear",
                               kparam = NULL,
                               suppress = TRUE)

is(obj1)
cvInfo(obj1)
DTRstep(obj1)
optimObj(obj1)
optTx(obj1)
optTx(obj1,bmiData)
print(obj1)
regimeCoef(obj1)
show(obj1)
summary(obj1)

obj2 <- DynTxRegime:::.newBOWL(moPropen = moPropen,
                               regime = regime,
                               BOWLObj = obj1,
                               fSet = NULL,
                               data = bmiData,
                               reward = y4,
                               txName = "A1",
                               cvFolds = 0L,
                               lambdas = 0.1,
                               kernel = "linear",
                               kparam = NULL,
                               suppress = TRUE)

is(obj2)
cvInfo(obj2)
DTRstep(obj2)
optimObj(obj2)
optTx(obj2)
optTx(obj2,bmiData)
print(obj2)
regimeCoef(obj2)
show(obj2)
summary(obj2)
}

\keyword{internal}
