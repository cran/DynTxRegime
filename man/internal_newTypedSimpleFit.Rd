\name{.newTypedSimpleFit}
\docType{methods}
\alias{.newTypedSimpleFit}
\alias{.newTypedSimpleFit,modelObj,modelObj,TxInfoNoSubsets-method}
\alias{.newTypedSimpleFit,NULL,modelObj,TxInfoNoSubsets-method}
\alias{.newTypedSimpleFit,modelObj,NULL,TxInfoNoSubsets-method}
\alias{.newTypedSimpleFit,modelObj,modelObj,TxInfoWithSubsets-method}
\alias{.newTypedSimpleFit,NULL,modelObj,TxInfoWithSubsets-method}
\alias{.newTypedSimpleFit,modelObj,NULL,TxInfoWithSubsets-method}

\title{ Complete Outcome Regression Step When Single Model.}

\description{
Complete an outcome regression step when iterative method
is not required and subsets are not used for modeling.
Method is not exported.
}

\usage{
.newTypedSimpleFit(moMain, moCont, txInfo, ...)

\S4method{.newTypedSimpleFit}{modelObj,modelObj,TxInfoNoSubsets}(moMain, moCont, txInfo, data, response, suppress)
\S4method{.newTypedSimpleFit}{modelObj,NULL,TxInfoNoSubsets}(moMain, moCont, txInfo, data, response, suppress)
\S4method{.newTypedSimpleFit}{NULL,modelObj,TxInfoNoSubsets}(moMain, moCont, txInfo, data, response, suppress)
\S4method{.newTypedSimpleFit}{modelObj,modelObj,TxInfoWithSubsets}(moMain, moCont, txInfo, data, response, suppress)
\S4method{.newTypedSimpleFit}{modelObj,NULL,TxInfoWithSubsets}(moMain, moCont, txInfo, data, response, suppress)
\S4method{.newTypedSimpleFit}{NULL,modelObj,TxInfoWithSubsets}(moMain, moCont, txInfo, data, response, suppress)

}

\arguments{
 \item{moMain}{\code{"modelObj"} for main effects component of outcome.}
 \item{moCont}{\code{"modelObj"} for contrast component of outcome.}
 \item{txInfo}{Treatment information.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{data}{\code{"data.frame"} of covariates and treatment history.}
 \item{response}{Vector of outcome of interest.}
 \item{suppress}{T/F indicating if screen prints are generated.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)

y <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$month4BMI * 100

moMain <- buildModelObj(model = ~ parentBMI + baselineBMI + month4BMI, 
                        solver.method = 'lm')
moCont <- buildModelObj(model = ~ parentBMI + baselineBMI + month4BMI, 
                        solver.method = 'lm')

# Treatment Object
txInfo <- DynTxRegime:::.newTxInfo(fSet = NULL, 
                                   txName = "A2", 
                                   data = bmiData, 
                                   suppress = TRUE, 
                                   verify = TRUE)

obj <- DynTxRegime:::.newTypedSimpleFit(moMain = moMain, 
                                        moCont = moCont,  
                                        txInfo = txInfo, 
                                        data = bmiData,  
                                        response = y,  
                                        suppress = TRUE)

is(obj)
coef(obj)
fitObject(obj)
plot(obj)
predict(obj)
predict(obj, bmiData)
DynTxRegime:::.predictAllTreatments(object = obj, data = bmiData)
print(obj)
show(obj)
summary(obj)

obj <- DynTxRegime:::.newTypedSimpleFit(moMain = moMain, 
                                        moCont = NULL,  
                                        txInfo = txInfo, 
                                        data = bmiData,  
                                        response = y,  
                                        suppress = TRUE)

is(obj)
print(obj)

obj <- DynTxRegime:::.newTypedSimpleFit(moMain = NULL, 
                                        moCont = moCont,  
                                        txInfo = txInfo, 
                                        data = bmiData,  
                                        response = y,  
                                        suppress = TRUE)


is(obj)
print(obj)

fSet <- function(data){
          subsets <- list(list("subset1", c("CD","MR")),
                          list("subset2", c("CD","MR")))
         txOpts <- character(nrow(data))
         txOpts[data$A1 == "CD"] <- "subset1"
         txOpts[data$A1 == "MR"] <- "subset2"
         return(list("subsets" = subsets, "txOpts" = txOpts))
        }

# Integer treatment with subsetting
txInfo <- DynTxRegime:::.newTxInfo(fSet = fSet, 
                                   txName = "A2", 
                                   data = bmiData, 
                                   suppress = TRUE, 
                                   verify = TRUE)

obj <- DynTxRegime:::.newTypedSimpleFit(moMain = moMain, 
                                        moCont = moCont, 
                                        txInfo = txInfo, 
                                        data = bmiData, 
                                        response = y, 
                                        suppress = TRUE)

is(obj)
coef(obj)
fitObject(obj)
plot(obj)
predict(obj)
predict(obj, bmiData)
DynTxRegime:::.predictAllTreatments(object = obj, data = bmiData)
print(obj)
show(obj)
summary(obj)


obj <- DynTxRegime:::.newTypedSimpleFit(moMain = moMain, 
                                        moCont = NULL, 
                                        txInfo = txInfo, 
                                        data = bmiData, 
                                        response = y, 
                                        suppress = TRUE)

is(obj)
print(obj)

obj <- DynTxRegime:::.newTypedSimpleFit(moMain = NULL, 
                                        moCont = moCont, 
                                        txInfo = txInfo, 
                                        data = bmiData, 
                                        response = y, 
                                        suppress = TRUE)
is(obj)
print(obj)

}

\keyword{internal}
