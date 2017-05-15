\name{.newIterateFit}
\docType{methods}
\alias{.newIterateFit}
\alias{.newIterateFit,modelObj,modelObj,TxInfoNoSubsets-method}
\alias{.newIterateFit,modelObj,modelObj,TxInfoWithSubsets-method}

\title{ Complete Outcome Regression Step When Two Component Model.}

\description{
Complete an outcome regression step when iterative method
is required and subsets are not used for modeling.
Method is not exported.
}

\usage{
.newIterateFit(moMain, moCont, txInfo, ...)

\S4method{.newIterateFit}{modelObj,modelObj,TxInfoNoSubsets}(moMain, moCont, txInfo, response, data, max.iter, suppress)
\S4method{.newIterateFit}{modelObj,modelObj,TxInfoWithSubsets}(moMain, moCont, txInfo, response, data, max.iter, suppress)

}

\arguments{
 \item{moMain}{\code{"modelObj"} for main effects component of model.}
 \item{moCont}{\code{"modelObj"} for contrasts component of model.}
 \item{txInfo}{Treatment information.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{response}{Vector of outcome of interest.}
 \item{data}{\code{"data.frame"} of covariates and treatment histories.}
 \item{max.iter}{Maximum number of iterations.}
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

# Treatment Object
txInfo <- DynTxRegime:::.newTxInfo(fSet = NULL, 
                                   txName = "A1", 
                                   data = bmiData, 
                                   suppress = TRUE, 
                                   verify = TRUE)

obj <- DynTxRegime:::.newIterateFit(moMain = moMain, 
                                    moCont = moCont, 
                                    txInfo = txInfo, 
                                    data = bmiData, 
                                    response = y, 
                                    max.iter = 100L, 
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

fSet1 <- function(data){
           subsets <- list(list("subset1",c("CD","MR")),
                           list("subset2",c("MR")))
           txOpts <- character(nrow(data))
           txOpts[data$baselineBMI <= 35] <- "subset2"
           txOpts[data$baselineBMI > 35] <- "subset1"
           return(list("subsets" = subsets, "txOpts" = txOpts))
         }

txInfo <- DynTxRegime:::.newTxInfo(fSet = fSet1, 
                                   txName = "A2", 
                                   data = bmiData, 
                                   suppress = TRUE, 
                                   verify = TRUE)

obj <- DynTxRegime:::.newIterateFit(moMain = moMain, 
                                    moCont = moCont, 
                                    txInfo = txInfo, 
                                    data = bmiData, 
                                    response = y, 
                                    max.iter = 100L, 
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
}

\keyword{internal}
