\name{.newBOWLBasic}
\docType{methods}
\alias{.newBOWLBasic}
\alias{.newBOWLBasic,formula,txInfoNoSubsets-method}
\alias{.newBOWLBasic,formula,txInfoWithSubsets-method}

\title{ Backward Outcome Weighted Learning With Binary Treatment.}

\description{
Estimates the optimal treatment using outcome
weighted learning for a single decision point and binary treatments.
Method is not exported.
}

\usage{
.newBOWLBasic(regime, txInfo, ...)
\S4method{.newBOWLBasic}{formula,txInfoNoSubsets}(regime, txInfo, ind, prWgt, response, txVec, data, kernel, kparam, 
              lambdas, cvFolds, suppress)
\S4method{.newBOWLBasic}{formula,txInfoWithSubsets}(regime, txInfo, ind, prWgt, response, txVec, data, kernel, kparam, 
              lambdas, cvFolds, suppress)
}

\arguments{
\item{regime }{\code{"formula"} description of decision rule.}
\item{txInfo }{\code{"TxInfoNoSubsets"} object.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
\item{ind    }{T/F indicating if optimal treatment has been followed
               beyond this decision point.}
\item{prWgt  }{Product of probabilities for received treatment beyond
               this decision point.}
\item{response}{Sum of rewards including this decision point.}
\item{txVec  }{Recast treatment vector as +/-1.}
\item{data   }{\code{"data.frame"} of covariates and treatment histories.}
\item{kernel }{\code{"character"} description of kernel function.}
\item{kparam }{\code{"numeric"} parameter for kernel function.}
\item{lambdas}{Tuning parameter(s).}
\item{cvFolds}{Number of cross-validation folds.}
\item{suppress}{T/F indicating if print to screens should be executed}
}


\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)

y <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$month4BMI * 100
miny <- min(y)
if(miny < 0.0) y <- y - miny

prWgt <- numeric(nrow(bmiData)) + 0.5

regime <- ~ parentBMI + baselineBMI + gender

txVec <- numeric(nrow(bmiData)) - 1L
txVec[bmiData$A2 == "MR"] <- 1L
bmiData$A2 <- as.factor(bmiData$A2)

txInfo <- DynTxRegime:::.newTxInfo(fSet = NULL, txName = "A2", data = bmiData, 
                                   suppress = TRUE, verify = TRUE)

obj <- DynTxRegime:::.newBOWLBasic(regime = regime,
                                   txInfo = txInfo,
                                   ind = !logical(nrow(bmiData)),
                                   prWgt = prWgt,
                                   response = y,
                                   txVec = txVec,
                                   data = bmiData,
                                   kernel = "linear",
                                   kparam = NULL,
                                   lambdas = 0.1,
                                   cvFolds = 0L,
                                   suppress = TRUE)

is(obj)
cvInfo(obj)
optimObj(obj)
DynTxRegime:::.predictOptimalTx(obj)
DynTxRegime:::.predictOptimalTx(obj,bmiData)
print(obj)
regimeCoef(obj)
show(obj)
summary(obj)


obj <- DynTxRegime:::.newBOWLBasic(regime = regime,
                                   txInfo = txInfo,
                                   ind = !logical(nrow(bmiData)),
                                   prWgt = prWgt,
                                   response = y,
                                   txVec = txVec,
                                   data = bmiData,
                                   kernel = "linear",
                                   kparam = NULL,
                                   lambdas = c(0.1,0.2,0.3),
                                   cvFolds = 4L,
                                   suppress = TRUE)

is(obj)
cvInfo(obj)
optimObj(obj)
DynTxRegime:::.predictOptimalTx(obj)
DynTxRegime:::.predictOptimalTx(obj,bmiData)
print(obj)
regimeCoef(obj)
show(obj)
summary(obj)

fSet <- function(data){
          subsets = list(list("subset1", c("CD","MR")),
                         list("subset2", c("CD")))
          txOpts <- character(nrow(data))
          txOpts[data$A1 == "CD"] <- "subset1"
          txOpts[data$A1 == "MR"] <- "subset2"

          return(list("subsets" = subsets, "txOpts" = txOpts))
        }

df <- bmiData
df$A2[df$A1 == "MR"] <- "CD"

txInfo <- DynTxRegime:::.newTxInfo(fSet = fSet, txName = "A2", data = df, 
                                   suppress = TRUE, verify = TRUE)

obj <- DynTxRegime:::.newBOWLBasic(regime = regime,
                                    txInfo = txInfo,
                                    ind = !logical(nrow(df)),
                                    prWgt = prWgt,
                                    response = y,
                                    txVec = txVec,
                                    data = df,
                                    kernel = "linear",
                                    kparam = NULL,
                                    lambdas = 0.1,
                                    cvFolds = 0L,
                                    suppress = TRUE)

is(obj)
cvInfo(obj)
optimObj(obj)
DynTxRegime:::.predictOptimalTx(obj)
DynTxRegime:::.predictOptimalTx(obj,df)
print(obj)
regimeCoef(obj)
show(obj)
summary(obj)


obj <- DynTxRegime:::.newBOWLBasic(regime = regime,
                                   txInfo = txInfo,
                                   ind = !logical(nrow(df)),
                                   prWgt = prWgt,
                                   response = y,
                                   txVec = txVec,
                                   data = df,
                                   kernel = "linear",
                                   kparam = NULL,
                                   lambdas = c(0.1,0.2,0.3),
                                   cvFolds = 4L,
                                   suppress = TRUE)

is(obj)
cvInfo(obj)
optimObj(obj)
DynTxRegime:::.predictOptimalTx(obj)
DynTxRegime:::.predictOptimalTx(obj,bmiData)
print(obj)
regimeCoef(obj)
show(obj)
summary(obj)
}

\keyword{internal}
