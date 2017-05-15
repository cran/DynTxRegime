\name{.newBOWLOptimization}
\docType{methods}
\alias{.newBOWLOptimization}
\alias{.newBOWLOptimization,formula-method}
\alias{.newBOWLOptimization,list-method}

\title{ Backward Outcome Weighted Learning
With and Without Subsetting of Regime.}

\description{
Estimates the optimal treatment using outcome
weighted learning for a single decision point. Method is not
exported.
}

\usage{
.newBOWLOptimization(regime, ...)
\S4method{.newBOWLOptimization}{formula}(regime, txInfo, ind, prWgt, response, txVec, data, kernel, 
                     kparam, lambdas, cvFolds, suppress)
\S4method{.newBOWLOptimization}{list}(regime, txInfo, ind, prWgt, response, txVec, data, kernel, 
                     kparam, lambdas, cvFolds, suppress)
}

\arguments{
\item{regime }{\code{"formula"} description of decision rule.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
\item{txInfo }{Treatment information object.}
\item{ind    }{T/F indicating if optimal treatment has been followed
               beyond this decision point.}
\item{prWgt  }{Product of probabilities for treatment received beyond this
               decision point.}
\item{response}{Sum of rewards including this decision point.}
\item{txVec  }{Recast treatment vector as +/-1.}
\item{data   }{\code{"data.frame"} of covariates and treatment histories.}
\item{kernel }{\code{"character"} description of kernel function.}
\item{kparam }{NULL or \code{"numeric"} parameter for kernel function.}
\item{lambdas}{Tuning parameter(s).}
\item{cvFolds}{Number of cross-validation folds.}
\item{suppress}{T/F indicating if print to screens should be executed.}
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

obj <- DynTxRegime:::.newBOWLOptimization(regime = regime,
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


obj <- DynTxRegime:::.newBOWLOptimization(regime = regime,
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
                         list("subset2", c("CD","MR")))
          txOpts <- character(nrow(data))
          txOpts[data$A1 == "CD"] <- "subset1"
          txOpts[data$A1 == "MR"] <- "subset2"

          return(list("subsets" = subsets, "txOpts" = txOpts))
        }

txInfo <- DynTxRegime:::.newTxInfo(fSet = fSet, txName = "A2", data = bmiData, 
                                   suppress = TRUE, verify = TRUE)

obj <- DynTxRegime:::.newBOWLOptimization(regime = list("subset1"=regime,"subset2"=regime),
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

bmiData$A2[bmiData$A1 == "MR"] <- "CD"

fSet <- function(data){
          subsets = list(list("subset1", c("CD","MR")),
                         list("subset2", c("CD")))
          txOpts <- character(nrow(data))
          txOpts[data$A1 == "CD"] <- "subset1"
          txOpts[data$A1 == "MR"] <- "subset2"

          return(list("subsets" = subsets, "txOpts" = txOpts))
        }

prWgt[bmiData$A1 == "MR"] <- 1.0

txInfo <- DynTxRegime:::.newTxInfo(fSet = fSet, txName = "A2", data = bmiData, 
                                   suppress = TRUE, verify = TRUE)

obj <- DynTxRegime:::.newBOWLOptimization(regime = regime,
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

}

\keyword{internal}
