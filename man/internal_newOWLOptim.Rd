\name{.newOWLOptim}
\docType{methods}
\alias{.newOWLOptim}
\alias{.newOWLOptim,matrix-method}

\title{ Optimization Routine - Outcome Weighted Learning.}

\description{
Optimizes OWL for given lambda and kparam. Method is not exported.
}

\usage{
.newOWLOptim(x, ...)
\S4method{.newOWLOptim}{matrix}(x, subset, lambda, txVec, prWgt, response, suppress, kernel, kparam)
}

\arguments{
 \item{x}{\code{"matrix"} of covariates for kernel.}
 \item{...}{Used to pass arguments that are required but
            the class of which does not determine the
            method that is selected.}
 \item{subset}{Vector of patients to include in training.}
 \item{lambda}{Tuning parameter.}
 \item{txVec}{Vector of treatments coded as +/-1.}
 \item{prWgt}{Vector of propensity valued according to tx received.}
 \item{response}{Outcome of interest.}
 \item{suppress}{T/F indicating if prints to screen are executed.}
 \item{kernel}{\code{"character"} description of kernel to be used.}
 \item{kparam}{\code{"numeric"} object, value of parameter in kernel.}
}

\author{
  Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{

data(bmiData)

y <- -(bmiData$month12BMI - bmiData$month4BMI) / bmiData$month4BMI * 100
y <- y - min(y)

regime <- ~ parentBMI + baselineBMI + gender
x <- model.matrix(regime, bmiData)

txVec <- numeric(nrow(bmiData)) - 1L
txVec[bmiData$A2 == "MR"] <- 1L
bmiData$A2 <- as.factor(bmiData$A2)

obj <- DynTxRegime:::.newOWLOptim(x = x,
                                  subset = 1L:nrow(bmiData),
                                  lambda = 0.1,
                                  txVec = txVec,
                                  prWgt = numeric(nrow(bmiData)) + 0.5,
                                  response = y,
                                  suppress = TRUE,
                                  kernel = 'linear',
                                  kparam = NULL)
is(obj)

}

\keyword{internal}
