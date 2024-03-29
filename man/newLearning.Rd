% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/O_Learning.R, R/O_LearningMulti.R
\name{.newLearning}
\alias{.newLearning}
\alias{.newLearning,NULL,Kernel-method}
\alias{.newLearning,function,Kernel-method}
\alias{.newLearning,function,SubsetList-method}
\title{Complete a Learning Analysis}
\usage{
.newLearning(fSet, kernel, ...)

\S4method{.newLearning}{`NULL`,Kernel}(
  fSet,
  kernel,
  ...,
  moPropen,
  moMain,
  moCont,
  data,
  response,
  txName,
  lambdas,
  cvFolds,
  iter,
  surrogate,
  suppress,
  guess,
  createObj,
  prodPi = 1,
  index = NULL
)

\S4method{.newLearning}{`function`,Kernel}(
  fSet,
  kernel,
  ...,
  moPropen,
  moMain,
  moCont,
  data,
  response,
  txName,
  lambdas,
  cvFolds,
  iter,
  surrogate,
  suppress,
  guess,
  createObj,
  prodPi = 1,
  index = NULL
)

\S4method{.newLearning}{`function`,SubsetList}(
  fSet,
  kernel,
  moPropen,
  moMain,
  moCont,
  data,
  response,
  txName,
  lambdas,
  cvFolds,
  iter,
  surrogate,
  suppress,
  guess,
  createObj,
  prodPi = 1,
  index = NULL,
  ...
)
}
\arguments{
\item{fSet}{NULL or function defining subset rules}

\item{kernel}{Kernel object or SubsetList}

\item{...}{Additional inputs for optimization}

\item{moPropen}{modelObj for propensity model}

\item{moMain}{modelObj for main effects of outcome model}

\item{moCont}{modelObj for contrasts of outcome model}

\item{data}{data.frame of covariates}

\item{response}{Vector of responses}

\item{txName}{Tx variable column header in data}

\item{lambdas}{Tuning parameter(s)}

\item{cvFolds}{Number of cross-validation folds}

\item{iter}{Maximum number of iterations for outcome regression}

\item{surrogate}{Surrogate object}

\item{suppress}{T/F indicating if prints to screen are executed}

\item{guess}{optional numeric vector providing starting values for
optimization methods}

\item{createObj}{A function name defining the method object for a
specific learning algorithm}

\item{prodPi}{A vector of propensity weights}

\item{index}{The subset of individuals to be included in learning}
}
\value{
A \code{Learning} object
}
\description{
Performs a weighted learning analysis.
}
\keyword{internal}
