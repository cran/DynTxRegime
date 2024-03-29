% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/I_ClassificationObj.R
\docType{methods}
\name{.newClassificationObj}
\alias{.newClassificationObj}
\alias{.newClassificationObj,ANY-method}
\title{Create an Object of Class \code{ClassificationFitObj}}
\usage{
.newClassificationObj(moClass, txObj, ...)

\S4method{.newClassificationObj}{ANY}(moClass, txObj, data, response, suppress, ...)
}
\arguments{
\item{moClass}{modeling object(s) defining the classification regression}

\item{txObj}{TxObj defining the tx feasible sets}

\item{...}{additional arguments. Ignored.}

\item{data}{data.frame of covariates and tx received}

\item{suppress}{logical indicating user's screen printing preference}
}
\description{
Method calls .newClassificationFit() and stores the result in @classif.
}
\keyword{internal}
