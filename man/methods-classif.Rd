\name{classif}
\docType{methods}
\alias{classif}
\alias{classif,DynTxRegime-method}

\title{ Retrieve Classification Value Object}
\description{
For the classification based value-search method, this function retrieves the value object returned by the classification method. This method returns a NULL value for all DynTxRegime objects except those returned by optimalClass.
}
\section{Methods}{
\describe{

\item{\code{signature(object = "DynTxRegime")}}{
Retrieves the value object returned by the classification method.
The exact value and structure of the returned object is determined by
the method specified in \code{moClass}.

Because the value object of the modeling function is returned, 
any methods developed for said object can be applied to the
object returned. For example, if the
classification method is \code{rpart}, the object returned
by classif() can be passed as input to \code{residuals.rpart},
\code{path.rpart}, etc. without modification.

}

}}
\keyword{methods}
