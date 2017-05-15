\name{BOWLBasic_fSet-class}
\Rdversion{1.1}
\docType{class}
\alias{BOWLBasic_fSet-class}

\title{Class \code{"BOWLBasic_fSet"}}

\description{
Results for a single step of the BOWL algorithm for
a subset of data with binary treatment options and
subsets for propensity modeling specified. 
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("BOWLBasic_fSet", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{

    \item{\code{txIno}:}{Object of class \code{"TxSubset."}
                         Treatment information. }
  }
}

\section{Methods}{
  \describe{

    \item{.predictOptimalTx}{\code{signature(x = "BOWLBasic_fSet", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. Method is not exported.}
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("BOWLBasic_fSet")
}

\keyword{internal}
