\name{IQLearnFS_ME-class}
\docType{class}
\alias{IQLearnFS_ME-class}

\title{Class \code{"IQLearnFS_ME"}}
\description{
Results of the regression of the estimated main effects of the 
outcome of interest from IQ-Learning algorithm (IQ2).
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IQLearnFS_ME", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{

    \item{\code{qFunc}:}{An object of class \code{"matrix."}
       The fitted values at both treatment options. }

    \item{\code{yMainHatSS}:}{An object of class \code{"numeric."}
      The estimated main effects component of the outcome of interest.}
  }
}
\section{Extends}{
Classes  \code{"\link{IQLearnBase-class}"},
\code{"\link{IQLearnFS-class}"}, and
\code{"\link{DynTxRegime-class}"} directly.

Class \code{"\link{OutcomeOnly-class}"} by class \code{"\link{IQLearnBase-class}"}, distance 2.
}
\section{Methods}{
  \describe{

    \item{DTRstep}{\code{signature(object = "IQLearnFS_ME")}: 
      Retrieve description of method implemented to obtain object. }

    \item{print}{\code{signature(x = "IQLearnFS_ME")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "IQLearnFS_ME")}: 
      Show key results of method. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("IQLearnFS_ME")
}

\keyword{internal}

