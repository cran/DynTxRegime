\name{IQLearnFS_VHet-class}
\Rdversion{1.1}
\docType{class}
\alias{IQLearnFS_VHet-class}

\title{Class \code{"IQLearnFS_VHet"}}
\description{
Results of the variance of the 
regression of the estimated contrast component of outcome of
interest in IQ-Learning.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("IQLearnFS_VHet", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Slots}{
  \describe{
    \item{\code{scale}:}{An object of class \code{"numeric."}
     Scaling factor for normalization of the residuals.}

    \item{\code{qFunc}:}{An object of class \code{"matrix."}
       The fitted values at both treatment options. }

    \item{\code{residuals}:}{An object of class \code{"numeric."}
      Residuals of the fit to the estimated contrast component. }
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
    \item{DTRstep}{\code{signature(object = "IQLearnFS_VHet")}: 
      Retrieve description of method implemented to obtain object. }

    \item{print}{\code{signature(x = "IQLearnFS_VHet")}: 
      Print key results of method. }

    \item{qqplot}{\code{signature(x = "IQLearnFS_VHet")}: 
      Generate qq-plot.}

    \item{residuals}{\code{signature(object = "IQLearnFS_VHet")}: 
      Retrieve the residual of the fitted second-stage contrast. }

    \item{show}{\code{signature(object = "IQLearnFS_VHet")}: 
      Show key results of method. }
	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}
\examples{
showClass("IQLearnFS_VHet")
}

\keyword{internal}

