\name{EARLAIPWE-class}
\docType{class}
\alias{EARLAIPWE-class}

\title{Class \code{"EARLAIPWE"}}
\description{
Efficient Augmentation and Relaxation Learning with 
Augmented Inverse Propensity Weighted Estimator
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("EARLAIPWE", ...)}.
These objects are for convenience in package development
and should not be created by users.
}

\section{Extends}{
Classes \code{"\link{EARL-class}"} and
\code{"\link{PropensityAndOutcome-class}"} directly.

Class \code{"\link{DynTxRegime-class}"} by class \code{"\link{EARL-class}"},
distance 2.
}

\section{Methods}{
  \describe{

    \item{DTRstep}{\code{signature(object = "EARLAIPWE")}: 
      Retrieve descripton of method. }

    \item{print}{\code{signature(x = "EARLAIPWE")}: 
      Print key results of method. }

    \item{show}{\code{signature(object = "EARLAIPWE")}: 
      Show key results of method. }

    \item{summary}{\code{signature(object = "EARLAIPWE")}: 
      Retrieve key summary information of method. }

   }
}

\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}

\examples{
showClass("EARLAIPWE")
}

\keyword{internal}

