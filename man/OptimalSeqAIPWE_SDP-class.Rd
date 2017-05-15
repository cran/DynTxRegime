\name{OptimalSeqAIPWE_SDP-class}
\Rdversion{1.1}
\docType{class}
\alias{OptimalSeqAIPWE_SDP-class}

\title{Class \code{"OptimalSeqAIPWE_SDP"}}
\description{
Implementation of single-decision-point missing data
perspective with AIPWE.
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("OptimalSeqAIPWE_SDP", ...)}.
These objects are for convenience in package development
and should not be created by users.
}
\section{Slots}{
  \describe{
    \item{\code{genetic}:}{An object of class \code{"list."} 
      Results returned by genetic algorithm. }

    \item{\code{regime}:}{An object of class \code{"Regime."}
      The regime class definition. }
  }
}
\section{Extends}{
Classes \code{"\link{OptimalSeq-class}"} and \code{"\link{PropensityAndOutcome-class}"}, directly.

Class \code{"\link{DynTxRegime-class}"} by class \code{"\link{OptimalSeq-class}"}, distance 2.
}
\section{Methods}{
  \describe{
    \item{DTRstep}{\code{signature(object = "OptimalSeqAIPWE_SDP")}: 
      Retrieve description of method used to create object. }
    \item{genetic}{\code{signature(object = "OptimalSeqAIPWE_SDP")}: 
      Retrieve the value object returned by rgenoud. }
    \item{optTx}{\code{signature(x = "OptimalSeqAIPWE_SDP", newdata = "data.frame")}: 
      Estimate optimal treatment for newdata. }
    \item{print}{\code{signature(x = "OptimalSeqAIPWE_SDP")}: 
      Print key results of method. }
    \item{regimeCoef}{\code{signature(x = "OptimalSeqAIPWE_SDP")}: 
      Retrieve regime parameter estimates. }
    \item{show}{\code{signature(object = "OptimalSeqAIPWE_SDP")}: 
      Show key results of method. }
    \item{summary}{\code{signature(object = "OptimalSeqAIPWE_SDP")}: 
      Retrieve key summary information of method. }

	 }
}
\author{
Shannon T. Holloway <sthollow@ncsu.edu>
}


\keyword{internal}
